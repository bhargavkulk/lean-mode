;;; lean-info.el --- Plain Lean Info View -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; URL: https://github.com/bhargavkulk/lean-mode

;;; Commentary:
;; A widget-free Info View backed by Lean's `$/lean/plainGoal' LSP request.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'fringe)
(require 'jsonrpc)
(require 'seq)
(require 'subr-x)
(require 'lean-syntax)

(defvar lean-info--buffer nil
  "The shared Lean Info View buffer.")

(defvar lean-info--update-revision 0
  "Revision of the newest scheduled Info View update.

Each request captures this revision.  Its result is rendered only while it
still equals this value, so a cursor move or edit invalidates results for the
previous position before the next throttled request is sent.")

(defvar lean-info--displayed-source nil
  "Source buffer whose goals are currently displayed in the Info View.")

(defconst lean-info-update-cooldown 0.05
  "Seconds between Info View goal requests during continuous activity.")

(defvar-local lean-info--update-timer nil
  "Timer for the trailing Info View update in the current cooldown period.")

(defvar-local lean-info--last-request-time nil
  "Time at which the current source last requested Info View goals.")

(defvar-local lean-info--last-position nil
  "Position used by the most recently observed Info View update.")

(defvar-local lean-info--last-modified-tick nil
  "Modification tick used by the most recently observed Info View update.")

(defvar-local lean-info--processing-ranges :unknown
  "Ranges currently being elaborated by Lean in this source buffer.

The value is `:unknown' until Lean sends its first `$/lean/fileProgress'
notification.  An empty vector means the whole file is ready.")

(defface lean-info-processing-fringe
  '((t :foreground "orange"))
  "Face for Lean file-processing markers in the fringe."
  :group 'faces)

(define-fringe-bitmap 'lean-info-processing-bar
  [24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24]
  nil nil 'center)

(defvar-local lean-info--processing-marker-timer nil
  "Timer that coalesces fringe updates for the current source buffer.")

(defvar-local lean-info--processing-overlays nil
  "Fringe overlays marking lines currently processed by Lean.")

(defvar-local lean-info--diagnostics []
  "Latest diagnostics published by Lean for this source buffer.")

(defvar-local lean-info--last-goal-text nil
  "Most recently rendered goal or processing text for this source buffer.")

(defvar-local lean-info--active nil
  "Non-nil while this source buffer owns an active Info View session.")

(define-derived-mode lean-info-mode special-mode "Lean Info"
  "Major mode for displaying plain Lean goals."
  (setq-local cursor-type nil)
  (setq-local font-lock-defaults lean-info-font-lock-defaults)
  (font-lock-mode 1))

(defun lean-info--ensure-buffer ()
  "Return the shared Info View buffer, creating it when necessary."
  (unless (buffer-live-p lean-info--buffer)
    (setq lean-info--buffer (get-buffer-create "*Lean Info*"))
    (with-current-buffer lean-info--buffer
      (lean-info-mode)))
  lean-info--buffer)

(defun lean-info--goal-text (result)
  "Return plain goal text from a `$/lean/plainGoal' RESULT."
  (let ((goals (plist-get result :goals)))
    (if goals
        (if-let* ((goal-list (append goals nil)))
            (string-join goal-list "\n\n")
          "No Goal")
      (or (plist-get result :rendered) "No Goal"))))

(defun lean-info--diagnostic-text ()
  "Return Lean diagnostics whose range starts on the current line."
  (let ((line (1- (line-number-at-pos))))
    (mapconcat
     (lambda (diagnostic)
       (let* ((severity (pcase (plist-get diagnostic :severity)
                          (1 "error") (2 "warning")
                          (3 "information") (4 "hint")
                          (_ "error")))
              (message (string-remove-suffix "\n" (plist-get diagnostic :message))))
         (format "%s:\n%s" severity message)))
     (seq-filter
      (lambda (diagnostic)
        (= line (plist-get (plist-get (plist-get diagnostic :range) :start)
                            :line)))
      (append lean-info--diagnostics nil))
     "\n\n")))

(defun lean-info--contents (goal-text)
  "Append the current line's Lean diagnostics to GOAL-TEXT."
  (let ((diagnostics (lean-info--diagnostic-text)))
    (if (string-empty-p diagnostics)
        goal-text
      (concat goal-text "\n\n" diagnostics))))

(defun lean-info--render (source revision result)
  "Render RESULT for SOURCE when it belongs to the current REVISION."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (and lean-info--active
                 (= revision lean-info--update-revision)
                 (buffer-live-p lean-info--buffer))
        (let ((goal-text (lean-info--goal-text result)))
          (setq lean-info--last-goal-text goal-text)
          (let ((contents (lean-info--contents goal-text)))
            (with-current-buffer lean-info--buffer
              (unless (equal (buffer-string) contents)
                (let ((inhibit-read-only t)
                      (point (point))
                      (window-starts
                       (mapcar (lambda (window)
                                 (cons window (window-start window)))
                               (get-buffer-window-list lean-info--buffer nil t))))
                  (erase-buffer)
                  (insert contents)
                  (goto-char (min point (point-max)))
                  (dolist (window-start window-starts)
                    (when (window-live-p (car window-start))
                      (set-window-start (car window-start)
                                        (min (cdr window-start) (point-max)) t)))
                  (font-lock-flush))))))
        (setq lean-info--displayed-source source)))))

(defun lean-info--clear (source revision)
  "Clear SOURCE's Info View when REVISION is still current."
  (lean-info--render source revision nil))

(defun lean-info--processing-at-point-p ()
  "Return non-nil when Lean is still elaborating point in this buffer."
  (or (eq lean-info--processing-ranges :unknown)
      (let ((line (1- (line-number-at-pos))))
        (seq-some
         (lambda (processing)
           (let* ((range (plist-get processing :range))
                  (start (plist-get (plist-get range :start) :line))
                  (end (plist-get (plist-get range :end) :line)))
             (and start end (<= start line end))))
         (append lean-info--processing-ranges nil)))))

(defun lean-info--show-processing ()
  "Render a processing indicator and invalidate any pending goal request."
  (let ((revision (cl-incf lean-info--update-revision)))
    (lean-info--render (current-buffer) revision '(:goals ["Processing file..."]))))

(defun lean-info--clear-processing-markers ()
  "Remove all Lean processing markers from the current source buffer."
  (mapc #'delete-overlay lean-info--processing-overlays)
  (setq lean-info--processing-overlays nil))

(defun lean-info--processing-line-ranges (last-line)
  "Return merged processing line ranges, bounded by LAST-LINE.

Each returned element is a cons cell whose car and cdr are the inclusive
first and final line numbers."
  (let (ranges merged-ranges)
    (dolist (processing (append lean-info--processing-ranges nil))
      (let* ((range (plist-get processing :range))
             (start (plist-get (plist-get range :start) :line))
             (end (plist-get (plist-get range :end) :line)))
        (when (and (integerp start) (integerp end))
          (let ((first-line (max 0 start))
                (final-line (min end last-line)))
            (when (<= first-line final-line)
              (push (cons first-line final-line) ranges))))))
    (dolist (range (sort ranges (lambda (left right) (< (car left) (car right)))))
      (if (and merged-ranges
               (<= (car range) (1+ (cdr (car merged-ranges)))))
          (setcdr (car merged-ranges)
                  (max (cdr (car merged-ranges)) (cdr range)))
        (push range merged-ranges)))
    (nreverse merged-ranges)))

(defun lean-info--refresh-processing-markers (source)
  "Render SOURCE's current Lean processing ranges in its left fringe."
  (when (buffer-live-p source)
    (with-current-buffer source
      (setq lean-info--processing-marker-timer nil)
      (lean-info--clear-processing-markers)
      (unless (eq lean-info--processing-ranges :unknown)
        (save-excursion
          (save-restriction
            (widen)
            (let ((last-line (1- (line-number-at-pos (point-max)))))
              (goto-char (point-min))
              (let ((current-line 0))
                (dolist (range (lean-info--processing-line-ranges last-line))
                  (forward-line (- (car range) current-line))
                  (setq current-line (car range))
                  (cl-loop repeat (1+ (- (cdr range) (car range)))
                           do (let ((overlay (make-overlay (point) (point))))
                                (overlay-put overlay 'before-string
                                             (propertize "!"
                                                         'display
                                                         '(left-fringe
                                                           lean-info-processing-bar
                                                           lean-info-processing-fringe)))
                                (push overlay lean-info--processing-overlays))
                           do (forward-line 1)
                           do (cl-incf current-line)))))))))))

(defun lean-info--schedule-processing-marker-refresh ()
  "Coalesce a fringe refresh for the current source's processing ranges."
  (unless (timerp lean-info--processing-marker-timer)
    (setq lean-info--processing-marker-timer
          (run-at-time 0.1 nil #'lean-info--refresh-processing-markers
                       (current-buffer)))))

(defun lean-info-handle-file-progress (uri processing)
  "Update the Info View for URI after Lean reports PROCESSING ranges.

PROCESSING is the range list from Lean's `$/lean/fileProgress' notification.
While point is within a reported range, show a processing indicator instead of
asking Lean for a goal that is not ready yet."
  (when-let* ((source (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer source
      (let ((was-processing (lean-info--processing-at-point-p)))
        (setq lean-info--processing-ranges processing)
        (lean-info--schedule-processing-marker-refresh)
        (when (and lean-info--active
                   (eq source lean-info--displayed-source))
          (if (lean-info--processing-at-point-p)
              (lean-info--show-processing)
            (when was-processing
              (lean-info--schedule-update))))))))

(defun lean-info-handle-diagnostics (uri diagnostics)
  "Refresh URI's Info View with Lean DIAGNOSTICS for the current line.

Keep the current Info View request revision: Lean's subsequent
`$/lean/fileProgress' notification is responsible for replacing the
processing indicator with its final goal state."
  (when-let* ((source (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer source
      (setq lean-info--diagnostics diagnostics)
      (when (and lean-info--active
                 (eq source lean-info--displayed-source)
                 lean-info--last-goal-text)
        (lean-info--render source lean-info--update-revision
                           `(:rendered ,lean-info--last-goal-text))))))

(defun lean-info--async-request (server method params success-fn error-fn)
  "Request METHOD from SERVER without blocking Emacs.

Use Eglot's request helper when available, otherwise use the compatible
JSON-RPC API bundled with older supported Emacs releases."
  (if (fboundp 'eglot--async-request)
      (eglot--async-request server method params
                            :hint 'lean-info--plain-goal
                            :success-fn success-fn
                            :error-fn error-fn)
    (jsonrpc-async-request server method params
                           :success-fn success-fn
                           :error-fn error-fn)))

(defun lean-info--request-update (source &optional revision)
  "Request the goal state at point in SOURCE for REVISION.

The request is deliberately the plain goal endpoint, which avoids Lean's
interactive widget protocol."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (and lean-info--active
                 eglot--managed-mode
                 (buffer-live-p lean-info--buffer))
        (let ((revision (or revision
                            (cl-incf lean-info--update-revision)))
              (server (eglot-current-server))
              (params (eglot--TextDocumentPositionParams)))
          (when server
            (eglot--signal-textDocument/didChange)
            (lean-info--async-request
             server
             '$/lean/plainGoal
             params
             (lambda (result)
               (lean-info--render source revision result))
             (lambda (&rest _error)
               (lean-info--clear source revision)))))))))

(defun lean-info--run-scheduled-update (source revision)
  "Request SOURCE's goals for REVISION after a cooldown period."
  (when (buffer-live-p source)
    (with-current-buffer source
      (setq lean-info--update-timer nil)
      (when (and lean-info--active
                 (= revision lean-info--update-revision))
        (setq lean-info--last-request-time (float-time))
        (lean-info--request-update source revision)))))

(defun lean-info--schedule-update ()
  "Request current goals now, or once at the end of the cooldown period."
  (when (and lean-info--active
             (buffer-live-p lean-info--buffer))
    (let* ((now (float-time))
           (revision (cl-incf lean-info--update-revision))
           (elapsed (and lean-info--last-request-time
                         (- now lean-info--last-request-time))))
      (if (or (null elapsed) (>= elapsed lean-info-update-cooldown))
          (progn
            (when (timerp lean-info--update-timer)
              (cancel-timer lean-info--update-timer))
            (setq lean-info--update-timer nil
                  lean-info--last-request-time now)
            (lean-info--request-update (current-buffer) revision))
        (when (timerp lean-info--update-timer)
          (cancel-timer lean-info--update-timer))
        (setq lean-info--update-timer
              (run-at-time (- lean-info-update-cooldown elapsed) nil
                           #'lean-info--run-scheduled-update
                           (current-buffer) revision))))))

(defun lean-info--update-if-needed ()
  "Schedule an update after the point, source text, or displayed source changes."
  (let ((position (point))
        (modified-tick (buffer-chars-modified-tick))
        (source-changed (not (eq (current-buffer) lean-info--displayed-source))))
    (unless (and (not source-changed)
                 (equal position lean-info--last-position)
                 (= modified-tick lean-info--last-modified-tick))
      (setq lean-info--last-position position
            lean-info--last-modified-tick modified-tick
            lean-info--displayed-source (current-buffer))
      (if (lean-info--processing-at-point-p)
          (lean-info--show-processing)
        (lean-info--schedule-update)))))

(defun lean-info--cleanup ()
  "Tear down the Info View session owned by the current source buffer."
  (when (timerp lean-info--update-timer)
    (cancel-timer lean-info--update-timer))
  (when (timerp lean-info--processing-marker-timer)
    (cancel-timer lean-info--processing-marker-timer))
  (lean-info--clear-processing-markers)
  (setq lean-info--update-timer nil
        lean-info--processing-marker-timer nil
        lean-info--last-request-time nil
        lean-info--last-position nil
        lean-info--last-modified-tick nil
        lean-info--processing-ranges :unknown
        lean-info--diagnostics []
        lean-info--last-goal-text nil)
  (remove-hook 'post-command-hook #'lean-info--update-if-needed t)
  (remove-hook 'kill-buffer-hook #'lean-info--cleanup t)
  (remove-hook 'eglot-managed-mode-hook #'lean-info--eglot-shutdown-cleanup t)
  (setq lean-info--active nil)
  (when (eq (current-buffer) lean-info--displayed-source)
    (setq lean-info--displayed-source nil)
    (when (buffer-live-p lean-info--buffer)
      (with-current-buffer lean-info--buffer
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (quit-windows-on lean-info--buffer))))

(defun lean-info--eglot-shutdown-cleanup ()
  "Clean up the current source's Info View session after Eglot stops managing it."
  (unless (eglot-managed-p)
    (lean-info--cleanup)))

(defun lean-info-auto-open ()
  "Open the Info View when Eglot begins managing a Lean buffer."
  (when (and (derived-mode-p 'lean-mode)
             (eglot-managed-p))
    (lean-info-view)))

;;;###autoload
(defun lean-info-view ()
  "Display a plain, live Info View for the current Lean buffer."
  (interactive)
  (unless eglot--managed-mode
    (user-error "Lean's LSP server is not connected"))
  (let ((source (current-buffer)))
    (lean-info--ensure-buffer)
    (display-buffer-in-side-window
     lean-info--buffer '((side . right) (window-width . 0.33)))
    (setq-local lean-info--active t)
    (setq-local lean-info--last-position (point))
    (setq-local lean-info--last-modified-tick (buffer-chars-modified-tick))
    (setq-local lean-info--last-request-time (float-time))
    (add-hook 'post-command-hook #'lean-info--update-if-needed nil t)
    (add-hook 'kill-buffer-hook #'lean-info--cleanup nil t)
    (add-hook 'eglot-managed-mode-hook #'lean-info--eglot-shutdown-cleanup nil t)
    (if (lean-info--processing-at-point-p)
        (lean-info--show-processing)
      (lean-info--request-update source))))

(provide 'lean-info)
;;; lean-info.el ends here
