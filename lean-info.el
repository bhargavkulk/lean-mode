;;; lean-info.el --- Plain Lean Info View -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; URL: https://github.com/bhargavkulk/lean-mode

;;; Commentary:
;; A widget-free Info View backed by Lean's `$/lean/plainGoal' LSP request.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)
(require 'lean-syntax)

(defvar lean-info--buffer nil
  "The shared Lean Info View buffer.")

(defvar lean-info--update-revision 0
  "Revision of the newest scheduled Info View update.

Each request captures this revision.  Its result is rendered only while it
still equals this value, so a cursor move or edit invalidates results for the
previous position before the next debounced request is sent.")

(defvar lean-info--displayed-source nil
  "Source buffer whose goals are currently displayed in the Info View.")

(defvar-local lean-info--update-timer nil
  "Idle timer used to debounce Info View updates.")

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

(defun lean-info--render (source revision result)
  "Render RESULT for SOURCE when it belongs to the current REVISION."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (and lean-info--active
                 (= revision lean-info--update-revision)
                 (buffer-live-p lean-info--buffer))
        (with-current-buffer lean-info--buffer
          (let ((inhibit-read-only t)
                (point (point))
                (window-starts
                 (mapcar (lambda (window)
                           (cons window (window-start window)))
                         (get-buffer-window-list lean-info--buffer nil t))))
            (erase-buffer)
            (insert (lean-info--goal-text result))
            (goto-char (min point (point-max)))
            (dolist (window-start window-starts)
              (when (window-live-p (car window-start))
                (set-window-start (car window-start)
                                  (min (cdr window-start) (point-max)) t)))
            (font-lock-flush)))
        (setq lean-info--displayed-source source)))))

(defun lean-info--clear (source revision)
  "Clear SOURCE's Info View when REVISION is still current."
  (lean-info--render source revision nil))

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

(defun lean-info--schedule-update ()
  "Schedule an Info View update for the current Lean buffer."
  (when (and lean-info--active
             (buffer-live-p lean-info--buffer))
    (when (timerp lean-info--update-timer)
      (cancel-timer lean-info--update-timer))
    (let ((revision (cl-incf lean-info--update-revision)))
      (setq lean-info--update-timer
            (run-with-idle-timer
             0.1 nil #'lean-info--request-update (current-buffer) revision)))))

(defun lean-info--cleanup ()
  "Tear down the Info View session owned by the current source buffer."
  (when (timerp lean-info--update-timer)
    (cancel-timer lean-info--update-timer))
  (setq lean-info--update-timer nil)
  (remove-hook 'post-command-hook #'lean-info--schedule-update t)
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
    (add-hook 'post-command-hook #'lean-info--schedule-update nil t)
    (add-hook 'kill-buffer-hook #'lean-info--cleanup nil t)
    (add-hook 'eglot-managed-mode-hook #'lean-info--eglot-shutdown-cleanup nil t)
    (lean-info--request-update source)))

(provide 'lean-info)
;;; lean-info.el ends here
