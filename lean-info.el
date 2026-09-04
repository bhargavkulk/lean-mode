;;; lean-info.el --- Plain Lean Info View -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; URL: https://github.com/bhargavkulk/lean-mode

;;; Commentary:
;; A widget-free Info View backed by Lean's `$/lean/plainGoal' LSP request.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lean-syntax)

(defvar-local lean-info--buffer nil
  "The Info View buffer associated with the current Lean buffer.")

(defvar-local lean-info--request-generation 0
  "Generation number of the newest Info View request.")

(defvar-local lean-info--update-timer nil
  "Idle timer used to debounce Info View updates.")

(define-derived-mode lean-info-mode special-mode "Lean Info"
  "Major mode for displaying plain Lean goals."
  (setq-local font-lock-defaults lean-info-font-lock-defaults)
  (font-lock-mode 1))

(defun lean-info--buffer-name (source)
  "Return the Info View buffer name for SOURCE."
  (format "*Lean Info: %s*" (buffer-name source)))

(defun lean-info--goal-text (result)
  "Return plain goal text from a `$/lean/plainGoal' RESULT."
  (let ((goals (plist-get result :goals)))
    (if goals
        (if-let* ((goal-list (append goals nil)))
            (string-join goal-list "\n\n")
          "No Goal")
      (or (plist-get result :rendered) "No Goal"))))

(defun lean-info--render (source generation result)
  "Render RESULT for SOURCE when it belongs to GENERATION."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (and (= generation lean-info--request-generation)
                 (buffer-live-p lean-info--buffer))
        (with-current-buffer lean-info--buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (lean-info--goal-text result))
            (goto-char (point-min))
            (font-lock-flush)))))))

(defun lean-info--clear (source generation)
  "Clear SOURCE's Info View when GENERATION is still current."
  (lean-info--render source generation nil))

(defun lean-info--request-update (source)
  "Request the goal state at point in SOURCE.

The request is deliberately the plain goal endpoint, which avoids Lean's
interactive widget protocol."
  (when (buffer-live-p source)
    (with-current-buffer source
      (when (and eglot--managed-mode
                 (buffer-live-p lean-info--buffer))
        (let ((generation (cl-incf lean-info--request-generation))
              (server (eglot-current-server))
              (params (eglot--TextDocumentPositionParams)))
          (when server
            (eglot--signal-textDocument/didChange)
            (eglot--async-request
             server
             '$/lean/plainGoal
             params
             :hint 'lean-info--plain-goal
             :success-fn
             (lambda (result)
               (lean-info--render source generation result))
             :error-fn
             (lambda (&rest _error)
               (lean-info--clear source generation)))))))))

(defun lean-info--schedule-update ()
  "Schedule an Info View update for the current Lean buffer."
  (when (buffer-live-p lean-info--buffer)
    (when (timerp lean-info--update-timer)
      (cancel-timer lean-info--update-timer))
    (setq lean-info--update-timer
          (run-with-idle-timer
           0.1 nil #'lean-info--request-update (current-buffer)))))

(defun lean-info--cleanup ()
  "Stop updating this buffer's Info View."
  (when (timerp lean-info--update-timer)
    (cancel-timer lean-info--update-timer))
  (when (buffer-live-p lean-info--buffer)
    (kill-buffer lean-info--buffer)))

;;;###autoload
(defun lean-info-view ()
  "Display a plain, live Info View for the current Lean buffer."
  (interactive)
  (unless eglot--managed-mode
    (user-error "Lean's LSP server is not connected"))
  (let ((source (current-buffer)))
    (unless (buffer-live-p lean-info--buffer)
      (setq lean-info--buffer (get-buffer-create
                                (lean-info--buffer-name source)))
      (with-current-buffer lean-info--buffer
        (lean-info-mode)))
    (display-buffer-in-side-window
     lean-info--buffer '((side . right) (window-width . 0.33)))
    (add-hook 'post-command-hook #'lean-info--schedule-update nil t)
    (add-hook 'kill-buffer-hook #'lean-info--cleanup nil t)
    (lean-info--request-update source)))

(provide 'lean-info)
;;; lean-info.el ends here
