;;; lean-info-test.el --- Tests for Lean Info View -*- lexical-binding: t; -*-

(require 'ert)
(require 'lean-info)

(defmacro lean-info-test-with-source (&rest body)
  "Evaluate BODY in a temporary Lean source buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "example : True := by\n  trivial\n")
     (setq-local lean-info--buffer (generate-new-buffer " *lean-info-test*"))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p lean-info--buffer)
         (kill-buffer lean-info--buffer)))))

(ert-deftest lean-info-renders-plain-goal-response ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq-local lean-info--request-generation 1)
    (lean-info--render (current-buffer) 1 '(:rendered "⊢ True"))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "⊢ True")))))

(ert-deftest lean-info-ignores-stale-responses ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (let ((inhibit-read-only t))
        (lean-info-mode)
        (insert "current")))
    (setq-local lean-info--request-generation 2)
    (lean-info--render (current-buffer) 1 '(:rendered "stale"))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "current")))))

(ert-deftest lean-info-requests-plain-goals-at-point ()
  (lean-info-test-with-source
    (let (method params)
      (setq-local eglot--managed-mode t)
      (cl-letf (((symbol-function 'eglot-current-server) (lambda () 'server))
                ((symbol-function 'eglot--signal-textDocument/didChange)
                 (lambda ()))
                ((symbol-function 'eglot--TextDocumentPositionParams)
                 (lambda () '(:position (:line 1 :character 2))))
                ((symbol-function 'eglot--async-request)
                 (lambda (_server request-method request-params &rest _args)
                   (setq method request-method
                         params request-params))))
        (lean-info--request-update (current-buffer)))
      (should (eq method '$/lean/plainGoal))
      (should (equal params '(:position (:line 1 :character 2)))))))

;;; lean-info-test.el ends here
