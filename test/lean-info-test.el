;;; lean-info-test.el --- Tests for Lean Info View -*- lexical-binding: t; -*-

(require 'ert)
(require 'lean-info)
(require 'lean-mode)

(defmacro lean-info-test-with-source (&rest body)
  "Evaluate BODY in a temporary Lean source buffer."
  (declare (indent 0))
  `(let ((lean-info--buffer (generate-new-buffer " *lean-info-test*"))
         (lean-info--request-generation 0))
     (with-temp-buffer
       (insert "example : True := by\n  trivial\n")
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p lean-info--buffer)
           (kill-buffer lean-info--buffer))))))

(ert-deftest lean-info-renders-plain-goal-response ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--request-generation 1)
    (lean-info--render (current-buffer) 1 '(:rendered "```lean\n⊢ True\n```"
                                             :goals ["⊢ True"]))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "⊢ True")))))

(ert-deftest lean-info-renders-no-goal ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--request-generation 1)
    (lean-info--render (current-buffer) 1 '(:rendered "no goals" :goals []))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "No Goal")))))

(ert-deftest lean-info-ignores-stale-responses ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (let ((inhibit-read-only t))
        (lean-info-mode)
        (insert "current")))
    (setq lean-info--request-generation 2)
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

(ert-deftest lean-info-reuses-one-buffer ()
  (let ((lean-info--buffer nil))
    (unwind-protect
        (let ((first (lean-info--ensure-buffer))
              second)
          (setq second (lean-info--ensure-buffer))
          (should (eq first second))
          (should (equal (buffer-name first) "*Lean Info*"))
          (with-current-buffer first
            (should-not cursor-type)))
      (when (buffer-live-p lean-info--buffer)
        (kill-buffer lean-info--buffer)))))

(ert-deftest lean-info-auto-opens-when-eglot-starts ()
  (with-temp-buffer
    (lean-mode)
    (let (opened)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'lean-info-view)
                 (lambda () (setq opened t))))
        (run-hooks 'eglot-managed-mode-hook))
      (should opened))))

(ert-deftest lean-info-does-not-auto-open-when-eglot-stops ()
  (with-temp-buffer
    (lean-mode)
    (let (opened)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
                ((symbol-function 'lean-info-view)
                 (lambda () (setq opened t))))
        (run-hooks 'eglot-managed-mode-hook))
      (should-not opened))))

;;; lean-info-test.el ends here
