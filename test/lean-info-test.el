;;; lean-info-test.el --- Tests for Lean Info View -*- lexical-binding: t; -*-

(require 'ert)
(require 'lean-info)
(require 'lean-mode)

(defmacro lean-info-test-with-source (&rest body)
  "Evaluate BODY in a temporary Lean source buffer."
  (declare (indent 0))
  `(let ((lean-info--buffer (generate-new-buffer " *lean-info-test*"))
         (lean-info--update-revision 0)
         (lean-info--displayed-source nil))
     (with-temp-buffer
       (insert "example : True := by\n  trivial\n")
       (setq-local lean-info--active t)
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p lean-info--buffer)
           (kill-buffer lean-info--buffer))))))

(ert-deftest lean-info-renders-plain-goal-response ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--update-revision 1)
    (lean-info--render (current-buffer) 1 '(:rendered "```lean\n⊢ True\n```"
                                             :goals ["⊢ True"]))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "⊢ True")))))

(ert-deftest lean-info-renders-no-goal ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--update-revision 1)
    (lean-info--render (current-buffer) 1 '(:rendered "no goals" :goals []))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "No Goal")))))

(ert-deftest lean-info-ignores-stale-responses ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (let ((inhibit-read-only t))
        (lean-info-mode)
        (insert "current")))
    (setq lean-info--update-revision 2)
    (lean-info--render (current-buffer) 1 '(:rendered "stale"))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "current")))))

(ert-deftest lean-info-scheduling-invalidates-an-in-flight-response ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (let ((inhibit-read-only t))
        (lean-info-mode)
        (insert "current")))
    (setq lean-info--update-revision 1)
    (let (scheduled-function scheduled-arguments)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat function &rest arguments)
                   (setq scheduled-function function
                         scheduled-arguments arguments)
                   nil)))
        (lean-info--schedule-update))
      (should (= lean-info--update-revision 2))
      (should (eq scheduled-function #'lean-info--request-update))
      (should (equal scheduled-arguments (list (current-buffer) 2))))
    (lean-info--render (current-buffer) 1 '(:rendered "stale"))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "current")))))

(ert-deftest lean-info-cleanup-clears-owned-view-and-ignores-late-response ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--update-revision 1)
    (lean-info--render (current-buffer) 1 '(:goals ["⊢ True"]))
    (setq-local lean-info--update-timer (run-at-time 60 nil #'ignore))
    (add-hook 'post-command-hook #'lean-info--schedule-update nil t)
    (add-hook 'kill-buffer-hook #'lean-info--cleanup nil t)
    (add-hook 'eglot-managed-mode-hook #'lean-info--eglot-shutdown-cleanup nil t)
    (let (view-hidden)
      (cl-letf (((symbol-function 'quit-windows-on)
                 (lambda (buffer) (setq view-hidden buffer))))
        (lean-info--cleanup))
      (should-not lean-info--active)
      (should-not lean-info--update-timer)
      (should-not (memq #'lean-info--schedule-update post-command-hook))
      (should-not (memq #'lean-info--cleanup kill-buffer-hook))
      (should-not (memq #'lean-info--eglot-shutdown-cleanup
                        eglot-managed-mode-hook))
      (should-not lean-info--displayed-source)
      (should (eq view-hidden lean-info--buffer)))
    (lean-info--render (current-buffer) 1 '(:goals ["late goal"]))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "")))))

(ert-deftest lean-info-cleanup-preserves-another-source-view ()
  (lean-info-test-with-source
    (let ((first-source (current-buffer))
          (second-source (generate-new-buffer " *lean-info-second-source*")))
      (unwind-protect
          (progn
            (with-current-buffer lean-info--buffer
              (lean-info-mode))
            (setq lean-info--update-revision 1)
            (lean-info--render first-source 1 '(:goals ["first goal"]))
            (with-current-buffer second-source
              (setq-local lean-info--active t))
            (setq lean-info--update-revision 2)
            (lean-info--render second-source 2 '(:goals ["second goal"]))
            (with-current-buffer first-source
              (lean-info--cleanup))
            (should (eq lean-info--displayed-source second-source))
            (with-current-buffer lean-info--buffer
              (should (equal (buffer-string) "second goal"))))
        (when (buffer-live-p second-source)
          (kill-buffer second-source))))))

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

(ert-deftest lean-info-uses-jsonrpc-fallback-without-eglot-async-request ()
  (let (server method params success-fn error-fn)
    (cl-letf (((symbol-function 'eglot--async-request) nil)
              ((symbol-function 'jsonrpc-async-request)
               (lambda (request-server request-method request-params &rest args)
                 (setq server request-server
                       method request-method
                       params request-params
                       success-fn (plist-get args :success-fn)
                       error-fn (plist-get args :error-fn)))))
      (lean-info--async-request 'server '$/lean/plainGoal '(:position nil)
                                #'ignore #'ignore))
    (should (eq server 'server))
    (should (eq method '$/lean/plainGoal))
    (should (equal params '(:position nil)))
    (should (eq success-fn #'ignore))
    (should (eq error-fn #'ignore))))

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
