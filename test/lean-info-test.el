;;; lean-info-test.el --- Tests for Lean Info View -*- lexical-binding: t; -*-

(require 'ert)
(require 'lean-info)
(require 'lean-mode)

(defmacro lean-info-test-with-source (&rest body)
  "Evaluate BODY in a temporary Lean source buffer."
  (declare (indent 0))
  `(let ((lean-info--buffer (generate-new-buffer " *lean-info-test*"))
         (lean-info--update-revision 0)
         (lean-info--displayed-source nil)
         (file (make-temp-file "lean-info-test-" nil ".lean")))
     (with-temp-buffer
       (insert "example : True := by\n  trivial\n")
       (set-visited-file-name file t)
       (setq-local lean-info--active t)
       (setq-local lean-info--processing-ranges [])
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p lean-info--buffer)
           (kill-buffer lean-info--buffer))
         (delete-file file)))))

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

(ert-deftest lean-info-does-not-redraw-unchanged-goals ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode)
      (let ((inhibit-read-only t))
        (insert "⊢ True")))
    (setq lean-info--update-revision 1)
    (let (erased)
      (cl-letf (((symbol-function 'erase-buffer)
                 (lambda () (setq erased t))))
        (lean-info--render (current-buffer) 1 '(:goals ["⊢ True"])))
      (should-not erased))))

(ert-deftest lean-info-render-preserves-info-view-scroll-position ()
  (lean-info-test-with-source
    (let* ((window (selected-window))
           (original-buffer (window-buffer window))
           (goals (mapconcat (lambda (number) (format "goal %d" number))
                             (number-sequence 1 100) "\n")))
      (unwind-protect
          (progn
            (set-window-buffer window lean-info--buffer)
            (with-current-buffer lean-info--buffer
              (lean-info-mode)
              (let ((inhibit-read-only t))
                (insert goals)
                (goto-char (point-min))
                (forward-line 50)
                (set-window-start window (point) t)))
            (let ((point (with-current-buffer lean-info--buffer (point)))
                  (window-start (window-start window)))
              (setq lean-info--update-revision 1)
              (lean-info--render (current-buffer) 1 `(:goals [,goals]))
              (with-current-buffer lean-info--buffer
                (should (= (point) point)))
              (should (= (window-start window) window-start))))
        (set-window-buffer window original-buffer)))))

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
    (let (requested)
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _value) 10.0))
                ((symbol-function 'lean-info--request-update)
                 (lambda (source revision)
                   (setq requested (list source revision)))))
        (lean-info--schedule-update))
      (should (= lean-info--update-revision 2))
      (should (equal requested (list (current-buffer) 2))))
    (lean-info--render (current-buffer) 1 '(:rendered "stale"))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "current")))))

(ert-deftest lean-info-schedules-only-after-a-point-or-text-change ()
  (lean-info-test-with-source
    (setq-local lean-info--last-position (point))
    (setq-local lean-info--last-modified-tick (buffer-chars-modified-tick))
    (setq lean-info--displayed-source (current-buffer))
    (let (scheduled)
      (cl-letf (((symbol-function 'lean-info--schedule-update)
                 (lambda () (setq scheduled (1+ (or scheduled 0))))))
        (lean-info--update-if-needed)
        (goto-char (point-min))
        (forward-char 1)
        (lean-info--update-if-needed)
        (insert " ")
        (lean-info--update-if-needed))
      (should (= scheduled 2)))))

(ert-deftest lean-info-refreshes-when-returning-to-a-source-buffer ()
  (lean-info-test-with-source
    (let ((first-source (current-buffer))
          (second-source (generate-new-buffer " *lean-info-second-source*")))
      (unwind-protect
          (progn
            (setq-local lean-info--last-position (point))
            (setq-local lean-info--last-modified-tick (buffer-chars-modified-tick))
            (setq lean-info--displayed-source second-source)
            (let (scheduled)
              (cl-letf (((symbol-function 'lean-info--schedule-update)
                         (lambda () (setq scheduled t))))
                (lean-info--update-if-needed))
              (should scheduled)
              (should (eq lean-info--displayed-source first-source))))
        (when (buffer-live-p second-source)
          (kill-buffer second-source))))))

(ert-deftest lean-info-throttles-updates-with-a-trailing-request ()
  (lean-info-test-with-source
    (let (requests scheduled-function scheduled-arguments)
      (cl-letf (((symbol-function 'float-time)
                 (let ((time 10.0))
                   (lambda (&optional _value) time)))
                ((symbol-function 'lean-info--request-update)
                 (lambda (source revision)
                   (push (list source revision) requests)))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (setq scheduled-function function
                         scheduled-arguments arguments)
                   'timer)))
        (lean-info--schedule-update)
        (lean-info--schedule-update))
      (should (equal (nreverse requests) (list (list (current-buffer) 1))))
      (should (eq scheduled-function #'lean-info--run-scheduled-update))
      (should (equal scheduled-arguments (list (current-buffer) 2))))))

(ert-deftest lean-info-shows-processing-until-its-point-is-ready ()
  (lean-info-test-with-source
    (setq-local lean-info--processing-ranges :unknown)
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (lean-info--show-processing)
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "Processing file...")))
    (let (scheduled)
      (cl-letf (((symbol-function 'lean-info--schedule-update)
                 (lambda () (setq scheduled t))))
        (lean-info-handle-file-progress
         (eglot-path-to-uri buffer-file-name)
         []))
      (should scheduled))))

(ert-deftest lean-info-keeps-processing-for-a-reported-range ()
  (lean-info-test-with-source
    (setq-local lean-info--processing-ranges [])
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--displayed-source (current-buffer))
    (let (requested)
      (cl-letf (((symbol-function 'lean-info--request-update)
                 (lambda (&rest _) (setq requested t))))
        (lean-info-handle-file-progress
         (eglot-path-to-uri buffer-file-name)
         [(:range (:start (:line 0) :end (:line 10)))]))
      (should-not requested))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string) "Processing file...")))))

(ert-deftest lean-info-renders-processing-ranges-in-the-left-fringe ()
  (lean-info-test-with-source
    (setq-local lean-info--processing-ranges
                [(:range (:start (:line 0) :end (:line 1)))])
    (unwind-protect
        (progn
          (lean-info--refresh-processing-markers (current-buffer))
          (should (= (length lean-info--processing-overlays) 2))
          (dolist (overlay lean-info--processing-overlays)
            (should (equal (get-text-property
                            0 'display (overlay-get overlay 'before-string))
                           '(left-fringe lean-info-processing-bar
                             lean-info-processing-fringe)))))
      (lean-info--clear-processing-markers))))

(ert-deftest lean-info-merges-processing-ranges-before-rendering-markers ()
  (lean-info-test-with-source
    (setq-local lean-info--processing-ranges
                [(:range (:start (:line 1) :end (:line 2)))
                 (:range (:start (:line 0) :end (:line 1)))])
    (unwind-protect
        (progn
          (lean-info--refresh-processing-markers (current-buffer))
          (should (= (length lean-info--processing-overlays) 3)))
      (lean-info--clear-processing-markers))))

(ert-deftest lean-info-file-progress-does-not-replace-another-source-view ()
  (lean-info-test-with-source
    (let ((first-source (current-buffer))
          (second-source (generate-new-buffer " *lean-info-second-source*"))
          (second-file (make-temp-file "lean-info-second-" nil ".lean")))
      (unwind-protect
          (progn
            (with-current-buffer lean-info--buffer
              (lean-info-mode))
            (setq lean-info--update-revision 1)
            (lean-info--render first-source 1 '(:goals ["first goal"]))
            (with-current-buffer second-source
              (insert "example : True := by\n  trivial\n")
              (set-visited-file-name second-file t)
              (goto-char (point-min))
              (setq-local lean-info--active t)
              (setq-local lean-info--processing-ranges []))
            (lean-info-handle-file-progress
             (eglot-path-to-uri second-file)
             [(:range (:start (:line 0) :end (:line 1)))])
            (should (eq lean-info--displayed-source first-source))
            (should (= lean-info--update-revision 1))
            (with-current-buffer lean-info--buffer
              (should (equal (buffer-string) "first goal"))))
        (when (buffer-live-p second-source)
          (kill-buffer second-source))
        (delete-file second-file)))))

(ert-deftest lean-info-renders-current-line-lean-diagnostics ()
  (lean-info-test-with-source
    (goto-char (point-min))
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--update-revision 1)
    (lean-info--render
     (current-buffer) 1
     '(:goals ["⊢ True"]))
    (lean-info-handle-diagnostics
     (eglot-path-to-uri buffer-file-name)
     [(:severity 1 :message "tactic 'exact' failed\n"
       :range (:start (:line 0) :end (:line 0)))
      (:severity 2 :message "other line"
       :range (:start (:line 1) :end (:line 1)))])
    (should (= lean-info--update-revision 1))
    (with-current-buffer lean-info--buffer
      (should (equal (buffer-string)
                     "⊢ True\n\nerror:\ntactic 'exact' failed")))))

(ert-deftest lean-info-cleanup-clears-owned-view-and-ignores-late-response ()
  (lean-info-test-with-source
    (with-current-buffer lean-info--buffer
      (lean-info-mode))
    (setq lean-info--update-revision 1)
    (lean-info--render (current-buffer) 1 '(:goals ["⊢ True"]))
    (setq-local lean-info--update-timer (run-at-time 60 nil #'ignore))
    (add-hook 'post-command-hook #'lean-info--update-if-needed nil t)
    (add-hook 'kill-buffer-hook #'lean-info--cleanup nil t)
    (add-hook 'eglot-managed-mode-hook #'lean-info--eglot-shutdown-cleanup nil t)
    (let (view-hidden)
      (cl-letf (((symbol-function 'quit-windows-on)
                 (lambda (buffer) (setq view-hidden buffer))))
        (lean-info--cleanup))
      (should-not lean-info--active)
      (should-not lean-info--update-timer)
      (should-not (memq #'lean-info--update-if-needed post-command-hook))
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
