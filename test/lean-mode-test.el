;;; lean-mode-test.el --- Tests for lean-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'lean-mode)

(ert-deftest lean-mode-loads-and-registers-files ()
  (should (eq (cdr (assoc "\\.lean\\'" auto-mode-alist)) #'lean-mode))
  (should (file-readable-p
           (expand-file-name "abbreviations.json" lean-input-data-directory))))

(ert-deftest lean-mode-abbreviations-are-valid-json ()
  (let ((data (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "abbreviations.json" lean-input-data-directory))
                (json-parse-buffer))))
    (should (gethash "alpha" data))
    (should (equal (gethash "alpha" data) "α"))))

;;; lean-mode-test.el ends here
