;;; lean-syntax-test.el --- Tests for lean syntax highlighting -*- lexical-binding: t; -*-

(require 'ert)
(require 'font-lock)
(require 'lean-syntax)

(defmacro lean-syntax-test-with-buffer (contents &rest body)
  "Evaluate BODY in a temporary buffer containing CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (set-syntax-table lean-syntax-table)
     ,@body))

(defun lean-syntax-test-fontify ()
  "Fontify the current buffer using Lean's syntax rules."
  (setq-local font-lock-defaults lean-font-lock-defaults)
  (font-lock-mode 1)
  (font-lock-ensure))

(ert-deftest lean-font-lock-highlights-multiline-attribute ()
  (lean-syntax-test-with-buffer "@[foo\n  bar]\ndef x := 1"
    (goto-char (point-min))
    (should (lean-font-lock-match-attribute (point-max)))
    (should (equal (match-string 0) "@[foo\n  bar]"))))

(ert-deftest lean-font-lock-highlights-attribute-command ()
  (lean-syntax-test-with-buffer "attribute [simp\n  inline] theoremName"
    (goto-char (point-min))
    (should (lean-font-lock-match-attribute (point-max)))
    (should (equal (match-string 0) "attribute [simp\n  inline]"))))

(ert-deftest lean-font-lock-highlights-triple-quoted-string ()
  (lean-syntax-test-with-buffer "def doc := \"\"\"first\nsecond\"\"\""
    (goto-char (point-min))
    (should (lean-font-lock-match-string (point-max)))
    (should (equal (match-string 0) "\"\"\"first\nsecond\"\"\""))))

(ert-deftest lean-font-lock-fontifies-multiline-forms ()
  (lean-syntax-test-with-buffer "@[foo\n  bar]\ndef doc := \"\"\"first\nsecond\"\"\""
    (lean-syntax-test-fontify)
    (should (eq (get-text-property (point-min) 'face)
                'font-lock-preprocessor-face))
    (goto-char (point-min))
    (search-forward "second")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-string-face))))

(ert-deftest lean-font-lock-bounds-incomplete-attribute ()
  (lean-syntax-test-with-buffer
      (concat "@[" (make-string (* 2 lean-font-lock-max-multiline-span) ?x))
    (goto-char (point-min))
    (should-not (lean-font-lock-match-attribute (point-max)))
    (should (= (point) (1+ lean-font-lock-max-multiline-span)))))

(ert-deftest lean-font-lock-bounds-incomplete-triple-quoted-string ()
  (lean-syntax-test-with-buffer
      (concat "\"\"\"" (make-string (* 2 lean-font-lock-max-multiline-span) ?x))
    (goto-char (point-min))
    (should-not (lean-font-lock-match-string (point-max)))
    (should (= (point) (1+ lean-font-lock-max-multiline-span)))))

;;; lean-syntax-test.el ends here
