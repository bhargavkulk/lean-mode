;;; lean-mode.el --- Minimal Lean 4 mode with Eglot -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Author: Bhargav Kulkarni
;; URL: https://github.com/bhargavkulk/lean-mode
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, tools, lean

;;; Commentary:
;; A small Lean 4 major mode using Eglot for LSP, with syntax highlighting,
;; relative indentation, and a Lean input method.

;;; Code:

(require 'eglot)
(require 'cl-lib)
(require 'flymake)
(require 'project)
(require 'lean-syntax)
(require 'lean-info)

(require 'lean-indent)
(require 'lean-input)

(define-abbrev-table 'lean-mode-abbrev-table
  '())

(defclass lean-eglot-lsp-server (eglot-lsp-server) nil
  :documentation "Eglot server class for Lean.")

(defun lean-eglot-server-class-init (&optional _interactive)
  "Return the Lean Eglot server class and command."
  (list 'lean-eglot-lsp-server "lake" "serve"))

(defun lean-refresh-file-dependencies ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (when eglot--managed-mode
    (eglot--signal-textDocument/didClose)
    (eglot--signal-textDocument/didOpen)))

(define-derived-mode lean-mode prog-mode "Lean"
  "Simple mode for Lean 4 files."
  :syntax-table lean-syntax-table
  :abbrev-table lean-mode-abbrev-table
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults lean-font-lock-defaults)
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'lean-indent-line)
  (setq-local next-error-function #'flymake-goto-next-error)
  (set-input-method "Lean")
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1)))

(add-to-list 'auto-mode-alist '("\\.lean\\'" . lean-mode))

(add-to-list 'eglot-server-programs
             (cons 'lean-mode #'lean-eglot-server-class-init))

(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

(add-to-list 'project-vc-extra-root-markers "lean-toolchain")

(defun lean-eglot-ensure ()
  "Start Eglot only in a project rooted by `lean-toolchain'."
  (when-let* ((project (project-current))
              (root (project-root project))
              ((file-exists-p (expand-file-name "lean-toolchain" root))))
    (eglot-ensure)))

(add-hook 'lean-mode-hook #'lean-eglot-ensure)
(add-hook 'eglot-managed-mode-hook #'lean-info-auto-open)

(provide 'lean-mode)
;;; lean-mode.el ends here
