;;; lean-indent.el --- Enhanced relative indentation (eri) -*- lexical-binding: t -*-

;; Version: 0.1.0
;; URL: https://github.com/bhargavkulk/lean-mode

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Enhanced relative indentation for Lean, adapted from Agda mode.

;;; Code:

(require 'cl-lib)

(defun lean-eri-current-line-length nil
  "Calculate length of current line."
  (- (line-end-position) (line-beginning-position)))

(defun lean-eri-current-line-empty nil
  "Return non-nil if the current line is empty (not counting white space)."
  (equal (current-indentation)
         (lean-eri-current-line-length)))

(defun lean-eri-maximum (xs)
  "Calculate maximum element in XS.
Returns nil if the list is empty."
  (if xs (apply #'max xs)))

(defun lean-eri-take (n xs)
  "Return the first N elements of XS."
  (butlast xs (- (length xs) n)))

(defun lean-eri-split (x xs)
  "Return a pair of lists split around X in sorted list XS."
  (let* ((pos (or (cl-position-if (lambda (y) (> y x)) xs) (length xs)))
         (xs1 (lean-eri-take pos xs))
         (xs2 (nthcdr pos xs)))
    (cons xs1 xs2)))

(defun lean-eri-calculate-indentation-points-on-line (max)
  "Calculate indentation points on the current line before MAX."
  (let ((result))
    (save-excursion
      (save-restriction
        (beginning-of-line)
        (narrow-to-region (line-beginning-position) (line-end-position))
        (while
            (progn
              (let ((pos (and (search-forward-regexp
                               "\\(?:\\s-\\|\\`\\)\\(\\S-\\)" nil t)
                              (match-beginning 1))))
                (when pos
                  (let ((pos1 (- pos (line-beginning-position))))
                    (when (or (null max) (< pos1 max))
                      (cl-pushnew pos1 result))))
                (and pos
                     (not (eolp))
                     (or (null max) (< (current-column) max))))))
        (nreverse result)))))

(defun lean-eri-new-indentation-points ()
  "Return a new indentation point based on the previous non-empty line."
  (let ((start (line-beginning-position)))
    (save-excursion
      (while
          (progn
            (forward-line -1)
            (not (or (bobp)
                     (not (lean-eri-current-line-empty))))))
      (if (or (equal (point) start)
              (lean-eri-current-line-empty))
          nil
        (list (+ 2 (current-indentation)))))))

(defun lean-eri-calculate-indentation-points (reverse)
  "Calculate indentation points above the current line.
If REVERSE is non-nil, return them in reverse cycling order."
  (let ((points)
        (max)
        (start (line-beginning-position)))
    (save-excursion
      (while
          (progn
            (forward-line -1)
            (unless (or (equal (point) start)
                        (lean-eri-current-line-empty))
              (setq points
                    (append
                     (lean-eri-calculate-indentation-points-on-line max)
                     points))
              (setq max (car points)))
            (not (or (bobp)
                     (and (equal (current-indentation) 0)
                          (> (lean-eri-current-line-length) 0)))))))
    (let* ((ps0 (remove (current-indentation)
                        (append (lean-eri-new-indentation-points) points)))
           (ps1 (lean-eri-split (current-indentation) (sort ps0 '<)))
           (ps2 (append (cdr ps1) (car ps1))))
      (if reverse
          (nreverse ps2)
        ps2))))

(defun lean-eri-indent (&optional reverse)
  "Cycle through plausible indentation points.
With prefix argument REVERSE, cycle in reverse order."
  (interactive "P")
  (let* ((points (lean-eri-calculate-indentation-points reverse))
         (remaining-points (cdr (member (current-indentation) points)))
         (indentation (if remaining-points
                          (car remaining-points)
                        (car points))))
    (when indentation
      (save-excursion (indent-line-to indentation))
      (if (< (current-column) indentation)
          (indent-line-to indentation)))))

(defun lean-eri-indent-reverse nil
  "Cycle through plausible indentation points in reverse order."
  (interactive)
  (lean-eri-indent t))

(defun lean-indent-line ()
  "Indent the current Lean line using ERI indentation."
  (let ((cur-column (current-column))
        (cur-indent (current-indentation)))
    (cond ((= cur-column cur-indent)
           (lean-eri-indent))
          ((< cur-column cur-indent)
           (move-to-column cur-indent)))))

(provide 'lean-indent)
;;; lean-indent.el ends here
