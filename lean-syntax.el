;;; lean-syntax.el --- Syntax definitions for lean-mode -*- lexical-binding: t -*-

;; Version: 0.1.0
;; URL: https://github.com/bhargavkulk/lean-mode

;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Leonardo de Moura
;;         Soonho Kong
;; SPDX-License-Identifier: Apache-2.0

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at:
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This library defines syntax for `lean-mode'.

;;; Code:

(require 'rx)

(defconst lean-keywords1
  '("import" "prelude" "protected" "private" "noncomputable"
    "unsafe" "partial" "renaming" "hiding" "begin" "constant"
    "variable" "variables" "theorem" "lemma" "example" "abbrev"
    "open" "export" "axiom" "inductive" "with"
    "structure" "universe" "universes" "hide"
    "precedence" "match_syntax" "match" "nomatch" "infix" "infixl" "infixr" "notation" "postfix" "prefix" "instance"
    "end" "this" "using" "using_well_founded" "namespace" "section"
    "attribute" "local" "set_option" "extends" "include" "class"
    "attributes" "raw" "have" "show" "suffices" "by" "in" "at" "do" "let" "for" "unless" "break" "continue"
    "try" "catch" "finally" "where" "rec" "mut" "forall" "fun"
    "exists" "if" "then" "else" "from" "init_quot" "return"
    "mutual" "def" "run_cmd" "declare_syntax_cat" "syntax" "macro_rules" "macro" "scoped" "elab"
    "initialize" "builtin_initialize" "register_builtin_option" "induction" "cases" "generalizing" "unif_hint" "deriving")
  "Lean keywords ending with `word' (not symbol).")
(defconst lean-keywords1-regexp
  (eval `(rx word-start (or ,@lean-keywords1) word-end)))
(defconst lean-constants
  '("#" "@" "!" "$" "->" "∼" "↔" "/" "==" "=" ":=" "<->" "/\\" "\\/" "∧" "∨"
    "≠" "<" ">" "≤" "≥" "¬" "<=" ">=" "⁻¹" "⬝" "▸" "+" "*" "-" "/" "λ"
    "→" "∃" "∀" "∘" "×" "Σ" "Π" "~" "||" "&&" "≃" "≡" "≅"
    "ℕ" "ℤ" "ℚ" "ℝ" "ℂ" "𝔸"
    "⬝e" "⬝i" "⬝o" "⬝op" "⬝po" "⬝h" "⬝v" "⬝hp" "⬝vp" "⬝ph" "⬝pv" "⬝r" "◾" "◾o"
    "∘n" "∘f" "∘fi" "∘nf" "∘fn" "∘n1f" "∘1nf" "∘f1n" "∘fn1"
    "^c" "≃c" "≅c" "×c" "×f" "×n" "+c" "+f" "+n" "ℕ₋₂")
  "Lean constants.")
(defconst lean-constants-regexp (regexp-opt lean-constants))
(defconst lean-numerals-regexp
  (eval `(rx word-start
             (one-or-more digit) (optional (and "." (zero-or-more digit)))
             word-end)))

(defconst lean-warnings '("sorry") "Lean warnings.")
(defconst lean-warnings-regexp
  (eval `(rx word-start (or ,@lean-warnings) word-end)))
(defconst lean-debugging '("unreachable" "panic" "assert" "dbgTrace") "Lean debugging.")
(defconst lean-debugging-regexp
  (eval `(rx word-start (or ,@lean-debugging))))

(defconst lean-syntax-table
  (let ((st (make-syntax-table)))
    ;; Matching parens
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; comment
    (modify-syntax-entry ?/ ". 14nb" st)
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?« "<" st)
    (modify-syntax-entry ?» ">" st)

    ;; Word constituent
    (seq-doseq (item "abcdefghijklmnopqrstuvwxyz\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
0123456789\
αβγδεζηθικμνξοπρςστυφχψω\
ϊϋόύώϏϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯϰϱϲϳϴϵ϶ϷϸϹϺϻ\
ἀἁἂἃἄἅἆἇἈἉἊἋἌἍἎἏἐἑἒἓἔἕ἖἗ἘἙἚἛἜἝ἞἟ἠἡἢἣἤἥ\
ἦἧἨἩἪἫἬἭἮἯἰἱἲἳἴἵἶἷἸἹἺἻἼἽἾἿὀὁὂὃὄὅ὆὇ὈὉὊὋὌὍ὎὏ὐὑὒὓὔὕὖὗ\
὘Ὑ὚Ὓ὜Ὕ὞ὟὠὡὢὣὤὥὦὧὨὩὪὫὬὭὮὯὰάὲέὴήὶίὸόὺύὼ\
ώ὾὿ᾀᾁᾂᾃᾄᾅᾆᾇᾈᾉᾊᾋᾌᾍᾎᾏᾐᾑᾒᾓᾔᾕᾖᾗᾘᾙᾚᾛᾜᾝᾞᾟᾠᾡᾢᾣᾤᾥᾦᾧᾨᾩᾪᾫᾬᾭᾮᾯᾰ\
ᾱᾲᾳᾴ᾵ᾶᾷᾸᾹᾺΆᾼ᾽ι᾿῀῁ῂῃῄ῅ῆῇῈΈῊΉῌ῍῎῏ῐῑῒΐ῔῕ῖῗ\
ῘῙῚΊ῜῝῞῟ῠῡῢΰῤῥῦῧῨῩῪΎῬ῭΅`῰῱ῲῳῴ῵ῶῷῸΌῺΏῼ´῾\
℀℁ℂ℃℄℅℆ℇ℈℉ℊℋℌℍℎℏℐℑℒℓ℔ℕ№℗℘ℙℚℛℜℝ℞℟℠℡™℣ℤ℥Ω℧ℨ℩KÅℬℭ℮ℯℰℱℲℳℴℵℶℷℸℹ℺℻\
ℼℽℾℿ⅀⅁⅂⅃⅄ⅅⅆⅇⅈⅉ⅊⅋⅌⅍ⅎ⅏\
₁₂₃₄₅₆₇₈₉₀ₐₑₒₓₔₕₖₗₘₙₚₛₜ'_!?")
      (modify-syntax-entry item "w" st))

    ;; Lean operator chars
    (seq-doseq (item "#$%&*+<=>@^|~:")
      (modify-syntax-entry item "." st))

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defconst lean-font-lock-max-multiline-span 8192
  "Maximum number of characters scanned for an unfinished multiline form.

Lean attributes and documentation strings may span lines.  Bound their
font-lock matchers so an unfinished form cannot make normal editing scan the
rest of a large buffer.")

(defun lean-font-lock--set-match (start end)
  "Set the current font-lock match to START and END."
  (set-match-data (list start end)))

(defun lean-font-lock-match-string (limit)
  "Find the next complete Lean string before LIMIT.

Single-line strings and triple-quoted multiline strings are recognized.  An
unfinished triple-quoted string is skipped after
`lean-font-lock-max-multiline-span' characters rather than scanning the whole
buffer."
  (catch 'match
    (while (search-forward "\"" limit t)
      (let ((start (1- (point))))
        (if (looking-at "\"\"")
            (let ((end (min limit
                            (+ start lean-font-lock-max-multiline-span))))
              (goto-char (+ start 3))
              (if (search-forward "\"\"\"" end t)
                  (progn
                    (lean-font-lock--set-match start (point))
                    (throw 'match t))
                (goto-char end)))
          (let ((escaped nil)
                (end (line-end-position))
                closed)
            (while (and (< (point) end) (not closed))
              (let ((character (char-after)))
                (cond
                 (escaped (setq escaped nil))
                 ((eq character ?\\) (setq escaped t))
                 ((eq character ?\") (setq closed (1+ (point)))))
                (forward-char 1)))
            (when closed
              (lean-font-lock--set-match start closed)
              (throw 'match t))))))))

(defun lean-font-lock-match-attribute (limit)
  "Find the next complete Lean attribute before LIMIT.

This recognizes `@[...]' and `attribute [...]'.  Nested brackets are
accepted.  Incomplete attributes are bounded so that a missing closing bracket
does not force fontification to inspect a whole buffer."
  (catch 'match
    (while (re-search-forward "@\\[\\|\\_<attribute\\_>" limit t)
      (let* ((start (match-beginning 0))
             (end (min limit (+ start lean-font-lock-max-multiline-span)))
             (depth (and (string= (match-string 0) "@[") 1)))
        (unless depth
          (skip-chars-forward " \t\n" end)
          (if (eq (char-after) ?\[)
              (progn
                (setq depth 1)
                (forward-char 1))
            (goto-char end)))
        (while (and (> depth 0) (< (point) end))
          (pcase (char-after)
            (?\[ (setq depth (1+ depth)))
            (?\] (setq depth (1- depth))))
          (forward-char 1))
        (if (and depth (zerop depth))
            (progn
              (lean-font-lock--set-match start (point))
              (throw 'match t))
          (goto-char end))))))

(defconst lean-font-lock-defaults
  `((;; attributes
     (lean-font-lock-match-attribute (0 'font-lock-preprocessor-face))
     (,(rx (group "#" (or "eval" "print" "reduce" "help" "check" "lang" "check_failure" "synth")))
      (1 'font-lock-keyword-face))
     ;; mutual definitions "names"
     (,(rx word-start
           "mutual"
           word-end
           (zero-or-more whitespace)
           word-start
           (or "inductive" "definition" "def")
           word-end
           (group (zero-or-more (not (any " \t\n\r{([,"))) (zero-or-more (zero-or-more whitespace) "," (zero-or-more whitespace) (not (any " \t\n\r{([,")))))
      (1 'font-lock-function-name-face))
     ;; Declarations.  Do not consume binder blocks here: an unfinished `{`
     ;; could otherwise make this pattern scan the rest of the buffer.
     (,(rx word-start
           (group (or "inductive" (group "class" (zero-or-more whitespace) "inductive") "instance" "structure" "class" "theorem" "axiom" "lemma" "definition" "def" "constant"))
           word-end (zero-or-more whitespace)
           (group (zero-or-more (not (any " \t\n\r{([")))))
      (3 'font-lock-function-name-face))
     ;; Constants which have a keyword as subterm
     (,(rx (or "∘if")) . 'font-lock-constant-face)
     ;; Keywords
     ("\\(set_option\\)[ \t]*\\([^ \t\n]*\\)" (2 'font-lock-constant-face))
     (,lean-keywords1-regexp . 'font-lock-keyword-face)
     (,(rx word-start (group "example") ".") (1 'font-lock-keyword-face))
     (,(rx (or "∎")) . 'font-lock-keyword-face)
     ;; Types
     (,(rx word-start (or "Prop" "Type" "Type*" "Sort" "Sort*") symbol-end) . 'font-lock-type-face)
     (,(rx word-start (group (or "Prop" "Type" "Sort")) ".") (1 'font-lock-type-face))
     ;; Strings
     (lean-font-lock-match-string (0 'font-lock-string-face))
     ;; ;; Constants
     (,lean-constants-regexp . 'font-lock-constant-face)
     (,lean-numerals-regexp . 'font-lock-constant-face)
     ;; place holder
     (,(rx symbol-start "_" symbol-end) . 'font-lock-preprocessor-face)
     ;; warnings
     (,lean-warnings-regexp . 'font-lock-warning-face)
     (,lean-debugging-regexp . 'font-lock-warning-face)
     ;; escaped identifiers
     (,(rx (and (group "«") (group (one-or-more (not (any "»")))) (group "»")))
      (1 font-lock-comment-face t)
      (2 nil t)
      (3 font-lock-comment-face t)))))

;; Syntax Highlighting for Lean Info Mode
(defconst lean-info-font-lock-defaults
  (let ((new-entries
         `(;; Please add more after this:
           (,(rx (group (+ symbol-start (+ (or word (char ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀))) symbol-end (* white))) ":")
            (1 'font-lock-variable-name-face))
           (,(rx white ":" white)
            . 'font-lock-keyword-face)
           (,(rx "⊢" white)
            . 'font-lock-keyword-face)
           (,(rx "[" (group "stale") "]")
            (1 'font-lock-warning-face))
           (,(rx line-start "No Goal" line-end)
            . 'font-lock-constant-face)))
        (inherited-entries (car lean-font-lock-defaults)))
    `(,(append new-entries inherited-entries))))

(provide 'lean-syntax)
;;; lean-syntax.el ends here
