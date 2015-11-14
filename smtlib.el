;;; smtlib.el --- Major mode to edit and run SMTLIB v2 files

;; Author: Christoph Sticksel (christoph@sticksel.info), Alain Mebsout

;; This mode is based on lisp-mode and provides highlighting of SMTLIB
;; v2 commands, as well as the command run-solver (bound to C-c C-c)
;; to run an SMT solver on the buffer or the region.

;; Add the following lines to your .emacs 
;; 
;;   (setq auto-mode-alist (cons '("\\.smt2$" . smtlib-mode) auto-mode-alist))
;;   (autoload 'smtlib-mode "smtlib" "Major mode for SMTLIB" t)

;; The command to run the SMT solver is by default "cvc4 --lang smt2",
;; modify and add the following line to your .emacs to change.

;;   (setq smtlib-solver-cmd "cvc4 --lang smt2")

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Define a sparse local keymap with default key bindings
(defvar smtlib-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'run-solver) map)
  "Keymap for smtlib-mode")

;; Create syntax table, add characters as word components
(defvar smtlib-mode-syntax-table
  (let ((st (make-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?! "w" st)
    (modify-syntax-entry ?? "w" st)
    (modify-syntax-entry ?- "w" st)
    ;; (modify-syntax-entry ?+ "w" st)
    ;; (modify-syntax-entry ?< "w" st)
    ;; (modify-syntax-entry ?= "w" st)
    ;; (modify-syntax-entry ?> "w" st)
    st)
  "Syntax table for `smtlib-mode'.")

;; SMTLIB commands as keywords
(setq 
 smtlib-keywords
 '("set-logic"
   "set-option"
   "set-info"
   "declare-sort"
   "define-sort"
   "declare-fun"
   "define-fun"
   "define-fun-rec"
   "define-funs-rec"
   "push"
   "pop"
   ;; "assert"
   ;; "check-sat"
   "get-assertions"
   "get-proof"
   "get-model"
   "get-unsat-core"
   "get-value"
   "get-assignment"
   "get-option"
   "get-info"
   "exit"))

(setq smtlib-constants '("true" "false"))

(setq smtlib-cmds '("assert" "check-sat"))

(setq smtlib-types '("Int" "Bool" "Real" "Array"))

(setq smtlib-combinators '("or" "and" "xor" "=>" "not" "ite" "forall" "exists" "let" "!"))

;; This is probably pushing it. in case you want that, uncomment lines related
;; to operators and the corresponding entries in the syntax table.
;; (setq smtlib-operators '("+" "<" "<=" ">" ">=" "-" "*" "/" "div" "mod" "="))

;; Create an optimized regular expression for commands, match only
;; whole words
(setq smtlib-keywords-regexp (regexp-opt smtlib-keywords 'words))
(setq smtlib-constants-regexp (regexp-opt smtlib-constants 'words))
(setq smtlib-types-regexp (regexp-opt smtlib-types 'words))
(setq smtlib-combinators-regexp (regexp-opt smtlib-combinators 'words))
(setq smtlib-cmds-regexp (regexp-opt smtlib-cmds 'words))
;; (setq smtlib-operators-regexp (regexp-opt smtlib-operators 'words))

;; Clear memory
(setq smtlib-keywords nil)
(setq smtlib-constants nil)
(setq smtlib-types nil)
(setq smtlib-combinators nil)
(setq smtlib-cmds nil)
;; (setq smtlib-operators nil)

;; Create the list for font-lock
(setq 
 smtlib-font-lock
 `(
   (,smtlib-keywords-regexp . font-lock-keyword-face)
   (,smtlib-constants-regexp . font-lock-constant-face)
   (,smtlib-types-regexp . font-lock-type-face)
   ;; (,smtlib-operators-regexp . font-lock-variable-name-face)
   (,smtlib-combinators-regexp . font-lock-builtin-face)
   (,smtlib-cmds-regexp . font-lock-warning-face)
   ("\\b\\([0-9]*\\.?[0-9]+\\)\\b" . font-lock-constant-face)
   (":\\(\\sw+\\)" . font-lock-doc-face)
   ;; recognize logical constants decls/defs
   ("\\(?:declare-fun\\|define-fun\\|define-fun-rec\\)\\(?:\\s-\\|\n\\)+\\(\\sw+\\)\\(?:\\s-\\|\n\\)+(\\(?:\\s-\\|\n\\)*)"
    (1 font-lock-variable-name-face))
   ;; recognize functions
   ("\\(?:declare-fun\\|define-fun\\|define-fun-rec\\)\\(?:\\s-\\|\n\\)+\\(\\sw+\\)"
    (1 font-lock-function-name-face))
   ))

;; Define the mode
(define-derived-mode smtlib-mode lisp-mode
  "SMTLIB mode"
  "Major mode for editing SMTLIB files"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((smtlib-font-lock)))

  ;; clear memory
  (setq smtlib-keywords-regexp nil)
  (setq smtlib-constants-regexp nil)
  (setq smtlib-types-regexp nil)
  ;; (setq smtlib-operators-regexp nil)
  (setq smtlib-combinators-regexp nil)
  (setq smtlib-cmds-regexp nil)
  )

;; Default to run SMT solver
(defvar smtlib-solver-cmd "cvc4 --lang smt2" "Command to run SMT solver") 

;; Command to run SMT solver on the whole buffer 
(defun run-solver ()
  "Run the SMT solver on the buffer "
  (interactive)
  (shell-command-on-region
   (if (region-active-p) (region-beginning) (point-min))
   (if (region-active-p) (region-end) (point-max))
   (read-shell-command "Run SMT solver: " smtlib-solver-cmd)
   "SMT Solver Output"))

;; Need this as last line
(provide 'smtlib-mode)
