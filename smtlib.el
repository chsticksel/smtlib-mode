;;; smtlib.el --- Major mode to edit and run SMTLIB v2 files

;; Author: Christoph Sticksel (christoph@sticksel.info)

;; This mode is bases on lisp-mode and provides highlighting of SMTLIB
;; v2 commands, as well as the command run-solver (bound to C-c C-c)
;; to run an SMT solver on the buffer or the region.

;; Add the following lines to your .emacs 
;; 
;;   (setq auto-mode-alist (cons '("\\.smt$" . smtlib-mode) auto-mode-alist))
;;   (autoload 'smtlib-mode "smtlib" "Major mode for SMTLIB" t)

;; The command to run the SMT solver is by default "cvc4 --lang smt",
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
   "push"
   "pop"
   "assert"
   "check-sat"
   "get-assertions"
   "get-proof"
   "get-unsat-core"
   "get-value"
   "get-assignment"
   "get-option"
   "get-info"
   "exit"))

;; Create an optimized regular expression for commands, match only
;; whole words
(setq smtlib-keywords-regexp (regexp-opt smtlib-keywords 'words))

;; Clear memory
(setq smtlib-keywords nil)

;; Create the list for font-lock
(setq 
 smtlib-font-lock-keywords
 `((,smtlib-keywords-regexp . font-lock-keyword-face)))

;; Define the mode
(define-derived-mode smtlib-mode lisp-mode
  "SMTLIB mode"
  "Major mode for editing SMTLIB files"

  ;; code for syntax highlighting
  (setq font-lock-defaults smtlib-font-lock-keywords)

  ;; clear memory
  (setq smtlib-keywords-regexp nil))

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
