smtlib-mode
===========

An Emacs major mode to edit and run SMTLIB v2 files

This mode is based on lisp-mode and provides highlighting of SMTLIB v2 commands, as well as the command run-solver (bound to `C-c C-c`) to run an SMT solver on the buffer or the region.

Add the following lines to your .emacs 
```
(setq auto-mode-alist (cons '("\\.smt$" . smtlib-mode) auto-mode-alist))
(autoload 'smtlib-mode "smtlib" "Major mode for SMTLIB" t)
```

The command to run the SMT solver is by default "cvc4 --lang smt", modify and add the following line to your .emacs to change.

```
(setq smtlib-solver-cmd "cvc4 --lang smt2")
```
