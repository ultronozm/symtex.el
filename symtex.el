;;; symtex.el --- Evaluate SAGE code on parts of a TeX buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/symtex.el
;; Package-Requires: ((emacs "26.1") (czm-tex-util "0.0") (sage-shell-mode "0.3") (ob-sagemath "0.4"))
;; Keywords: tex, tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions for operating on LaTeX math
;; expressions using Sage.  See the README or the documentation of
;; `symtex-dwim' for details.
;; 

;;; Code:

(require 'sage-shell-mode)
(require 'czm-tex-util)
(require 'python)
(require 'ob-sagemath)

(defgroup symtex nil
  "Symtex: A package for parsing and evaluating LaTeX math expressions with SageMath."
  :prefix "symtex-"
  :group 'applications)

(defcustom symtex-expand-expression
  "expr.expand()*2/2"
  "sage expression for expansion."
  :type 'string
  :group 'symtex)

(defconst symtex--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory containing this file.")

(defcustom symtex-temp-dir
  "~/temp-sage/"
  "Directory to store temporary results."
  :type 'string
  :group 'symtex)

(defcustom symtex-sympy2latex-expr
  ;; "latex(result_expr._sage_())"
  (mapconcat #'identity
	     (list
       "result_str = latex(result_expr._sage_())")
	     "\n")
  "Expression used for converting SymPy to LaTeX."
  :type 'string
  :group 'symtex)

(defcustom symtex-sage-src-block
  "#+begin_src sage :results silent\n%s\n#+end_src"
  "Format of the Org-mode Sage source block used for evaluation."
  :type 'string
  :group 'symtex)

(defcustom symtex-finale
  "result_str"
  "Expression returned when converting SymPy to LaTeX.
This is the last expression in the Sage source block.  The idea
is that you can customize this to do some post-processing."
  :type 'string
  :group 'symtex)

(defun symtex--evaluate (sage-code)
  "Evaluate SAGE-CODE.
Create an org-mode sage source block in a temporary buffer, call
`org-babel-execute-src-block', and return the result.  For future
reference and debugging, the sage code used to produce this
result is saved in `symtex-temp-dir'."
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (org-file-path (expand-file-name (concat "temp-sage-" date-str ".org") symtex-temp-dir ))
         (org-buffer (find-file-noselect org-file-path))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         result contents)
    (with-temp-buffer
      (insert (format symtex-sage-src-block sage-code))
      (goto-char (point-min))
      (setq result
	    (let ((python-indent-guess-indent-offset-verbose nil)
                  (inhibit-message t))
	      (org-babel-execute-src-block)))
      (setq contents (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer org-buffer
      (goto-char (point-max))
      (org-insert-heading t nil t)
      (insert (format "%s\n" timestamp))
      (insert contents)
      (save-buffer))
    result))

(defun symtex--tidy (result)
  "Tidy the RESULT of some sage code evaluation."
  (let ((tidied-result result))
    (setq tidied-result (substring tidied-result 1 -1))
    (setq tidied-result (string-replace "\\\\" "\\" tidied-result))
    (setq tidied-result (string-replace "bmatrix" "pmatrix" tidied-result))
    tidied-result))

(defun symtex--evaluate-copy-result (sage-code)
  "Evaluate SAGE-CODE.  Save the (tidied) result in the kill ring."
  (interactive)
  (let* ((result (symtex--evaluate sage-code))
	 (tidied-result (symtex--tidy result)))
    (kill-new tidied-result)
    (message "Result saved to kill-ring: %s" tidied-result)))

(defun symtex--format-list (list)
  (mapconcat (lambda (item)
               (cond ((listp item)
                      (symtex--format-list item))
                     ((stringp item)
                      (cond
                       ((equal item "^")
                        "**")
                       ((equal item "matrix(")
                        "Matrix(")
                      (t
                       item)))))
             list))

(defun symtex--parse-latex (latex-expr)
  "Parse LATEX-EXPR."
  (let* ((parsed
          (let ((calc-language 'latex))
            (math-read-big-expr latex-expr)))
         (composed
          (let ((calc-language 'maple))
            (math-compose-expr parsed 0))))
    (symtex--format-list composed)))

;;;###autoload
(defun symtex-process (output &optional input)
  "Evaluate sage code OUTPUT using TeX code INPUT.
The sage code OUTPUT is evaluated, its result converted to TeX
and stored in the kill ring.  If INPUT is non-nil, then it is
converted to a sage object using latex2sympy and stored in the
sage variable `expr' prior to the evaluation of OUTPUT."
  (interactive "sSage expression to evaluate:")
  (let* ((sage-code (mapconcat
		                   #'identity
		                   (list
		                    (when input
                        (format
			                      (mapconcat
			                       #'identity
			                       (list
			                        "expr_str = r'''%s'''"
			                        "expr = sympy.parse_expr(expr_str)"
			                        )
			                       "\n")
			                      (symtex--parse-latex input)))
		                    (format "result_expr = %s" output)
		                    ;; "if not result_exprs:"
		                    ;; "    result_exprs = [result_expr]"
		                    ;; "else:"
		                    ;; "    result_exprs.append(result_expr)"
		                    symtex-sympy2latex-expr
		                    symtex-finale)
		                   "\n")))
    (symtex--evaluate-copy-result sage-code)))

(defun symtex--read-evaluate-region (beg end &optional output)
  "Evaluate SAGE expression involving TeX region (BEG . END).
If OUTPUT is nil, then we prompt for it from the minibuffer.  The
result is stored in the kill ring; see the documentation for
`symtex-process'."
  (interactive "r")
  (unless output
    (setq output
          (read-string "Sage function to apply (use 'expr' as variable):")))
  (let ((input (buffer-substring-no-properties beg end)))
    (symtex-process output input)))

(defun symtex--read-expand-region (beg end)
  "Symbolically expand TeX region between BEG and END.
The customization variable `symtex-expand-expression' gives the
expression used to expand the region contents.  The result is
stored in the kill ring; see the documentation for
`symtex-process'."
  (interactive "r")
  (symtex--read-evaluate-region beg end symtex-expand-expression))

(defun symtex-dwim-region (beg end &optional arg)
  "Evaluate sage expression involving the region (BEG . END).
This user-facing function is
intended to be called interactively with an active region.  If
prefix ARG is provided, then the contents of that region are
simply expanded (via `symtex-expand-expression').  Otherwise, the
user is prompted for a sage expression to evaluate.  See the
documentation for `symtex-dwim' for more information."
  (interactive "r\nP")
  (if arg
      (symtex--read-expand-region beg end)
    (symtex--read-evaluate-region beg end)))

;;;###autoload
(defun symtex-dwim (&optional arg)
  "Evaluate sage expression, storing result as TeX in the kill ring.
If the region is not active, then prompt for sage expression to
evaluate.

If the region is active, then convert its contents to a sage
expression, store the result of that conversion in the sage
variable `expr', and prompt for a sage expression to evaluate.

With prefix ARG, evaluate the sage expression stored in the
variable `symtex-expand-expression'."
  (interactive "P")
  (if (use-region-p)
      (let* ((bounds (cons (region-beginning) (region-end)))
	     (beg (car bounds))
	     (end (cdr bounds)))
        (symtex-dwim-region beg end arg))
    (call-interactively #'symtex-process)))

;; old experiments:

;; (sage-code (mapconcat
;; 		    #'identity
;; 		    (list
;; 		     symtex--latex2sympy-import-statement
;; 		     (when latex-expr
;; 		       (format "expr_str = r'''{%s}'''\n%s" latex-expr symtex-latex2sympy-expr))
;; 		     (format "result_expr = %s" result-expr)
;; 		     (format "%s(result_expr)" symtex-sympy2latex-expr)
;; 		     "latex(result_expr._sage_())")
;; 		    "\n"))


;; (defun symtex-setup-python ()
;;   "Set variables for using the Python version of the parser."
;;   (interactive)
;;   (setq
;;    symtex--latex2sympy-import-statement
;;    "exec(open(\"/Users/au710211/doit/sage/cool_latex_parse.py\").read())"
;;    symtex-sympy2latex-expr
;;    "result_str = sympy2latex(result_expr)"
;;    symtex-sage-src-block
;;    "#+begin_src python :results silent :session\n%s\n#+end_src"
;;    symtex-finale
;;    "result_str"))

;; (defun symtex-setup-sage ()
;;   "Set variables for using the Sage version of the parser."
;;   (interactive)
;;   (setq
;;    symtex--latex2sympy-import-statement
;;    "load(\"~/doit/sage/cool_latex_parse.py\")"
;;    symtex-sympy2latex-expr
;;    ;; "result_str = sympy2latex(result_expr._sage_()._sympy_())"
;;    "result_str = sympy2latex(result_expr)"
;;    symtex-sage-src-block
;;    "#+begin_src sage :results silent\n%s\n#+end_src"
;;    symtex-finale
;;    "result_str"))


(provide 'symtex)
;;; symtex.el ends here
