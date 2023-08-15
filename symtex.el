;;; symtex.el --- Use SymPy/SAGE in a TeX buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/symtex.el
;; Package-Requires: ((emacs "26.1") (czm-tex-util "0.1"))
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

;; This package provides functions for evaluating LaTeX math
;; expressions with SageMath.

;;; Code:

(require 'sage-shell-mode)
(require 'czm-tex-util)

(defgroup symtex nil
  "Symtex: A package for parsing and evaluating LaTeX math expressions with SageMath."
  :prefix "symtex-"
  :group 'applications)

(defcustom symtex-expand-expression
  "expr.expand()*2/2"
  "SAGE expression for expansion."
  :type 'string
  :group 'symtex)

(defcustom symtex-latex2sympy-import-statement
  "load(\"~/doit/sage/cool_latex_parse.py\")"
  "Path to the parsing library file."
  :type 'string
  :group 'symtex)

(defcustom symtex-temp-dir
  "~/temp-sage/"
  "Directory to store temporary results."
  :type 'string
  :group 'symtex)

(defcustom symtex-latex2sympy-expr
  "latex2sympy(expr_str)"
  "Name of the function to use for parsing LaTeX."
  :type 'string
  :group 'symtex)

(defcustom symtex-sympy2latex-expr
  ;; "latex(result_expr._sage_())"
  (mapconcat #'identity
	     (list
	      "try:"
	      "    result_str = sympy2latex(result_expr._sage_()._sympy_())"
	      "except Exception as e:"
	      "    result_str = sympy2latex(result_expr)")
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

(defun symtex-setup-python ()
  "Set variables for using the Python version of the parser."
  (interactive)
  (setq
   symtex-latex2sympy-import-statement
   "exec(open(\"/Users/au710211/doit/sage/cool_latex_parse.py\").read())"
   symtex-sympy2latex-expr
   "result_str = sympy2latex(result_expr)"
   symtex-sage-src-block
   "#+begin_src python :results silent :session\n%s\n#+end_src"
   symtex-finale
   "result_str"))

(defun symtex-setup-sage ()
  "Set variables for using the Sage version of the parser."
  (interactive)
  (setq
   symtex-latex2sympy-import-statement
   "load(\"~/doit/sage/cool_latex_parse.py\")"
   symtex-sympy2latex-expr
   ;; "result_str = sympy2latex(result_expr._sage_()._sympy_())"
   "result_str = sympy2latex(result_expr)"
   symtex-sage-src-block
   "#+begin_src sage :results silent\n%s\n#+end_src"
   symtex-finale
   "result_str"))

;;;###autoload
(defun symtex-dwim (&optional arg)
  "Apply SAGE function to the current environment or region.
With prefix ARG, simply return the result of calling the SAGE
function \"expand\" on the current environment or region.
Otherwise, prompt for a SAGE function to apply."
  (interactive "P")
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (czm-tex-util-environment-bounds)))
	 (beg (car bounds))
	 (end (cdr bounds)))
    (symtex-dwim-region beg end arg)))

(defun symtex-dwim-region (beg end &optional arg)
  (interactive "r\nP")
  (if arg
      (symtex-read-expand-region beg end)
    (symtex-read-evaluate-region beg end)))

(defun symtex-read-expand-region (beg end)
  (interactive "r")
  (symtex-read-evaluate-region beg end symtex-expand-expression))

(defun symtex-read-evaluate-region (beg end &optional result-expr)
  (interactive "r")
  (unless result-expr
    (setq result-expr
          (read-string "SAGE function to apply (use 'expr' as variable):")))
  (let ((latex-expr (buffer-substring-no-properties beg end)))
    (symtex-process result-expr latex-expr)))

;;;###autoload
(defun symtex-process (result-expr &optional latex-expr)
  (interactive "sSAGE expression to evaluate:")
  (let ((sage-code (mapconcat
		     #'identity
		     (list
		      symtex-latex2sympy-import-statement
		      (when latex-expr
			(format
			 (mapconcat
			  #'identity
			  (list
			   "expr_str = r'''{%s}'''"
			   "expr = %s"
			   ;; "if not exprs:"
			   ;; "    exprs = [expr]"
			   ;; "else:"
			   ;; "    exprs.append(expr)"
			   )
			  "\n")
			 latex-expr symtex-latex2sympy-expr))
		      (format "result_expr = %s" result-expr)
		      ;; "if not result_exprs:"
		      ;; "    result_exprs = [result_expr]"
		      ;; "else:"
		      ;; "    result_exprs.append(result_expr)"
		      symtex-sympy2latex-expr
		      symtex-finale)
		     "\n")))
    (symtex-evaluate-copy-result sage-code)))

(defun symtex-evaluate-copy-result (sage-code)
  (interactive)
  (let* ((result (symtex-evaluate sage-code))
	 (formatted-result (symtex-format result)))
    (kill-new formatted-result)
    (message "Result saved to kill-ring: %s" formatted-result)))

(defun symtex-format (result)
  (let ((formatted-result result))
    (setq formatted-result (substring formatted-result 1 -1))
    (setq formatted-result (string-replace "\\\\" "\\" formatted-result))
    (setq formatted-result (string-replace "bmatrix" "pmatrix" formatted-result))
    formatted-result))

(defun symtex-evaluate (sage-code)
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (org-file-path (expand-file-name (concat "temp-sage-" date-str ".org") symtex-temp-dir ))
         (org-buffer (find-file-noselect org-file-path))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         result contents)
    (with-temp-buffer
      (insert (format symtex-sage-src-block sage-code))
      (goto-char (point-min))
      (setq result
	    (let ((python-indent-guess-indent-offset-verbose nil))
	      (org-babel-execute-src-block)))
      (setq contents (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer org-buffer
      (goto-char (point-max))
      (org-insert-heading t nil t)
      (insert (format "%s\n" timestamp))
      (insert contents)
      (save-buffer))
    result))

(provide 'symtex)
;;; symtex.el ends here


;; (sage-code (mapconcat
;; 		    #'identity
;; 		    (list
;; 		     symtex-latex2sympy-import-statement
;; 		     (when latex-expr
;; 		       (format "expr_str = r'''{%s}'''\n%s" latex-expr symtex-latex2sympy-expr))
;; 		     (format "result_expr = %s" result-expr)
;; 		     (format "%s(result_expr)" symtex-sympy2latex-expr)
;; 		     "latex(result_expr._sage_())")
;; 		    "\n"))
