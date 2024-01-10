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
  (with-temp-buffer
    (insert result)
    (goto-char (point-max))
    (goto-char (point-min))
    (goto-char (point-min))
    (while (re-search-forward "\\\\left(\\\\begin{array}{\\(r+\\)}" nil t)
      (replace-match "\\\\begin{pmatrix}"))
    (goto-char (point-min))
    (while (re-search-forward "\\\\end{array}\\\\right)" nil t)
      (replace-match "\\\\end{pmatrix}"))
    (buffer-substring-no-properties (point-min) (point-max))
    ))

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
  (let* ((preprocessed
          (let ((pattern (rx "\\\\" (any "\n" space) (group "\\end{pmatrix}"))))
            (replace-regexp-in-string pattern "\\1" latex-expr)))
         (parsed
          (let ((calc-language 'latex))
            (math-read-big-expr preprocessed)))
         (composed
          (let ((calc-language 'maple))
            (symtex--math-compose-expr parsed 0))))
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
			                        "expr = parse_expr(expr_str)"
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

(defvar math-comp-just)
(defvar math-comp-comma-spc)
(defvar math-comp-vector-prec)
(defvar math-comp-left-bracket)
(defvar math-comp-right-bracket)
(defvar math-comp-comma)

;; very slight tweaking of Emacs Calc's `math-compose-expr'
(defun symtex--math-compose-expr (a prec &optional div)
  (let ((calc-multiplication-has-precedence t)
        (math-compose-level (1+ math-compose-level))
        (math-expr-opers (math-expr-ops))
        spfn)
    (cond
     ((or (and (eq a math-comp-selected) a)
	  (and math-comp-tagged
	       (not (eq math-comp-tagged a))))
      (let ((math-comp-selected nil))
	(and math-comp-tagged (setq math-comp-tagged a))
	(list 'tag a (math-compose-expr a prec))))
     ((and (not (consp a)) (not (integerp a)))
      (concat "'" (prin1-to-string a)))
     ((setq spfn (assq (car-safe a)
                       (get calc-language 'math-special-function-table)))
      (setq spfn (cdr spfn))
      (if (consp spfn)
          (funcall (car spfn) a spfn)
        (funcall spfn a)))
     ((math-scalarp a)
      (if (or (eq (car-safe a) 'frac)
	      (and (nth 1 calc-frac-format) (Math-integerp a)))
	  (if (and
               calc-language
               (not (memq calc-language
                          '(flat big unform))))
	      (let ((aa (math-adjust-fraction a))
		    (calc-frac-format nil))
		(math-compose-expr (list '/
					 (if (memq calc-language
                                                   calc-lang-slash-idiv)
					     (math-float (nth 1 aa))
					   (nth 1 aa))
					 (nth 2 aa))
                                   prec))
	    (if (and (eq calc-language 'big)
		     (= (length (car calc-frac-format)) 1))
		(let* ((aa (math-adjust-fraction a))
		       (calc-frac-format nil)
		       (math-radix-explicit-format nil)
		       (c (list 'horiz
				(if (math-negp (nth 1 aa))
				    "- " "")
				(list 'vcent 1
				      (math-format-number
				       (math-abs (nth 1 aa)))
				      '(rule ?-)
				      (math-format-number (nth 2 aa))))))
		  (if (= calc-number-radix 10)
		      c
                    (list 'subscr (math--comp-round-bracket c)
			  (int-to-string calc-number-radix))))
	      (math-format-number a)))
	(if (not (eq calc-language 'big))
	    (math-format-number a prec)
	  (if (memq (car-safe a) '(cplx polar))
	      (if (math-zerop (nth 2 a))
		  (math-compose-expr (nth 1 a) prec)
                (math--comp-round-bracket
		 (list 'horiz
		       (math-compose-expr (nth 1 a) 0)
		       (if (eq (car a) 'cplx) ", " "; ")
		       (math-compose-expr (nth 2 a) 0))))
	    (if (or (= calc-number-radix 10)
		    (not (Math-realp a))
		    (and calc-group-digits
			 (not (assoc calc-group-char '((",") (" "))))))
		(math-format-number a prec)
	      (let ((s (math-format-number a prec))
		    (c nil))
		(while (string-match (if (> calc-number-radix 14)
					 "\\([0-9]+\\)#\\([0-9a-zA-Z., ]+\\)"
				       "\\([0-9]+\\)#\\([0-9a-dA-D., ]+\\)")
				     s)
		  (setq c (nconc c (list (substring s 0 (match-beginning 0))
					 (list 'subscr
					       (math-match-substring s 2)
					       (math-match-substring s 1))))
			s (substring s (match-end 0))))
		(if (string-match
		     "\\*\\([0-9.]+\\)\\^\\(-?[0-9]+\\)\\()?\\)\\'" s)
		    (setq s (list 'horiz
				  (substring s 0 (match-beginning 0)) " "
				  (list 'supscr
					(math-match-substring s 1)
					(math-match-substring s 2))
				  (math-match-substring s 3))))
		(if c (cons 'horiz (nconc c (list s))) s)))))))
     ((and (get (car a) 'math-compose-forms)
	   (not (eq calc-language 'unform))
	   (let ((comps (get (car a) 'math-compose-forms))
		 temp temp2)
	     (or (and (setq temp (assq calc-language comps))
		      (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			       (setq temp (apply (cdr temp2) (cdr a)))
			       (math-compose-expr temp prec))
			  (and (setq temp2 (assq nil (cdr temp)))
			       (funcall (cdr temp2) a))))
		 (and (setq temp (assq nil comps))
		      (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			       (setq temp (apply (cdr temp2) (cdr a)))
			       (math-compose-expr temp prec))
			  (and (setq temp2 (assq nil (cdr temp)))
			       (funcall (cdr temp2) a))))))))
     ((eq (car a) 'vec)
      (let* ((math-comp-left-bracket (if calc-vector-brackets
			       (substring calc-vector-brackets 0 1) ""))
	     (math-comp-right-bracket (if calc-vector-brackets
				(substring calc-vector-brackets 1 2) ""))
	     (inner-brackets (memq 'R calc-matrix-brackets))
	     (outer-brackets (memq 'O calc-matrix-brackets))
	     (row-commas (memq 'C calc-matrix-brackets))
	     (math-comp-comma-spc (or calc-vector-commas " "))
	     (math-comp-comma (or calc-vector-commas ""))
	     (math-comp-vector-prec (if (or (and calc-vector-commas
				                 (math-vector-no-parens a))
				            (memq 'P calc-matrix-brackets))
                                        0 1000))
	     (math-comp-just (cond ((eq calc-matrix-just 'right) 'vright)
                                      ((eq calc-matrix-just 'center) 'vcent)
                                      (t 'vleft)))
	     (break calc-break-vectors))
	(if (and (memq calc-language '(nil big))
		 (not calc-break-vectors)
		 (math-matrixp a) (not (math-matrixp (nth 1 a)))
		 (or calc-full-vectors
		     (and (< (length a) 7) (< (length (nth 1 a)) 7))
		     (progn (setq break t) nil)))
	    (if (progn
		  (setq math-comp-vector-prec (if (or (and calc-vector-commas
                                                           (math-vector-no-parens
                                                            (nth 1 a)))
                                                      (memq 'P calc-matrix-brackets))
                                                  0 1000))
		  (= (length a) 2))
		(list 'horiz
		      (concat math-comp-left-bracket math-comp-left-bracket " ")
		      (math-compose-vector (cdr (nth 1 a)) (concat math-comp-comma " ")
					   math-comp-vector-prec)
		      (concat " " math-comp-right-bracket math-comp-right-bracket))
	      (let* ((rows (1- (length a)))
		     (cols (1- (length (nth 1 a))))
		     (base (/ (1- rows) 2))
		     (calc-language 'flat))
		(append '(horiz)
			(list (append '(vleft)
				      (list base)
				      (list (concat (and outer-brackets
							 (concat math-comp-left-bracket
								 " "))
						    (and inner-brackets
							 (concat math-comp-left-bracket
								 " "))))
				      (make-list (1- rows)
						 (concat (and outer-brackets
							      "  ")
							 (and inner-brackets
							      (concat
							       math-comp-left-bracket
							       " "))))))
			(math-compose-matrix (cdr a) 1 cols base)
			(list (append '(vleft)
				      (list base)
				      (make-list (1- rows)
						 (if inner-brackets
						     (concat " "
							     math-comp-right-bracket
							     (and row-commas
								  math-comp-comma))
						   (if (and outer-brackets
							    row-commas)
						       ";" "")))
				      (list (concat
					     (and inner-brackets
						  (concat " "
							  math-comp-right-bracket))
					     (and outer-brackets
						  (concat
						   " "
						   math-comp-right-bracket)))))))))
	  (if (and calc-display-strings
		   (cdr a)
		   (math-vector-is-string a))
	      (math-vector-to-string a t)
	    (if (and break (cdr a)
		     (not (eq calc-language 'flat)))
		(let* ((full (or calc-full-vectors (< (length a) 7)))
		       (rows (if full (1- (length a)) 5))
		       (base (/ (1- rows) 2))
		       (calc-break-vectors nil))
		  (list 'horiz
			(cons 'vleft (cons base
					   (math-compose-rows
					    (cdr a)
					    (if full rows 3) t)))))
	      (if (or calc-full-vectors (< (length a) 7))
                  (if (and
                       (setq spfn (get calc-language 'math-matrix-formatter))
                       (math-matrixp a))
                      (funcall spfn a)
                    (list 'horiz
                          math-comp-left-bracket
                          (math-compose-vector (cdr a)
                                               (concat math-comp-comma " ")
                                               math-comp-vector-prec)
                          math-comp-right-bracket))
		(list 'horiz
		      math-comp-left-bracket
		      (math-compose-vector (list (nth 1 a) (nth 2 a) (nth 3 a))
					   (concat math-comp-comma " ")
                                           math-comp-vector-prec)
		      math-comp-comma
                      (if (setq spfn (get calc-language 'math-dots))
                          (concat " " spfn)
                        " ...")
		      math-comp-comma " "
		      (list 'break math-compose-level)
		      (math-compose-expr (nth (1- (length a)) a)
					 (if (equal math-comp-comma "") 1000 0))
		      math-comp-right-bracket)))))))
     ((eq (car a) 'incomplete)
      (if (cdr (cdr a))
	  (cond ((eq (nth 1 a) 'vec)
		 (list 'horiz "["
		       (math-compose-vector (cdr (cdr a)) ", " 0)
		       " ..."))
		((eq (nth 1 a) 'cplx)
		 (list 'horiz "("
		       (math-compose-vector (cdr (cdr a)) ", " 0)
		       ", ..."))
		((eq (nth 1 a) 'polar)
		 (list 'horiz "("
		       (math-compose-vector (cdr (cdr a)) "; " 0)
		       "; ..."))
		((eq (nth 1 a) 'intv)
		 (list 'horiz
		       (if (memq (nth 2 a) '(0 1)) "(" "[")
		       (math-compose-vector (cdr (cdr (cdr a))) " .. " 0)
		       " .. ..."))
		(t (format "%s" a)))
	(cond ((eq (nth 1 a) 'vec) "[ ...")
	      ((eq (nth 1 a) 'intv)
	       (if (memq (nth 2 a) '(0 1)) "( ..." "[ ..."))
	      (t "( ..."))))
     ((eq (car a) 'var)
      (let ((v (rassq (nth 2 a) math-expr-variable-mapping)))
	(if v
	    (symbol-name (car v))
          (if (setq spfn (get calc-language 'math-var-formatter))
              (funcall spfn a prec)
            (math-compose-var a)))))
     ((eq (car a) 'intv)
      (math--comp-bracket
       (if (memq (nth 1 a) '(0 1)) ?\( ?\[)
       (if (memq (nth 1 a) '(0 2)) ?\) ?\])
       (list 'horiz
	     (math-compose-expr (nth 2 a) 0)
             " .. "
	     (math-compose-expr (nth 3 a) 0))))
     ((eq (car a) 'date)
      (if (eq (car calc-date-format) 'X)
	  (math-format-date a)
	(concat "<" (math-format-date a) ">")))
     ((and (eq (car a) 'calcFunc-subscr)
           (setq spfn (get calc-language 'math-compose-subscr)))
      (funcall spfn a))
     ((and (eq (car a) 'calcFunc-subscr) (= (length a) 3)
	   (eq calc-language 'big))
      (let* ((a1 (math-compose-expr (nth 1 a) 1000))
	     (calc-language 'flat)
	     (a2 (math-compose-expr (nth 2 a) 0)))
	(if (or (eq (car-safe a1) 'subscr)
		(and (eq (car-safe a1) 'tag)
		     (eq (car-safe (nth 2 a1)) 'subscr)
		     (setq a1 (nth 2 a1))))
	    (list 'subscr
		  (nth 1 a1)
		  (list 'horiz
			(nth 2 a1)
			", "
			a2))
	  (list 'subscr a1 a2))))
     ((and (eq (car a) '^)
	   (eq calc-language 'big))
      (list 'supscr
	    (if (or (math-looks-negp (nth 1 a))
		    (memq (car-safe (nth 1 a)) '(^ / frac calcFunc-sqrt))
		    (and (eq (car-safe (nth 1 a)) 'cplx)
			 (math-negp (nth 1 (nth 1 a)))
			 (eq (nth 2 (nth 1 a)) 0)))
                (math--comp-round-bracket (math-compose-expr (nth 1 a) 0))
	      (math-compose-expr (nth 1 a) 201))
	    (let ((calc-language 'flat)
		  (calc-number-radix 10)
                  (calc-twos-complement-mode nil))
	      (math-compose-expr (nth 2 a) 0))))
     ((and (eq (car a) '/)
	   (eq calc-language 'big))
      (let ((a1 (let ((calc-language (if (memq (car-safe (nth 1 a)) '(/ frac))
					 'flat 'big)))
		  (math-compose-expr (nth 1 a) 0)))
	    (a2 (let ((calc-language (if (memq (car-safe (nth 2 a)) '(/ frac))
					 'flat 'big)))
		  (math-compose-expr (nth 2 a) 0))))
	(list 'vcent
	      (math-comp-height a1)
	      a1 '(rule ?-) a2)))
     ((and (eq (car a) 'calcFunc-lambda)
	   (> (length a) 2)
	   (memq calc-language '(nil flat big)))
      (let ((p (cdr a))
	    (ap calc-arg-values)
	    (math-compose-hash-args (if (= (length a) 3) 1 t)))
	(while (and (cdr p) (equal (car p) (car ap)))
	  (setq p (cdr p) ap (cdr ap)))
	(append '(horiz "<")
		(if (cdr p)
		    (list (math-compose-vector
			   (nreverse (cdr (reverse (cdr a)))) ", " 0)
			  " : ")
		  nil)
		(list (math-compose-expr (nth (1- (length a)) a) 0)
		      ">"))))
     ((and (eq (car a) 'calcFunc-string)
	   (= (length a) 2)
	   (math-vectorp (nth 1 a))
	   (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	  (concat "string(" (math-vector-to-string (nth 1 a) t) ")")
	(math-vector-to-string (nth 1 a) nil)))
     ((and (eq (car a) 'calcFunc-bstring)
	   (= (length a) 2)
	   (math-vectorp (nth 1 a))
	   (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	  (concat "bstring(" (math-vector-to-string (nth 1 a) t) ")")
	(let ((c nil)
	      (s (math-vector-to-string (nth 1 a) nil))
	      p)
	  (while (string-match "[^ ] +[^ ]" s)
	    (setq p (1- (match-end 0))
		  c (cons (list 'break math-compose-level)
			  (cons (substring s 0 p)
				c))
		  s (substring s p)))
	  (setq c (nreverse (cons s c)))
	  (or (= prec -123)
	      (setq c (cons (list 'set math-compose-level 2) c)))
	  (cons 'horiz c))))
     ((and (eq (car a) 'calcFunc-cprec)
	   (not (eq calc-language 'unform))
	   (= (length a) 3)
	   (integerp (nth 2 a)))
      (let ((c (math-compose-expr (nth 1 a) -1)))
	(if (> prec (nth 2 a))
            (if (setq spfn (get calc-language 'math-big-parens))
                (list 'horiz (car spfn) c (cdr spfn))
              (math--comp-round-bracket c))
	  c)))
     ((and (eq (car a) 'calcFunc-choriz)
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3 4))
	   (math-vectorp (nth 1 a))
	   (if (integerp (nth 2 a))
	       (or (null (nth 3 a))
		   (and (math-vectorp (nth 3 a))
			(math-vector-is-string (nth 3 a))))
	     (or (null (nth 2 a))
		 (and (math-vectorp (nth 2 a))
		      (math-vector-is-string (nth 2 a))))))
      (let* ((cprec (and (integerp (nth 2 a)) (nth 2 a)))
	     (sep (nth (if cprec 3 2) a))
	     (bprec nil))
	(if sep
	    (math-compose-vector (cdr (nth 1 a))
				 (math-vector-to-string sep nil)
				 (or cprec prec))
          (cons 'horiz (mapcar (lambda (x)
                                 (if (eq (car-safe x) 'calcFunc-bstring)
                                     (prog1
                                         (math-compose-expr
                                          x (or bprec cprec prec))
                                       (setq bprec -123))
                                   (math-compose-expr x (or cprec prec))))
			       (cdr (nth 1 a)))))))
     ((and (memq (car a) '(calcFunc-cvert calcFunc-clvert calcFunc-crvert))
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3))
	   (math-vectorp (nth 1 a))
	   (or (null (nth 2 a))
	       (integerp (nth 2 a))))
      (let* ((base 0)
	     (v 0)
	     (prec (or (nth 2 a) prec))
             (c (mapcar (lambda (x)
                          (let ((b nil) (cc nil) a d)
                            (if (and (memq (car-safe x) '(calcFunc-cbase
                                                          calcFunc-ctbase
                                                          calcFunc-cbbase))
                                     (memq (length x) '(1 2)))
                                (setq b (car x)
                                      x (nth 1 x)))
                            (if (and (eq (car-safe x) 'calcFunc-crule)
                                     (memq (length x) '(1 2))
                                     (or (null (nth 1 x))
                                         (and (math-vectorp (nth 1 x))
                                              (= (length (nth 1 x)) 2)
                                              (math-vector-is-string
                                               (nth 1 x)))
                                         (and (natnump (nth 1 x))
                                              (<= (nth 1 x) 255))))
                                (setq cc (list
                                          'rule
                                          (if (math-vectorp (nth 1 x))
                                              (aref (math-vector-to-string
                                                     (nth 1 x) nil) 0)
                                            (or (nth 1 x) ?-))))
                              (or (and (memq (car-safe x) '(calcFunc-cvspace
                                                            calcFunc-ctspace
                                                            calcFunc-cbspace))
                                       (memq (length x) '(2 3))
                                       (eq (nth 1 x) 0))
                                  (null x)
                                  (setq cc (math-compose-expr x prec))))
                            (setq a (if cc (math-comp-ascent cc) 0)
                                  d (if cc (math-comp-descent cc) 0))
                            (if (eq b 'calcFunc-cbase)
                                (setq base (+ v a -1))
                              (if (eq b 'calcFunc-ctbase)
                                  (setq base v)
                                (if (eq b 'calcFunc-cbbase)
                                    (setq base (+ v a d -1)))))
                            (setq v (+ v a d))
                            cc))
			(cdr (nth 1 a)))))
	(setq c (delq nil c))
	(if c
	    (cons (if (eq (car a) 'calcFunc-cvert) 'vcent
		    (if (eq (car a) 'calcFunc-clvert) 'vleft 'vright))
		  (cons base c))
	  " ")))
     ((and (memq (car a) '(calcFunc-csup calcFunc-csub))
	   (not (eq calc-language 'unform))
	   (memq (length a) '(3 4))
	   (or (null (nth 3 a))
	       (integerp (nth 3 a))))
      (list (if (eq (car a) 'calcFunc-csup) 'supscr 'subscr)
	    (math-compose-expr (nth 1 a) (or (nth 3 a) 0))
	    (math-compose-expr (nth 2 a) 0)))
     ((and (eq (car a) 'calcFunc-cflat)
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3))
	   (or (null (nth 2 a))
	       (integerp (nth 2 a))))
      (let ((calc-language (if (memq calc-language '(nil big))
			       'flat calc-language)))
	(math-compose-expr (nth 1 a) (or (nth 2 a) 0))))
     ((and (eq (car a) 'calcFunc-cspace)
	   (memq (length a) '(2 3))
	   (natnump (nth 1 a)))
      (if (nth 2 a)
	  (cons 'horiz (make-list (nth 1 a)
				  (if (and (math-vectorp (nth 2 a))
					   (math-vector-is-string (nth 2 a)))
				      (math-vector-to-string (nth 2 a) nil)
				    (math-compose-expr (nth 2 a) 0))))
	(make-string (nth 1 a) ?\ )))
     ((and (memq (car a) '(calcFunc-cvspace calcFunc-ctspace calcFunc-cbspace))
	   (memq (length a) '(2 3))
	   (natnump (nth 1 a)))
      (if (= (nth 1 a) 0)
	  ""
	(let* ((c (if (nth 2 a)
		      (if (and (math-vectorp (nth 2 a))
			       (math-vector-is-string (nth 2 a)))
			  (math-vector-to-string (nth 2 a) nil)
			(math-compose-expr (nth 2 a) 0))
		    " "))
	       (ca (math-comp-ascent c))
	       (cd (math-comp-descent c)))
	  (cons 'vleft
		(cons (if (eq (car a) 'calcFunc-ctspace)
			  (1- ca)
			(if (eq (car a) 'calcFunc-cbspace)
			    (+ (* (1- (nth 1 a)) (+ ca cd)) (1- ca))
			  (/ (1- (* (nth 1 a) (+ ca cd))) 2)))
		      (make-list (nth 1 a) c))))))
     ((and (eq (car a) 'calcFunc-evalto)
	   (setq calc-any-evaltos t)
	   (setq spfn (get calc-language 'math-evalto))
	   (= math-compose-level (if math-comp-tagged 2 1))
	   (= (length a) 3))
      (list 'horiz
            (car spfn)
	    (math-compose-expr (nth 1 a) 0)
	    (cdr spfn)
	    (math-compose-expr (nth 2 a) 0)))
     (t
      (let ((op (and (not (eq calc-language 'unform))
		     (if (and (eq (car a) 'calcFunc-if) (= (length a) 4))
			 (assoc "?" math-expr-opers)
		       (math-assq2 (car a) math-expr-opers)))))
	(cond ((and op
		    (or (= (length a) 3) (eq (car a) 'calcFunc-if))
		    (/= (nth 3 op) -1))
	       (cond
		((or
                  (> prec (or (nth 4 op) (min (nth 2 op) (nth 3 op))))
                  (and div (eq (car a) '*)))
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (if (eq (car-safe a) '/)
			 (list 'horiz "{" (math-compose-expr a -1) "}")
		       (list 'horiz "\\left( "
			     (math-compose-expr a -1)
			     " \\right)"))
		   (if (eq calc-language 'eqn)
		       (if (or (eq (car-safe a) '/)
			       (= (/ prec 100) 9))
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "( " (math-compose-expr a -1) " )")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
                     (math--comp-round-bracket (math-compose-expr a 0)))))
		((and (memq calc-language '(tex latex))
		      (memq (car a) '(/ calcFunc-choose calcFunc-evalto))
		      (>= prec 0))
		 (list 'horiz "{" (math-compose-expr a -1) "}"))
		((eq (car a) 'calcFunc-if)
		 (list 'horiz
		       (math-compose-expr (nth 1 a) (nth 2 op))
		       " ? "
		       (math-compose-expr (nth 2 a) 0)
		       " : "
		       (math-compose-expr (nth 3 a) (nth 3 op))))
		(t
		 (let* ((math-comp-tagged (and math-comp-tagged
					       (not (math-primp a))
					       math-comp-tagged))
			(setlev (if (= prec (min (nth 2 op) (nth 3 op)))
				    (progn
				      (setq math-compose-level
					    (1- math-compose-level))
				      nil)
				  math-compose-level))
			(lhs (math-compose-expr (nth 1 a) (nth 2 op)))
			(rhs (math-compose-expr (nth 2 a) (nth 3 op) (eq (nth 1 op) '/))))
		   (and (equal (car op) "^")
			(eq (math-comp-first-char lhs) ?-)
			(setq lhs (math--comp-round-bracket lhs)))
		   (and (memq calc-language '(tex latex))
			(or (equal (car op) "^") (equal (car op) "_"))
			(not (and (stringp rhs) (= (length rhs) 1)))
			(setq rhs (list 'horiz "{" rhs "}")))
		   (or (and (eq (car a) '*)
			    (or (null calc-language)
				(assoc "2x" math-expr-opers))
			    (let* ((prevt (math-prod-last-term (nth 1 a)))
				   (nextt (math-prod-first-term (nth 2 a)))
				   (prevc (or (math-comp-last-char lhs)
					      (and (memq (car-safe prevt)
							 '(^ calcFunc-subscr
							     calcFunc-sqrt
							     frac))
						   (eq calc-language 'big)
						   ?0)))
				   (nextc (or (math-comp-first-char rhs)
					      (and (memq (car-safe nextt)
							 '(calcFunc-sqrt
							   calcFunc-sum
							   calcFunc-prod
							   calcFunc-integ))
						   (eq calc-language 'big)
						   ?0))))
			      (and prevc nextc
				   (or (and (>= nextc ?a) (<= nextc ?z))
				       (and (>= nextc ?A) (<= nextc ?Z))
				       (and (>= nextc ?α) (<= nextc ?ω))
				       (and (>= nextc ?Α) (<= nextc ?Ω))
				       (and (>= nextc ?0) (<= nextc ?9))
				       (memq nextc '(?. ?_ ?#
							?\( ?\[ ?\{))
				       (and (eq nextc ?\\)
					    (not (string-match
						  "\\`\\\\left("
						  (math-comp-first-string
						   rhs)))))
				   (not (and (eq (car-safe prevt) 'var)
					     (eq nextc ?\()))
				   (list 'horiz
					 (list 'set setlev 1)
					 lhs
					 (list 'break math-compose-level)
                                         (if (eq calc-language 'maple)
                                             "*"
					   " ")
					 rhs))))
		       (list 'horiz
			     (list 'set setlev 1)
			     lhs
			     (list 'break math-compose-level)
			     (if (or (equal (car op) "^")
				     (equal (car op) "_")
				     (equal (car op) "**")
				     (and (equal (car op) "*")
					  (math-comp-last-char lhs)
					  (math-comp-first-char rhs))
				     (and (equal (car op) "/")
					  (math-num-integerp (nth 1 a))
					  (math-integerp (nth 2 a))))
				 (car op)
			       (if (and (eq calc-language 'big)
					(equal (car op) "=>"))
				   "  =>  "
				 (concat " " (car op) " ")))
			     rhs))))))
	      ((and op (= (length a) 2) (= (nth 3 op) -1))
	       (cond
		((or (> prec (or (nth 4 op) (nth 2 op)))
		     (and (not (eq (assoc (car op) math-expr-opers) op))
			  (> prec 0)))   ; don't write x% + y
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (list 'horiz "\\left( "
			   (math-compose-expr a -1)
			   " \\right)")
		   (if (eq calc-language 'eqn)
		       (if (= (/ prec 100) 9)
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "{( " (math-compose-expr a -1) " )}")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (math--comp-round-bracket (math-compose-expr a 0)))))
		(t
		 (let ((lhs (math-compose-expr (nth 1 a) (nth 2 op))))
		 (list 'horiz
		       lhs
		       (if (or (> (length (car op)) 1)
			       (not (math-comp-is-flat lhs)))
			   (concat " " (car op))
			 (car op)))))))
	      ((and op (= (length a) 2) (= (nth 2 op) -1))
	       (cond
		((eq (nth 3 op) 0)
		 (let ((lr (and (memq calc-language '(tex latex))
				(not (math-tex-expr-is-flat (nth 1 a))))))
		   (list 'horiz
			 (if lr "\\left" "")
			 (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'" (car op))
			     (substring (car op) 1)
			   (car op))
			 (if (or lr (> (length (car op)) 2)) " " "")
			 (math-compose-expr (nth 1 a) -1)
			 (if (or lr (> (length (car op)) 2)) " " "")
			 (if lr "\\right" "")
			 (car (nth 1 (memq op math-expr-opers))))))
		((> prec (or (nth 4 op) (nth 3 op)))
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (list 'horiz "\\left( "
			   (math-compose-expr a -1)
			   " \\right)")
		   (if (eq calc-language 'eqn)
		       (if (= (/ prec 100) 9)
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "{( " (math-compose-expr a -1) " )}")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (math--comp-round-bracket (math-compose-expr a 0)))))
		(t
		 (let ((rhs (math-compose-expr (nth 1 a) (nth 3 op))))
		   (list 'horiz
			 (let ((ops (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'"
						      (car op))
					(substring (car op) 1)
				      (car op))))
			   (if (or (> (length ops) 1)
				   (not (math-comp-is-flat rhs)))
			       (concat ops " ")
			     ops))
			 rhs)))))
	      ((and (eq calc-language 'big)
		    (setq op (get (car a) 'math-compose-big))
		    (funcall op a prec)))
	      ((and (setq op (assq calc-language
				   '( ( nil . math-compose-normal )
				      ( flat . math-compose-normal )
				      ( big . math-compose-normal )
				      ( c . math-compose-c )
				      ( pascal . math-compose-pascal )
				      ( fortran . math-compose-fortran )
				      ( tex . math-compose-tex )
				      ( latex . math-compose-latex )
				      ( eqn . math-compose-eqn )
                                      ( yacas . math-compose-yacas )
                                      ( maxima . math-compose-maxima )
                                      ( giac . math-compose-giac )
				      ( math . math-compose-math )
				      ( maple . math-compose-maple ))))
		    (setq op (get (car a) (cdr op)))
		    (funcall op a prec)))
	      (t
	       (let* ((func (car a))
		      (func2 (assq func '(( mod . calcFunc-makemod )
					  ( sdev . calcFunc-sdev )
					  ( + . calcFunc-add )
					  ( - . calcFunc-sub )
					  ( * . calcFunc-mul )
					  ( / . calcFunc-div )
					  ( % . calcFunc-mod )
					  ( ^ . calcFunc-pow )
					  ( neg . calcFunc-neg )
					  ( | . calcFunc-vconcat )))))
		 (if func2
		     (setq func (cdr func2)))
		 (if (setq func2 (rassq func math-expr-function-mapping))
		     (setq func (car func2)))
		 (setq func (math-remove-dashes
			     (if (string-match
				  "\\`calcFunc-\\([a-zA-Zα-ωΑ-Ω0-9']+\\)\\'"
				  (symbol-name func))
				 (math-match-substring (symbol-name func) 1)
			       (symbol-name func))))
		 (if (memq calc-language calc-lang-allow-percentsigns)
		     (setq func (math-to-percentsigns func)))
		 (if (memq calc-language calc-lang-allow-underscores)
		     (setq func (math-to-underscores func)))
                 (if (setq spfn (get calc-language 'math-func-formatter))
                     (funcall spfn func a)

                   (let ((args (math-compose-vector (cdr a) ", " 0)))
                     (if (and (member calc-function-open '("(" "[" "{"))
                              (member calc-function-close '(")" "]" "}")))
                         (list 'horiz func
                               (math--comp-bracket
                                (string-to-char calc-function-open)
                                (string-to-char calc-function-close)
                                args))
                       (list 'horiz func calc-function-open
		             args calc-function-close))))))))))))


(provide 'symtex)
;;; symtex.el ends here
