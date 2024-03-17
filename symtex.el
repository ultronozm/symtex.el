;;; symtex.el --- Evaluate SAGE code on parts of a TeX buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/symtex.el
;; Package-Requires: ((emacs "28.1") (czm-tex-util "0.0") (sage-shell-mode "0.3") (ob-sagemath "0.4"))
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
(require 'calc)
(require 'calc-ext)
(require 'calccomp)

(defgroup symtex nil
  "Symtex: A package for parsing and evaluating LaTeX math expressions with SageMath."
  :prefix "symtex-"
  :group 'applications)

(defcustom symtex-log-dir
  "~/symtex-log/"
  "Directory for logging the source blocks used by symtex."
  :type 'string)

(defun symtex--log (block)
  "Log the org source BLOCK."
  (let* ((date-str (format-time-string "%Y-%m-%d"))
         (org-file-path (expand-file-name (concat "symtex-" date-str ".org")
                                          symtex-log-dir ))
         (org-buffer (find-file-noselect org-file-path))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-current-buffer org-buffer
      (goto-char (point-max))
      (org-insert-heading t nil t)
      (insert (format "%s\n" timestamp))
      (insert block)
      (save-buffer))))

(defcustom symtex-spec
  '(:preprocess symtex--parse-latex-for-sage
                :block-format "#+begin_src sage :results silent\n%s\n#+end_src"
                :header nil
                :input "expr = %s"
                :op "result_expr = %s"
                :output "result_str = latex(result_expr)
result_str")
  "Specification for how to operate on TeX expressions."
  :type 'plist)

(defun symtex--code (op input spec)
  (let ((preprocess (plist-get spec :preprocess))
        (header (plist-get spec :header))
        (input-format (plist-get spec :input))
        (op-format (plist-get spec :op))
        (output-format (plist-get spec :output)))
    (setq input (when input (funcall preprocess input)))
    (mapconcat #'identity
               (delq nil (list header
                               (when input (format input-format input))
                               (format op-format op)
                               output-format))
               "\n")))

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
    (buffer-substring-no-properties (point-min)
                                    (point-max))))

;;;###autoload
(defun symtex-process (op &optional input)
  "Evaluate code OP using TeX code INPUT.
If INPUT is non-nil, then it is parsed, converted to a sage
object, and stored in the sage variable `expr'.  The code OP is
evaluated.  Its result is stored in the `kill-ring'."
  (interactive "sExpression to evaluate:")
  (let ((block (format (plist-get symtex-spec :block-format)
                       (symtex--code op input symtex-spec))))
    (symtex--log block)
    (let ((result (with-temp-buffer
                    (insert block)
                    (goto-char (point-min))
                    (let ((python-indent-guess-indent-offset-verbose nil)
                          (inhibit-message t))
                      (symtex--tidy
                       (org-babel-execute-src-block))))))
      (kill-new result)
      (message "Result saved to kill-ring: %s" result))))

(defun symtex--read-evaluate-region (beg end &optional op)
  "Evaluate SAGE expression involving TeX region (BEG . END).
If OP is nil, then we prompt for it from the minibuffer.  The
result is stored in the kill ring; see the documentation for
`symtex-process'."
  (interactive "r")
  (unless op
    (setq op
          (read-string "Expression to evaluate (use 'expr' for input expression):")))
  (let ((input (buffer-substring-no-properties beg end)))
    (symtex-process op input)))

(defcustom symtex-expand-expression
  "expr.expand()*2/2"
  "Sage expression for expansion."
  :type 'string)

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

;;;###autoload
(defmacro symtex-with-calc-language (lang &rest body)
  "Execute the forms in BODY with `calc-language` set to LANG.
The value of `calc-language` is restored after BODY has been processed."
  `(let ((old-lang calc-language))
     (unwind-protect
         (progn
           (save-excursion
             (calc-create-buffer))
                                        ; this only needs to be called once,
                                        ; but couldn't think of a more robust
                                        ; place to put it
           (calc-set-language ,lang)
           ,@body)
       (calc-set-language old-lang))))

(defun symtex--parse-latex-for-sage (latex-expr)
  "Parse LATEX-EXPR."
  (let* ((parsed (symtex-with-calc-language 'latex
                                            (math-read-expr latex-expr)))
         (composed (symtex-with-calc-language 'sage
                                              (math-compose-expr parsed 0)))
         (postprocessor
          (lambda (item self)
            (cond ((listp item)
                   (mapconcat (lambda (subitem)
                                (funcall self subitem self))
                              item))
                  ((stringp item)
                   item))))
         (postprocessed
          (funcall postprocessor composed postprocessor)))
    postprocessed))

;; everything from here on is a mild tweaking of stuff from Emacs
;; calc, including a couple pending bug fixes
;; (https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-01/msg01776.html,
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-01/msg01777.html)

(defvar math-comp-just)
(defvar math-comp-comma-spc)
(defvar math-comp-vector-prec)
(defvar math-comp-left-bracket)
(defvar math-comp-right-bracket)
(defvar math-comp-comma)

(defvar calc-alg-exp)
(defvar calc-buffer)
(defvar calc-digit-value)
(defvar math-exp-pos)
(defvar math-exp-str)
(defvar math-exp-old-pos)
(defvar math-exp-token)
(defvar math-exp-keep-spaces)
(defvar math-expr-data)
(defvar calc-lang-slash-idiv)
(defvar calc-lang-allow-underscores)
(defvar calc-lang-allow-percentsigns)
(defvar math-exp-str) ;; Dyn scoped


(defun symtex--fix-math-read-factor ()
  (let ((math-expr-opers (math-expr-ops))
        op)
    (cond ((eq math-exp-token 'number)
	   (let ((num (math-read-number math-expr-data)))
	     (if (not num)
		 (progn
		   (setq math-exp-old-pos math-exp-pos)
		   (throw 'syntax "Bad format")))
	     (math-read-token)
	     (if (and math-read-expr-quotes
		      (consp num))
		 (list 'quote num)
	       num)))
	  ((and calc-user-parse-table
		(setq op (calc-check-user-syntax)))
	   op)
	  ((or (equal math-expr-data "-")
	       (equal math-expr-data "+")
	       (equal math-expr-data "!")
	       (equal math-expr-data "|")
	       (equal math-expr-data "/"))
	   (setq math-expr-data (concat "u" math-expr-data))
	   (math-read-factor))
	  ((and (setq op (assoc math-expr-data math-expr-opers))
		(eq (nth 2 op) -1))
	   (if (consp (nth 1 op))
	       (funcall (car (nth 1 op)) op)
	     (math-read-token)
	     (let ((val (math-read-expr-level (nth 3 op))))
	       (cond ((eq (nth 1 op) 'ident)
		      val)
		     ((and (Math-numberp val)
			   (equal (car op) "u-"))
		      (math-neg val))
		     (t (list (nth 1 op) val))))))
	  ((eq math-exp-token 'symbol)
	   (let ((sym (intern math-expr-data)))
	     (math-read-token)
	     (if (equal math-expr-data calc-function-open)
		 (let ((f (assq sym math-expr-function-mapping)))
		   (math-read-token)
		   (if (consp (cdr f))
		       (funcall (car (cdr f)) f sym)
		     (let ((args (if (or (equal math-expr-data calc-function-close)
					 (eq math-exp-token 'end))
				     nil
				   (math-read-expr-list))))
		       (if (not (or (equal math-expr-data calc-function-close)
				    (eq math-exp-token 'end)))
			   (throw 'syntax "Expected `)'"))
		       (math-read-token)
		       (if (and (memq calc-language
                                      calc-lang-parens-are-subscripts)
                                args
				(require 'calc-ext)
				(let ((calc-matrix-mode 'scalar))
				  (math-known-matrixp
				   (list 'var sym
					 (intern
					  (concat "var-"
						  (symbol-name sym)))))))
			   (math-parse-fortran-subscr sym args)
			 (if f
			     (setq sym (cdr f))
			   (and (= (aref (symbol-name sym) 0) ?\\)
				(< (prefix-numeric-value calc-language-option)
				   0)
				(setq sym (intern (substring (symbol-name sym)
							     1))))
			   (or (string-search "-" (symbol-name sym))
			       (setq sym (intern
					  (concat "calcFunc-"
						  (symbol-name sym))))))
			 (cons sym args)))))
	       (if math-read-expr-quotes
		   sym
		 (let ((val (list 'var
				  (intern (math-remove-dashes
					   (symbol-name sym)))
				  (if (string-search "-" (symbol-name sym))
				      sym
				    (intern (concat "var-"
						    (symbol-name sym)))))))
		   (let ((v (or
                             (assq (nth 1 val) math-expr-variable-mapping)
                             (assq (math-restore-placeholders (nth 1 val))
                                   math-expr-variable-mapping))))
		     (and v (setq val (if (consp (cdr v))
					  (funcall (car (cdr v)) v val)
					(list 'var
					      (intern
					       (substring (symbol-name (cdr v))
							  4))
					      (cdr v))))))
		   (while (and (memq calc-language
                                     calc-lang-brackets-are-subscripts)
			       (equal math-expr-data "["))
		     (math-read-token)
                     (let ((el (math-read-expr-list)))
                       (while el
                         (setq val (append (list 'calcFunc-subscr val)
                                           (list (car el))))
                         (setq el (cdr el))))
		     (if (equal math-expr-data "]")
			 (math-read-token)
		       (throw 'syntax "Expected `]'")))
		   val)))))
	  ((eq math-exp-token 'dollar)
	   (let ((abs (if (> math-expr-data 0) math-expr-data (- math-expr-data))))
	     (if (>= (length calc-dollar-values) abs)
		 (let ((num math-expr-data))
		   (math-read-token)
		   (setq calc-dollar-used (max calc-dollar-used num))
		   (math-check-complete (nth (1- abs) calc-dollar-values)))
	       (throw 'syntax (if calc-dollar-values
				  "Too many $'s"
				"$'s not allowed in this context")))))
	  ((eq math-exp-token 'hash)
	   (or calc-hashes-used
	       (throw 'syntax "#'s not allowed in this context"))
	   (require 'calc-ext)
	   (if (<= math-expr-data (length calc-arg-values))
	       (let ((num math-expr-data))
		 (math-read-token)
		 (setq calc-hashes-used (max calc-hashes-used num))
		 (nth (1- num) calc-arg-values))
	     (throw 'syntax "Too many # arguments")))
	  ((equal math-expr-data "(")
	   (let* ((exp (let ((math-exp-keep-spaces nil))
			 (math-read-token)
			 (if (or (equal math-expr-data "\\dots")
				 (equal math-expr-data "\\ldots"))
			     '(neg (var inf var-inf))
			   (math-read-expr-level 0)))))
	     (let ((math-exp-keep-spaces nil))
	       (cond
		((equal math-expr-data ",")
		 (progn
		   (math-read-token)
		   (let ((exp2 (math-read-expr-level 0)))
		     (setq exp
			   (if (and exp2 (Math-realp exp) (Math-realp exp2))
			       (math-normalize (list 'cplx exp exp2))
			     (list '+ exp (list '* exp2 '(var i var-i))))))))
		((equal math-expr-data ";")
		 (progn
		   (math-read-token)
		   (let ((exp2 (math-read-expr-level 0)))
		     (setq exp (if (and exp2 (Math-realp exp)
					(Math-anglep exp2))
				   (math-normalize (list 'polar exp exp2))
				 (require 'calc-ext)
				 (list '* exp
				       (list 'calcFunc-exp
					     (list '*
						   (math-to-radians-2 exp2)
						   '(var i var-i)))))))))
		((or (equal math-expr-data "\\dots")
		     (equal math-expr-data "\\ldots"))
		 (progn
		   (math-read-token)
		   (let ((exp2 (if (or (equal math-expr-data ")")
				       (equal math-expr-data "]")
				       (eq math-exp-token 'end))
				   '(var inf var-inf)
				 (math-read-expr-level 0))))
		     (setq exp
			   (list 'intv
				 (if (equal math-expr-data ")") 0 1)
				 exp
				 exp2)))))))
	     (if (not (or (equal math-expr-data ")")
			  (and (equal math-expr-data "]") (eq (car-safe exp) 'intv))
			  (eq math-exp-token 'end)))
		 (throw 'syntax "Expected `)'"))
	     (math-read-token)
	     exp))
	  ((eq math-exp-token 'string)
	   (require 'calc-ext)
	   (math-read-string))
	  ((equal math-expr-data "[")
	   (require 'calc-ext)
	   (let ((space-sep
                  ;; Allow spaces as separators when the vector is
                  ;; specified using "[", but not when it is specified
                  ;; using language-specific constructions such as
                  ;; "\\begin{pmatrix}".
                  (equal "[" (substring math-exp-str math-exp-old-pos
                                        math-exp-pos))))
             (math-read-brackets space-sep "]")))
	  ((equal math-expr-data "{")
	   (require 'calc-ext)
	   (math-read-brackets nil "}"))
	  ((equal math-expr-data "<")
	   (require 'calc-ext)
	   (math-read-angle-brackets))
	  (t (throw 'syntax "Expected a number")))))

(advice-add 'math-read-factor :override #'symtex--fix-math-read-factor)


(defun symtex--fix-math-read-vector ()
  (let* ((val (list (math-read-expr-level 0)))
	 (last val))
    (while (progn
	     (while (eq math-exp-token 'space)
	       (math-read-token))
	     (and (not (eq math-exp-token 'end))
		  (not (equal math-expr-data ";"))
		  (not (equal math-expr-data math-rb-close))
		  (not (equal math-expr-data "\\dots"))
		  (not (equal math-expr-data "\\ldots"))))
      (if (equal math-expr-data ",")
	  (math-read-token))
      (while (eq math-exp-token 'space)
	(math-read-token))
      (when (not (equal math-expr-data math-rb-close))
        (let ((rest (list (math-read-expr-level 0))))
	  (setcdr last rest)
	  (setq last rest))))
    (cons 'vec val)))

(advice-add 'math-read-vector :override #'symtex--fix-math-read-vector)

(defun symtex--fix-math-read-matrix (mat)
  (while (equal math-expr-data ";")
    (math-read-token)
    (while (eq math-exp-token 'space)
      (math-read-token))
    (when (not (equal math-expr-data math-rb-close))
      (setq mat (nconc mat (list (math-read-vector))))))
  mat)

(advice-add 'math-read-matrix :override #'symtex--fix-math-read-matrix)

(defun symtex--math-compose-subscr (a)
  (if (eq (car (nth 1 a))
          'var)
      (let* ((var (cadr (nth 1 a)))
             (sub (nth 2 a))
             (var-str
              (concat
               (symbol-name var)
               "_"
               (number-to-string sub))))
        (list 'horiz
              (format "var(\"%s\")"
                      var-str)))
    (list 'horiz

          (math-compose-expr (nth 1 a)
                             1000)
          "_"
          (math-compose-expr (nth 2 a)
                             0)
          "")))

(put 'sage 'math-compose-subscr #'symtex--math-compose-subscr)

(defun symtex--math-matrix-formatter (a)
  (list 'horiz
        "matrix("
        math-comp-left-bracket
        (math-compose-vector (cdr a)
                             (concat math-comp-comma " ")
                             math-comp-vector-prec)
        math-comp-right-bracket
        ")"))

(put 'sage 'math-matrix-formatter #'symtex--math-matrix-formatter)

(defun symtex--math-compose-var (a _prec)
  (let ((sn (nth 1 a)))
    (format "var(\"%s\")"
            sn)))

(put 'sage 'math-var-formatter #'symtex--math-compose-var)

;; can add to the below as needed

(put 'sage 'math-oper-table
  '( ( "matrix" ident	     -1  300 )
     ( "!"     calcFunc-fact  210 -1 )
     ( "^"     ^	     201 200 )
     ( "**"    ^	     201 200 )
     ( "u+"    ident	     -1  197 )
     ( "u-"    neg	     -1  197 )
     ( "/"     /	     191 192 )
     ( "*"     *	     191 192 )
     ( "+"     +	     180 181 )
     ( "-"     -	     180 181 )
     ( "%"   %	     170 170 )
     ( "<"     calcFunc-lt   160 160 )
     ( ">"     calcFunc-gt   160 160 )
     ( "<="    calcFunc-leq  160 160 )
     ( ">="    calcFunc-geq  160 160 )
     ( "="     calcFunc-eq   160 160 )
     ( "!="    calcFunc-neq  160 160 )
     ( "not"   calcFunc-lnot -1  121 )
     ( "and"   calcFunc-land 110 111 )
     ( "or"    calcFunc-lor  100 101 )))

(provide 'symtex)
;;; symtex.el ends here
