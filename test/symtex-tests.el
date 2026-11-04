;;; symtex-tests.el --- Tests for symtex.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; This file is part of symtex.el.

;;; Commentary:

;; Unit and regression tests for SymTeX's parsing and comint protocol.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'symtex)

(ert-deftest symtex-test-code-adds-result-markers ()
  (should
   (equal
    (symtex--code "factor(x)" nil '("BEGIN" . "END"))
    (concat "result_expr = factor(x);result_str = latex(result_expr);"
            "print(\"BEGIN\", result_str, \"END\", sep=\"\")"))))

(ert-deftest symtex-test-tidy-normalizes-comint-newlines ()
  (should
   (equal
    (symtex--tidy
     (concat "\\left(\\begin{array}{rr}1 & 2 \\\\\r\n"
             "3 & 4\\end{array}\\right)\r\n"))
    (concat "\\begin{pmatrix}1 & 2 \\\\\n"
            "3 & 4\\end{pmatrix}"))))

(ert-deftest symtex-test-result-markers-must-delimit-output-line ()
  (let ((markers '("BEGIN" . "END")))
    (should
     (equal (symtex--result-from-output
             "BEGINx^2 - 1END\r\n" markers)
            "x^2 - 1"))
    ;; A Python traceback quotes the submitted source line.  Markers in
    ;; that quoted line must not turn the traceback into a false result.
    (should-error
     (symtex--result-from-output
      (concat "  Cell In[1], line 1\r\n"
              "    print(\"BEGIN\", result_str, \"END\")\r\n"
              "SyntaxError: invalid syntax\r\n")
      markers)
     :type 'user-error)))

(ert-deftest symtex-test-process-does-not-kill-new-sage-errors ()
  (let ((kill-ring '("sentinel")))
    (cl-letf (((symbol-function 'symtex--result-markers)
               (lambda () '("BEGIN" . "END")))
              ((symbol-function 'symtex--process-buffer) #'ignore)
              ((symbol-function 'symtex--redirect-output)
               (lambda (_buffer _code)
                 "SyntaxError: invalid syntax\r\n")))
      (should-error (symtex-process "not valid Sage(") :type 'user-error)
      (should (equal (car kill-ring) "sentinel")))))

(ert-deftest symtex-test-process-buffer-falls-back-to-sole-sage-buffer ()
  (let ((preferred-name " *symtex-test-preferred*")
        (fallback-name " *symtex-test-python*"))
    (unwind-protect
        (let ((fallback (get-buffer-create fallback-name))
              (symtex-process-buffer preferred-name))
          (get-buffer-create preferred-name)
          (cl-letf (((symbol-function 'symtex--sage-process-buffer-p)
                     (lambda (buffer) (eq buffer fallback))))
            (should (eq (symtex--process-buffer) fallback))))
      (kill-buffer preferred-name)
      (kill-buffer fallback-name))))

(ert-deftest symtex-test-parse-latex-for-sage ()
  (let ((symtex-variable-policy 'symbolic))
    (should (equal (symtex--parse-latex-for-sage "x (y+1)")
                   "var(\"x\")*(var(\"y\") + 1)")))
  (should
   (equal
    (symtex--parse-latex-for-sage
     "\\begin{pmatrix}1&2\\\\3&4\\end{pmatrix}")
    "matrix([[1, 2], [3, 4]])"))
  (should-error
   (symtex--parse-latex-for-sage "\\begin{pmatrix}")))

(ert-deftest symtex-test-does-not-globally-override-calc-readers ()
  (should-not
   (advice-member-p #'symtex--fix-math-read-factor 'math-read-factor))
  (should-not
   (advice-member-p #'symtex--fix-math-read-vector 'math-read-vector))
  (should-not
   (advice-member-p #'symtex--fix-math-read-matrix 'math-read-matrix)))

(ert-deftest symtex-test-variable-policies-in-sage ()
  "Check generated TeX expressions in a real Sage namespace."
  (skip-unless (executable-find "sage"))
  (let* ((symtex-variable-policy 'existing)
         (code
          (concat
           "K = CyclotomicField(21); u = K.gen(); "
           "omega = u**7; alpha = u**3 + u**18; "
           (symtex--code
            "expr**3 == 7*(2+3*omega**2)"
            "-\\alpha^2+\\alpha+2+\\omega(3-\\alpha-2\\alpha^2)")
           "; assert result_expr; assert omega == u**7; "
           "assert alpha == u**3 + u**18; "
           (symtex--code
            symtex-expand-expression
            "(2+\\alpha-\\alpha^2+\\omega(3-\\alpha-2\\alpha^2))(2+\\alpha-\\alpha^2+\\omega^{-1}(3-\\alpha-2\\alpha^2))")
           "; assert result_expr == 7; "
           (symtex--code symtex-expand-expression "(symtexmissing+1)^2")
           "; assert result_expr == symtexmissing**2+2*symtexmissing+1; "
           (symtex--code
            symtex-expand-expression
            "\\begin{pmatrix}1&1\\\\0&1\\end{pmatrix}^2")
           "; assert result_expr == matrix([[1,2],[0,1]]); "
           "x = 0; x_1 = 7; "
           (symtex--code "expr" "x+x_1")
           "; assert result_expr == 7; assert x == 0; "
           (symtex--code "expr" "symtexmissing+1")
           "; assert str(symtexmissing) == 'symtexmissing'; "
           "assert result_expr == symtexmissing+1; "
           "x = None; "
           (symtex--code "expr" "x")
           "; assert result_expr is None; assert x is None; "
           (let ((symtex-variable-policy 'symbolic))
             (symtex--code "expr" "x+x_1"))
           "; assert str(x) == 'x'; assert str(x_1) == 'x_1'")))
    (with-temp-buffer
      (let ((status (call-process "sage" nil t nil "-c" code)))
        (ert-info ((buffer-string))
          (should (equal status 0)))))))

(provide 'symtex-tests)
;;; symtex-tests.el ends here
