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
  (should (equal (symtex--parse-latex-for-sage "x (y+1)")
                 "var(\"x\")*(var(\"y\") + 1)"))
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

(provide 'symtex-tests)
;;; symtex-tests.el ends here
