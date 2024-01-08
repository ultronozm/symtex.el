#!/usr/bin/python3

# symtex.py - Convert LaTeX <-> SymPy.
# Copyright (C) 2023-2024 Soumendra Ganguly

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import sys
from sympy import Matrix
from sympy.parsing.latex import parse_latex
from sympy.parsing.sympy_parser import parse_expr
from sympy.printing.latex import latex
from sympy import UnevaluatedExpr
from re import sub as re_sub

MATRIX_DELIM = {r"\\left\(\\begin{matrix}": r"\\end{matrix}\\right\)",
                r"\\left\[\\begin{matrix}": r"\\end{matrix}\\right\]",
                r"\\begin{pmatrix}": r"\\end{pmatrix}",
                r"\\begin{bmatrix}": r"\\end{bmatrix}",
                r"\\left\(\\begin{array}{.*?}": r"\\end{array}\\right\)",
                r"\\left\[\\begin{array}{.*?}": r"\\end{array}\\right\]"
                }

def parse_matrix(matrix_str):
    """Construct a SymPy Matrix from a LaTeX matrix string."""
    matrix_str = matrix_str.replace("\n", "")
    mat = []
    for row_str in matrix_str.split("\\\\"):
        row = row_str.split("&")
        if row != [""]:
            parsed_row = [parse_latex(x) for x in row]
            mat.append(parsed_row)
    return UnevaluatedExpr(Matrix(mat))

def uniformize_matrix_delimiters(latex_str, uniform_begin, uniform_end):
    """Replace all matrix begin, end delimiters with specific ones."""
    for begin_delim in MATRIX_DELIM:
        end_delim = MATRIX_DELIM[begin_delim]
        latex_str = re_sub(begin_delim, uniform_begin, latex_str)
        latex_str = re_sub(end_delim, uniform_end, latex_str)
    return latex_str

def symtex_parse_latex(latex_str):
    """Add matrix parsing to SymPy's parse_latex."""
    original_latex_str = latex_str
    new_matrix_name = "a"
    while new_matrix_name in original_latex_str:
        new_matrix_name = f"{new_matrix_name}a"

    uniform_begin = "<matrixbegin>"
    while uniform_begin in original_latex_str:
        uniform_begin = f"<{uniform_begin}>"

    uniform_end = "<matrixend>"
    while uniform_end in original_latex_str:
        uniform_end = f"<{uniform_end}>"

    latex_str = uniformize_matrix_delimiters(latex_str,
                                             uniform_begin,
                                             uniform_end)

    matrices = []
    while True:
        b = latex_str.find(uniform_begin)
        e = latex_str.find(uniform_end)
        if b == -1 and e == -1:
            # all matrices have been parsed

            # We could have created a dictionary with keys
            # "mat_var_name" mapped to values "mat" instead of storing
            # pairs (mat_var_name, mat) in a list; then we could have
            # performed parse_latex(latex_str) followed by
            # .subs(matrix_dict). However, parse_latex and parse_expr
            # do not know that these variables are matrcies and they
            # sort the variables based on alphabetical order of
            # variable names, breaking matrix multiplication.
            # Therefore, we perform a string replacement instead of
            # following the .subs approach.
            sympy_str = str(parse_latex(latex_str))
            while matrices != []:
                mat_var_name, mat = matrices.pop()
                sympy_str = sympy_str.replace(mat_var_name,
                                              str(mat))

            return parse_expr(sympy_str)
        if (b != -1 and e == -1) or (b == -1 and e != -1):
            print("Mismatched matrix delimiters.", file=sys.stderr)
            sys.exit(1)

        b2 = b + len(uniform_begin)
        e2 = e + len(uniform_end)

        before_matrix = latex_str[:b]
        matrix_content = latex_str[b2:e]
        after_matrix = latex_str[e2:]

        matrices.append((new_matrix_name, parse_matrix(matrix_content)))
        latex_str = f"{before_matrix}\\{new_matrix_name}{after_matrix}"

        new_matrix_name = f"{new_matrix_name}a"

def symtex_get_latex(sympy_expr):
    latex_str = latex(sympy_expr,
                      full_prec=True,
                      fold_frac_powers=False,
                      fold_func_brackets=False,
                      fold_short_frac=None,
                      inv_trig_style="abbreviated",
                      itex=False,
                      ln_notation=False,
                      long_frac_ratio=None,
                      mat_delim="(",
                      mat_str=None,
                      mode="plain",
                      mul_symbol=None,
                      order=None,
                      root_notation=True,
                      mat_symbol_style="plain",
                      imaginary_unit="i",
                      gothic_re_im=False,
                      decimal_separator="period",
                      perm_cyclic=True,
                      parenthesize_super=True,
                      min=None,
                      max=None,
                      diff_operator="d")

    return uniformize_matrix_delimiters(latex_str,
                                        r"\\begin{pmatrix}",
                                        r"\\end{pmatrix}")

# Tests:
if __name__ == "__main__":
    if len(sys.argv) == 1:
        test_latex_str = sys.stdin.read()
    else:
        test_latex_str = sys.argv[1]
    print(symtex_get_latex(symtex_parse_latex(test_latex_str).doit()))
