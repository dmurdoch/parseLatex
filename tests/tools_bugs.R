# parseLatex bug fixes.  These are all bugs in
# tools::parseLatex in R 4.4.2, but not in this package

library(parseLatex)

# Uncomment this to see the issues
# parseLatex <- tools::parseLatex

# 1. Syntax error in tools
parseLatex("\\begin{document} \\end{document} \\begin{foo}")


# 2. Syntax error in tools
parseLatex("\\begin{foo}\\end{foo}")

# 3. Accepted without error by tools -- we want an error
stopifnot(inherits(
  try(parseLatex("\\begin{foo} abc \\end{bar}")),
  "try-error"))

# 4. Syntax error in tools
parseLatex("\\Sexpr{ 1 + {1} }")

if (! identical(parseLatex, tools::parseLatex)) {
  # 5. causes crash in tools.  We just want an error.
  stopifnot(inherits(
    try(parseLatex("{")),
    "try-error"))
}

# 6. Syntax errors in tools
parseLatex("\\newenvironment{bea}{\\begin{eqnarray*}}{\\end{eqnarray*}}")
parseLatex("\\newcommand{\\bea}{\\begin{eqnarray*}}")
parseLatex("\\def\\bea{\\begin{eqnarray*}}")


