test_that("parsing works", {
  parsed <- parseLatex("{abc}")
  expect_equal(latexTag(parsed[[1]]), "BLOCK")
  expect_equal(length(parsed), 1)

  parsed <- parseLatex("\\begin{center} abc \\end{center}")
  expect_true(is_env(parsed[[1]], "center"))
  expect_equal(length(parsed), 1)

  parsed <- parseLatex("\\texttt abc")
  expect_true(is_macro(parsed[[1]], "\\texttt"))
  expect_equal(c(parsed[[2]]), "abc")
  expect_equal(length(parsed), 2)

  parsed <- parseLatex("abc \\foo  % def")
  expect_equal(c(parsed[[1]]), "abc")
  expect_true(is_macro(parsed[[3]], "\\foo"))
  expect_equal(latexTag(parsed[[5]]), "COMMENT")
  expect_equal(length(parsed), 5)

  parsed <- parseLatex("\\verb!abc!\\Sexpr{x$1}")
  expect_equal(latexTag(parsed[[1]]), "VERB")
  expect_equal(latexTag(parsed[[2]]), "VERB")
  expect_equal(length(parsed), 2)

  parsed <- parseLatex("\\def\\x3\\def\\y = 4\\newcommand{\\z}{5}")
  expect_equal(latexTag(parsed[[1]]), "DEFINITION")
  expect_equal(latexTag(parsed[[2]]), "DEFINITION")
  expect_equal(latexTag(parsed[[3]]), "DEFINITION")
  expect_equal(length(parsed), 3)

  parsed <- parseLatex("$x + 3$ $$ x + 4 $$")
  expect_equal(latexTag(parsed[[1]]), "MATH")
  expect_equal(latexTag(parsed[[2]]), "SPECIAL")
  expect_equal(latexTag(parsed[[3]]), "DISPLAYMATH")
  expect_equal(length(parsed), 3)

})
