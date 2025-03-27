test_that("names fns work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  expect_equal(latexTag(parsed[[c(2,11)]]), "TEXT")
  expect_equal(catcode(parsed[[1]]), 5L)
  expect_equal(envName(parsed[[2]]), "tabular")
  expect_equal(macroName(parsed[[c(2,18)]]), "\\hline")
  })
