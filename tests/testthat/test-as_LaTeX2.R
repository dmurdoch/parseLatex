test_that("as_LaTeX2 works", {
  expect_identical(as_LaTeX2("\\emph{text}"),
                   parseLatex("\\emph{text}"))
  expect_identical(rmSrcrefs(latex2("\\emph", "{text}")),
                   rmSrcrefs(parseLatex("\\emph{text}")))
})
