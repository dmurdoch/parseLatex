test_that("get_leftovers works", {
  text <- "\\documentclass{article}\\begin{document}\\end{document}\nsome stuff after the document"
  expect_equal(unname(get_leftovers(text)),
               "some stuff after the document")

})
