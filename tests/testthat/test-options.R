test_that("options fns work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_equal(find_bracket_options(table), LaTeX2range(NULL, 1:3))
  expect_output(print(bracket_options(table)), "[t]")
  bracket_options(table) <- "[H]"
  expect_output(print(bracket_options(table)), "[H]")
  find_brace_options(table)
  expect_equal(find_brace_options(table), 4)
  brace_options(table)
  expect_output(print(brace_options(table)), "{l|r|r}", fixed = TRUE)
  brace_options(table) <- "l|ll"
  expect_output(print(brace_options(table)), "{l|ll}", fixed = TRUE)

  table <- parsed[[find_tabular(parsed)]]
  table <- prepare_table(table)
  expect_equal(find_bracket_options(table), LaTeX2range(1, 1:3))
  expect_output(print(bracket_options(table)), "[t]")
  bracket_options(table) <- "[H]"
  expect_output(print(bracket_options(table)), "[H]")
  expect_equal(find_brace_options(table[[1]]), 4)
  expect_output(print(brace_options(table[[1]])), "{l|r|r}", fixed = TRUE)
  brace_options(table[[1]]) <- "l|ll"
  expect_output(print(brace_options(table[[1]])), "{l|ll}", fixed = TRUE)
})
