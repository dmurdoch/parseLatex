test_that("table options work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_identical(find_posOption(table), LaTeX2range(NULL, 1:3))
  expect_snapshot(posOption(table))
  posOption(table) <- "[H]"
  expect_snapshot(table)
  expect_null(find_widthOption(table))
  expect_warning(widthOption(table) <- "3in")
  expect_identical(find_columnOptions(table), LaTeX2range(NULL, 4L))
  expect_output(print(columnOptions(table)),
                "{l|r|r}", fixed = TRUE)
  expect_output(print(columnOption(table, 2)), "r")
  table <- prepare_table(table)
  expect_identical(find_posOption(table), LaTeX2range(1L, 1:3))
  expect_snapshot(posOption(table))
  posOption(table) <- "[H]"
  expect_snapshot(table)
  expect_null(find_widthOption(table))
  expect_warning(widthOption(table) <- "3in")
  expect_identical(find_columnOptions(table), LaTeX2range(1L, 4L))
  expect_output(print(columnOptions(table)),
                "{l|r|r}", fixed = TRUE)
  expect_output(print(columnOption(table, 2)), "r")
})
