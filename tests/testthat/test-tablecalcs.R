test_that("table calculations are okay", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_identical(tableNrow(table), 3L)
  expect_identical(tableNcol(table), 3L)
  expect_identical(tableDim(table), c(3L,3L))
  table <- prepare_table(table)
  expect_identical(tableNrow(table), 3L)
  expect_identical(tableNcol(table), 3L)
  expect_identical(tableDim(table), c(3L,3L))
})
