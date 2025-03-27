test_that("table content fns work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_identical(find_tableContent(table), 5:55)
  expect_snapshot(table)
  tableContent(table) <- "Mazda RX4 & 21 & 6\\\\"
  expect_snapshot(table)
  table <- parsed[[find_tabular(parsed)]]
  table <- prepare_table(table)
  expect_error(find_tableContent(table))
  })
