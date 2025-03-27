test_that("tableCell fns work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_error(find_tableCell(table, 2, 2))
  table <- prepare_table(table)
  expect_output(print(tableCell(table, 2,2)),
                " 21 &")
  tableCell(table, 2, 2) <- "22 &"
  expect_output(print(tableCell(table, 2,2)),
                " 22 &")
  tableCell(table, 2, 2) <- "23"
  expect_output(print(tableCell(table, 2,2)),
                " 23 &")
  tableRow(table, 2) <- "\\multicolumn{2}{c}{Multi}\\\\"
  expect_output(print(tableCell(table, 2, 1)),
                "\\multicolumn{2}{c}{Multi}\\\\", fixed = TRUE)

})
