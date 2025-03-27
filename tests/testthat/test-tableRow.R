test_that("tableRow works", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_equal(tableNrow(table), 3)
  expect_equal(find_tableRow(table, 1),
               LaTeX2range(NULL, 8:16))
  table <- prepare_table(table)
  expect_equal(tableNrow(table), 3)
  expect_equal(find_tableRow(table, 1),
               LaTeX2range(2, 2:4))
  expect_output(print(tableRow(table, 2)),
                "Mazda RX4 & 21 & 6\\\\")
  tableRow(table, 5) <- "1 & 2 & 3"
  expect_output(print(tableRow(table, 5)),
                "1 & 2 & 3\\\\")
  tableRow(table, 5) <- tableRow(table, 2)
  expect_output(print(tableRow(table, 5)),
                "Mazda RX4 & 21 & 6\\\\")
  expect_equal(tableNrow(table), 5)
})
