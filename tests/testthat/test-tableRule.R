test_that("tableRule works", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]

  expect_error(find_rules(table))

  table <- prepare_table(table)

  expect_snapshot(find_rules(table))
  expect_snapshot(find_rule(table,2))
  expect_output(print(rule(table, 2)), "\\hline")
})
