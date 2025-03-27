test_that("find_sequence works", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_snapshot(find_sequence(table, "&21&6"))
  table <- prepare_table(table, do_cells = FALSE)
  expect_snapshot(find_sequence(table, "&21&6"))
})
