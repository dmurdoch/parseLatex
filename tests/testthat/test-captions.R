test_that("captions functions work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "the caption")
  parsed <- parseLatex(latex)
  path <- path_to(parsed, is_env, envtypes = "table", all = FALSE)
  table <- parsed[[path]]

  expect_snapshot(find_caption(table))
  expect_snapshot(path_to_caption(parsed))

  table <- prepare_table(table)

  expect_snapshot(find_caption(table))
  parsed[[path]] <- table
  expect_snapshot(path_to_caption(parsed))
})
