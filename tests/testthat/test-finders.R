test_that("finders work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  expect_equal(find_whitespace(table, all = FALSE), 5)
  expect_equal(find_env(parsed), 2)
  expect_equal(find_macro(table, "\\\\", all = FALSE), 16)
  expect_equal(find_catcode(table, ALIGN, all = FALSE), 9)
  expect_equal(find_tags(table, "TEXT", all = FALSE), 2)
  expect_equal(find_char(table, "6", all = FALSE), 32)
  expect_equal(find_block(table), 4)
  expect_equal(path_to(parsed, is_block), c(2, 4))
  expect_equal(as.character(get_item(table, index = 32)), "6")
  expect_equal(as.character(get_items(table, indices = c(9, 32))),
                            c("&", "6"))
  expect_snapshot(get_container(table, find_char(table, "6", all = FALSE, path = TRUE)))
  expect_snapshot(find_pattern(parsed, "RX4 Wag", fixed = TRUE))
  table <- prepare_table(table)
  expect_equal(find_whitespace(table, all = FALSE), 5)
  expect_equal(find_env(parsed), 2)
  expect_equal(find_macro(table, "\\\\", all = FALSE), 16)
  expect_equal(find_catcode(table, ALIGN, all = FALSE), 9)
  expect_equal(find_tags(table, "TEXT", all = FALSE), 2)
  expect_equal(find_char(table, "6", all = FALSE), 32)
  expect_equal(find_block(table), 4)
  expect_equal(path_to(parsed, is_block), c(2, 4))
  expect_equal(as.character(get_item(table, index = 32)), "6")
  expect_equal(as.character(get_items(table, indices = c(9, 32))),
               c("&", "6"))
  expect_snapshot(get_container(table, find_char(table, "6", all = FALSE, path = TRUE)))
  expect_snapshot(find_pattern(parsed, "RX4 Wag", fixed = TRUE))
})

test_that("setters work", {
  latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
  parsed <- parseLatex(latex)
  table <- parsed[[find_tabular(parsed)]]
  table1 <- set_item(table, find_char(table, "6", path = TRUE)[[2]], new_block("6"))
  expect_snapshot(table1)
  table1 <- insert_values(table, find_char(table, "6", path = TRUE)[[2]],
                          new_block("6"))
  expect_snapshot(table1)
  parsed1 <- set_range(parsed, find_pattern(parsed, "RX4 Wag", fixed = TRUE), "RX4 Wagon")
  expect_snapshot(parsed1)

  table <- prepare_table(table)
  table1 <- set_item(table, find_char(table, "6", path = TRUE)[[2]], new_block("6"))
  expect_snapshot(table1)
  table1 <- insert_values(table, find_char(table, "6", path = TRUE)[[2]],
                          new_block("6"))
  expect_snapshot(table1)
  parsed1 <- set_range(parsed, find_pattern(parsed, "RX4 Wag", fixed = TRUE), "RX4 Wagon")
  expect_snapshot(parsed1)
})
