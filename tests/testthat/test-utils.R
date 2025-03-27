test_that("utils work", {
  parsed <- parseLatex("a b c d ")
  expect_output(print(drop_items(parsed, c(1, 3))),
                "  c d ")
  expect_output(print(drop_items(parsed, LaTeX2range(NULL, c(1, 3)))),
                "  c d ")
  expect_output(print(drop_items(parsed, LaTeX2range(1, NULL))),
                " b c d ")
  expect_output(print(select_items(parsed, 1:3)), "a b")
  expect_output(print(trim_whitespace(parsed)), "a b c d")
  expect_equal(include_whitespace(parsed, 5), 5:6)
  expect_output(print(split_list(parsed, find_whitespace(parsed))[[c(3,1)]],
                      "c"))
  expect_output(print(split_latex(parsed, find_whitespace(parsed))),
                "abcd")
  expect_output(print(split_chars(latex2("abcd")[[1]], "c")[[1]]),
                "ab")
  expect_output(print(get_contents(new_block(parsed))), "a b c d")
  expect_output(print(set_contents(new_block(parsed), "e")), "BLOCK: {e}", fixed = TRUE)
  expect_output(print(new_env("env")), "ENVIRONMENT: \\begin{env}\\end{env}", fixed = TRUE)
  expect_output(print(new_itemlist("a")), "ITEMLIST: a")
  expect_output(print(flatten_itemlists(new_itemlist("a"))),
                "a")
  expect_output(print(placeholder()), "")
  expect_output(show_itemlists(new_itemlist("a")),
                "1: non-itemlist")
})
