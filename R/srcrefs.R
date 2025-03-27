#' Remove srcrefs
#'
#' @param items A [LaTeX2] object, or any other list of [LaTeX2item]s.
#'
#' @returns The `items` with source references removed.
#' @export
rmSrcrefs <- function(items) {
  attr(items, "srcref") <- NULL
  if (is.list(items)) {
    attrs <- attributes(items)
    items <- lapply(items, rmSrcrefs)
    attributes(items) <- attrs
  }
  items
}
