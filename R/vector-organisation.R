#' Re-organise a character vector
#'
#' @description
#' Sort, order and rank strings in a character vector alphabetically.
#'
#' `chr_sort()` returns a sorted vector.
#'
#' `chr_order()` returns the desired order when used for sub-setting i.e.
#' `x[chr_order(x)]` is the same as `chr_sort(x)`. This is particularly useful
#' for sorting many vectors in the same way.
#'
#' `chr_rank()` returns the rank of the strings; rank 1 is given to the string
#' that would be first in the sorted list.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param decreasing
#' Logical. If `FALSE` (the default) strings will be ordered alphabetically
#' from low to high i.e. A to Z; if `TRUE` sort from high to low or Z to A.
#'
#' @return
#' `chr_sort()`: A sorted character vector the same length as `strings`.
#'
#' `chr_order()`: An integer vector the same length as `strings`.
#'
#' `chr_rank()`: An integer vector the same length as `strings`.
#'
#' @seealso
#' [sort()], [order()] and [rank()] which these functions are wrappers around.
#'
#' @examples
#' chr_sort(c("c", "a", "b"))
#' #> [1] "a" "b" "c"
#' chr_order(c("c", "a", "b"))
#' #> [1] 2 3 1
#' chr_rank(c("c", "a", "b"))
#' #> [1] 3 1 2
#'
#' @rdname chr_sort
#' @export
chr_sort <- function(strings) {
  sort(strings)
}

#' @rdname chr_sort
#' @export
chr_order <- function(strings) {
  order(strings)
}

#' @rdname chr_sort
#' @export
chr_rank <- function(strings) {
  rank(strings)
}

chr_unique <- function(strings) {
  unique(strings)
}
