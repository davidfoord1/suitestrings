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
#' strings <- c("cherry", "apple", "banana")
#'
#' chr_sort(strings)
#' [1] "apple"  "banana" "cherry"
#' chr_order(strings)
#' #> [1] 2 3 1
#' chr_rank(strings)
#' #> [1] 3 1 2
#' chr_sort(strings, decreasing = TRUE)
#' #> [1] "cherry" "banana" "apple"
#'
#' strings[chr_order(strings)]
#' [1] "apple"  "banana" "cherry"
#'
#' @rdname chr_sort
#' @export
chr_sort <- function(strings, decreasing = FALSE) {
  sort(strings, decreasing = decreasing)
}

#' @rdname chr_sort
#' @export
chr_order <- function(strings, decreasing = FALSE) {
  order(strings, decreasing = decreasing)
}

#' @rdname chr_sort
#' @export
chr_rank <- function(strings, decreasing = FALSE) {
  rank(strings, decreasing = decreasing)
}

chr_unique <- function(strings) {
  unique(strings)
}
