#' Detect the presence of a pattern in a string
#'
#' @description
#' These functions search for patterns in strings
#'
#' @param string A character vector of strings to search in.
#' @param pattern The pattern to look for.
#'
#' @return A logical vector indicating the presence of each pattern in a string.
#' @export
#' @rdname str_detect
#'
#' @examples
#' str_detect_match(c("apple", "banana", "cherry"), "a")
#' #> [1]  TRUE  TRUE FALSE
str_detect_match <- function(string, pattern) {
  grepl(pattern, string, perl = TRUE)
}


