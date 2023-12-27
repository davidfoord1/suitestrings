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

#' Find matching indices
#'
#' @param string A character vector of strings to search in.
#' @param pattern The pattern to look for.
#'
#' @return An integer vector of matching indices in string.
#' @export
#'
#' @examples
#' str_which(letters, "[aeiou]")
#' #> [1]  1  5  9 15 21
str_which <- function(string, pattern) {
  grep(pattern, string)
}



