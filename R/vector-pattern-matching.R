#' Find matching indices in character vector
#'
#' @param string A character vector of strings to search in.
#' @param pattern The pattern to look for.
#'
#' @return An integer vector of matching indices in string.
#' @export
#'
#' @examples
#' chr_which(letters, "[aeiou]")
#' #> [1]  1  5  9 15 21
chr_which <- function(string, pattern) {
  grep(pattern, string)
}

#' Count matching indices in character vector
#'
#' @param string A character vector of strings to search in.
#' @param pattern The pattern to look for.
#'
#' @return A single integer representing the number of matches found
#' @export
#'
#' @examples
#' chr_count_matches(letters, "[aeiou]")
#' #> [1] 5
chr_count_matches <- function(string, pattern) {
  length(grep(pattern, string))
}


# chr_split -----------------------------------------------------

chr_split_all <- function(string, pattern) {
  unlist(strsplit(string, pattern, perl = TRUE))
}

