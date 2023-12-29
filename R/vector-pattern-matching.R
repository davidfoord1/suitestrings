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
  grep(pattern, string, perl = TRUE)
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
  length(grep(pattern, string, perl = TRUE))
}


# chr_extract ---------------------------------------------------

#' @rdname str_extract
#' @export
chr_extract_all <- function(string, pattern) {
  unlist(regmatches(string, gregexpr(pattern, string, perl = TRUE)))
}

# chr_split -----------------------------------------------------

#' @rdname str_split
#' @export
chr_split_all <- function(string, pattern) {
  unlist(strsplit(string, pattern, perl = TRUE))
}

# chr_subset ----------------------------------------------------

#' Get matching elements
#'
#' @description
#' `chr_subset` returns a subset of the input character vector, containing only the elements that match a specified pattern.
#'
#' @param string A character vector from which elements are to be subset.
#' @param pattern A character string containing a regular expression (or a fixed string) to match against each element of `string`.
#'
#' @return
#' A character vector containing only the elements of `string` that match the specified `pattern`. If no matches are found, an empty character vector is returned.
#'
#' @examples
#' chr_subset(c("apple", "banana", "cherry", "date"), "a")
#' #> [1] "apple" "banana" "date"
#' chr_subset(c("apple", "banana", "cherry", "date"), "^a")
#' #> [1] "apple"
#'
#' @export
chr_subset <- function(string, pattern) {
  grep(pattern, string, value = TRUE, perl = TRUE)
}

