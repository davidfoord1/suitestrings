# chr_detect ----------------------------------------------------

#' @rdname str_detect
#' @export
chr_detect_any <- function(strings, pattern, fixed = FALSE) {
  any(grepl(pattern, strings, perl = TRUE, fixed = fixed))
}

#' @rdname str_detect
#' @export
chr_detect_all <- function(strings, pattern, fixed = FALSE) {
  all(grepl(pattern, strings, perl = TRUE, fixed = fixed))
}


#' Find matching indices in character vector
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @details
#' These functions are built using the base R regular expression functions.
#' `{suitestrings}` uses Perl-compatible Regular Expressions (PCRE).
#' This is achieved by setting `perl = TRUE` in the underlying base functions.
#' See R's [regexp] documentation for info on the regex implementation.
#' For complete syntax details see \href{https://www.pcre.org/current/doc/html/}{https://www.pcre.org/current/doc/html/}
#'
#' @return An integer vector of matching indices in string.
#' @export
#'
#' @examples
#' chr_which(letters, "[aeiou]")
#' #> [1]  1  5  9 15 21
chr_which <- function(strings, pattern, fixed = FALSE) {
  grep(pattern, strings, perl = TRUE, fixed = fixed)
}

#' Count matching indices in character vector
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return A single integer representing the number of matches found
#'
#' @details
#' These functions are built using the base R regular expression functions.
#' `{suitestrings}` uses Perl-compatible Regular Expressions (PCRE).
#' This is achieved by setting `perl = TRUE` in the underlying base functions.
#' See R's [regexp] documentation for info on the regex implementation.
#' For complete syntax details see \href{https://www.pcre.org/current/doc/html/}{https://www.pcre.org/current/doc/html/}
#'
#' @export
#'
#' @examples
#' chr_count_matches(letters, "[aeiou]")
#' #> [1] 5
chr_count_matches <- function(strings, pattern, fixed = FALSE) {
  length(grep(pattern, strings, perl = TRUE, fixed = fixed))
}


# chr_extract ---------------------------------------------------

#' @rdname str_extract
#' @export
chr_extract_all <- function(strings, pattern, fixed = FALSE) {
  unlist(regmatches(strings, gregexpr(pattern, strings, perl = TRUE, fixed = fixed)))
}

# chr_split -----------------------------------------------------

#' @rdname str_split
#' @export
chr_split_all <- function(strings, pattern, fixed = FALSE) {
  unlist(strsplit(strings, pattern, perl = TRUE, fixed = fixed))
}

# chr_subset ----------------------------------------------------

#' Get matching elements
#'
#' @description
#' `chr_subset` returns a subset of the input character vector, containing only the elements that match a specified pattern.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param pattern
#' A single character string to be searched for in each element of `strings`.
#' By default, `pattern` is interpreted as a regular expression (regex). If the `fixed` argument is set to `TRUE`,
#' `pattern` will be treated as a literal string to be matched exactly.
#' @param fixed
#' Logical; whether `pattern` should be matched exactly,
#' treating regex special characters as regular  string characters. Default `FALSE`.
#'
#' @return
#' A character vector containing only the elements of `strings` that match the specified `pattern`. If no matches are found, an empty character vector is returned.
#'
#' @details
#' These functions are built using the base R regular expression functions.
#' `{suitestrings}` uses Perl-compatible Regular Expressions (PCRE).
#' This is achieved by setting `perl = TRUE` in the underlying base functions.
#' See R's [regexp] documentation for info on the regex implementation.
#' For complete syntax details see \href{https://www.pcre.org/current/doc/html/}{https://www.pcre.org/current/doc/html/}
#'
#' @examples
#' chr_subset(c("apple", "banana", "cherry", "date"), "a")
#' #> [1] "apple" "banana" "date"
#' chr_subset(c("apple", "banana", "cherry", "date"), "^a")
#' #> [1] "apple"
#'
#' @export
chr_subset <- function(strings, pattern, fixed = FALSE) {
  grep(pattern, strings, value = TRUE, perl = TRUE, fixed = fixed)
}

