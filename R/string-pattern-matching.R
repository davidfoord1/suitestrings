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

# str_locate ----------------------------------------------------

#' Locate Pattern Occurrences in Strings
#'
#' @description
#' These functions find occurrences of a pattern in strings.
#'
#' `str_locate_first()` finds the first occurrence of a pattern in a string.
#' `str_locate_all()` finds all occurrences of a pattern in each string of the input vector.
#'
#' @param string A character vector of strings to search in.
#' @param pattern A character string containing a regular expression.
#'
#' @return `str_locate_first()` returns a two-column matrix with the start and end positions
#' of the first match. There is a row for each string. `str_locate_all()` returns a list of matrices.
#' There is a matrix for each string and a row for each match.
#' If no match is found, NA values are returned.
#'
#' @examples
#' str_locate_first("Hello world", "world")
#' #>      start end
#' #> [1,]     7  11
#' str_locate_first(c("Hello world", "Goodbye world"), "o")
#' #>      start end
#' #> [1,]     5   5
#' #> [2,]     2   2
#'
#' str_locate_all(c("Hello world", "Goodbye world"), "world")
#' #> [[1]]
#' #>      start end
#' #> [1,]     7  11
#' #>
#' #> [[2]]
#' #>      start end
#' #> [1,]     9  13
#' str_locate_all(c("Hello world", "Goodbye world"), "o")
#' #> [[1]]
#' #> start end
#' #> [1,]     5   5
#' #> [2,]     8   8
#' #>
#' #> [[2]]
#' #> start end
#' #> [1,]     2   2
#' #> [2,]     3   3
#' #> [3,]    10  10
#'
#' @rdname str_locate
#' @export
str_locate_first <- function(string, pattern) {
  # Get the match info
  match_info <- regexpr(pattern, string)
  match_length <- attr(match_info, "match.length")
  match_end <- match_info + match_length - 1L

  no_match <- match_info == -1
  match_info[no_match] <- NA_integer_
  match_end[no_match] <- NA_integer_

  matrix(c(match_info, match_end),
         ncol = 2,
         dimnames = list(NULL, c("start", "end")))
}

#' @rdname str_locate
#' @export
str_locate_all <- function(string, pattern) {
  find_matches_in_string <- function(string) {
    match_info <- gregexpr(pattern, string)[[1]]
    ends <- match_info + attr(match_info, "match.length") - 1L

    # If there are no matches, store as NA NA
    if (all(match_info == -1L)) {
      return(matrix(NA_integer_,
                    ncol = 2,
                    nrow = 1,
                    dimnames = list(NULL, c("start", "end"))))
    }

    cbind(start = match_info, end = ends)
  }

  matches <- lapply(string, find_matches_in_string)
  return(matches)
}

