#' Convert strings to different cases
#'
#' @description
#' Convert strings to "UPPER CASE", "lower case" or "snake_case".
#' Snake case is particularly useful for clean and consistent `names()`.
#' These functions are an example of assignment in snake case.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#'
#' @return
#' `str_to_upper_case()`:
#' Returns `strings` with all lower case letters converted to upper case.
#'
#' `str_to_lower_case()`
#' Returns `strings` with all upper case letters converted to lower case.
#'
#' `str_to_snake_case()`
#' Returns `strings` with all upper case letters converted to lower case,
#' non-alphanumeric characters between words converted to an underscore "_",
#' and all preceding/trailing non-alphanumeric characters removed.
#' Any alternate word separations are replaced with an underscore,
#' including as an insertion between words, for example,
#' "camelCase" will be converted to "camel_case".
#'
#' @seealso
#' [str_replace_all()] for replacing specific characters with other characters.
#'
#' @export
#' @rdname str_to_case
#'
#' @examples
#' string <- "A good day for kite_flying!"
#'
#' str_to_upper_case(string)
#' #> [1] "A GOOD DAY FOR KITE-FLYING!"
#' str_to_lower_case(string)
#' #> [1] "a good day for kite-flying!"
#' str_to_snake_case(string)
#' #> [1] "a_good_day_for_kite_flying"
str_to_upper_case<- function(strings) {
  toupper(strings)
}

#' @export
#' @rdname str_to_case
str_to_lower_case <- function(strings) {
  tolower(strings)
}

#' @export
#' @rdname str_to_case
str_to_snake_case <- function(strings) {
  # Replace all non-alphanumeric sequences with an underscore
  strings <- gsub("[^[:alnum:]]+", "_", strings)

  # Remove any leading or trailing underscores
  strings <- gsub("^_+|_+$", "", strings)

  # To handle things like camelCase
  # Insert underscores between lowercase and uppercase letters
  strings <- gsub("([a-z])([A-Z])", "\\1_\\2", strings)

  tolower(strings)
}

#' Remove trailing and leading whitespace
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#'
#' @return
#' The same character vector, but with whitespace removed from the start and end
#' of each string.
#'
#' @rdname str_trim
#' @export
#'
#' @examples
#' print(str_trim("    Remove these extra spaces    "))
#' #> [1] "Remove these extra spaces"
#' print(str_trim("Remove trailing new line character \n"))
#' #> [1] "Remove trailing new line character"
#' print(str_trim("But leave space     in   the      middle"))
#' #> [1] "But leave space     in   the      middle"
#'
#' print(str_squish(" Don't leave   space    in  the   middle   here  "))
#' #> [1] "Don't leave space in the middle here"
str_trim <- function(strings) {
  strings <- gsub("^[[:space:]]+|[[:space:]]+$", "", strings)
  strings
}

#' @export
#' @rdname str_trim
str_squish <- function(strings) {
  strings <- gsub("^[[:space:]]+", "", strings)
  strings <- gsub("[[:space:]]+$", "", strings)
  strings <- gsub("[[:space:]]+", " ", strings)
  strings
}
