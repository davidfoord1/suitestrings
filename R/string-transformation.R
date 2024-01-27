#' Convert strings to different cases
#'
#' @description
#' Convert strings to "UPPER CASE", "lower case" or "snake_case".
#' Snake case is particularly useful for clean and consistent `names()`.
#' These functions are an example of assignment in snake case.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param split_on_capital
#' `str_to_snake_case()` only: whether to treat every upper case letter as
#' the start of a new word. This is for converting from PascalCase and
#' camelCase.
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
#'
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
#'
#' # Optionally convert from PascalCase and camelCase:
#' str_to_snake_case(
#'   "AGoodDayForKiteFlying",
#'   split_on_capital = TRUE
#' )
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
str_to_snake_case <- function(strings, split_on_capital = FALSE) {
  # Replace all non-alphanumeric sequences with an underscore
  strings <- gsub("[^[:alnum:]]+", "_", strings)

  # Remove any leading or trailing underscores
  strings <- gsub("^_+|_+$", "", strings)

  if (split_on_capital) {
    # Insert underscores between lowercase and uppercase letters
    strings <- gsub("([a-zA-Z])([A-Z])", "\\1_\\2", strings)
  }

  tolower(strings)
}
