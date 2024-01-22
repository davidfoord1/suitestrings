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
  strings <- gsub("[[:space:]]+", " ", str_trim(strings))
  strings
}

#' Add characters to the ends of strings
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param times
#' The number of times the `indent` should be repeated, on each side specified.
#' @param indent
#' The characters to be used to indent.
#' @param side
#' Which side of the string should the indent be applied, one of
#' "left", "right" or "both". The left side is the default.
#'
#' @return A character vector of the same length as `strings`, with each
#' element indented as specified by the `times`, `indent`, and `side` arguments.
#' @export
#'
#' @seealso
#' [str_concat()] and [str_glue()] for combining strings together.
#'
#' Base [paste0()] and [strrep()] used by this function.
#'
#' @examples
#' str_indent(c("Hello", "World"), 3)
#' #> [1] "   Hello" "   World"
#' str_indent(c("Hello", "World"), 3, ".")
#' #> [1] "...Hello" "...World"
#' str_indent(c("Hello", "World"), 3, "-", "both")
#' #> [1] "---Hello---" "---World---"
#'
#' # Being extra
#' "wow" |>
#'   str_indent(side = "both") |>
#'   str_indent(3, indent = "->>") |>
#'   str_indent(3,  side = "right", indent = "<<-") |>
#'   str_indent(2, side = "both", indent = "-||-") |>
#'   str_indent(2, indent = "[") |>
#'   str_indent(2, side = "right", indent = "]")
#' #> [1] "[[-||--||-->>->>->> wow <<-<<-<<--||--||-]]"
str_indent <- function(strings,
                       times = 1,
                       side = c("left", "right", "both"),
                       indent = " ") {
  side = match.arg(side)

  left_indent <- right_indent <- 0

  if (side == "left" || side == "both") left_indent <- times
  if (side == "right" || side == "both") right_indent <- times

  paste0(strrep(indent, left_indent), strings, strrep(indent, right_indent))
}
