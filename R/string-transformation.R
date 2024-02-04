#' Convert strings to different cases
#'
#' @description
#' Convert strings to "UPPER CASE", "lower case", "snake_case" or
#' "camelCase".
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param split_on_capital
#' `str_to_snake_case()` only: whether to treat every upper case letter as
#' the start of a new word. This is for converting from camelCase.
#'
#' @param capitalise_first
#' `str_to_camel_case()` only: whether the first letter of the first word
#' in each string should be capitalised as in "PascalCase" not "camelCase".
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
#' Base R [tolower()] and [toupper()].
#'
#' @export
#' @rdname str_to_case
#'
#' @examples
#' string <- "A good day for kite-flying!"
#'
#' str_to_upper_case(string)
#' #> [1] "A GOOD DAY FOR KITE-FLYING!"
#' str_to_lower_case(string)
#' #> [1] "a good day for kite-flying!"
#' str_to_snake_case(string)
#' #> [1] "a_good_day_for_kite_flying"
#' str_to_camel_case(string)
#' #> [1] "aGoodDayForKiteFlying"
#'
#' # Optionally convert from camelCase:
#' str_to_snake_case(
#'   str_to_camel_case(string),
#'   split_on_capital = TRUE
#' )
#' #> [1] "a_good_day_for_kite_flying"
#'
#' # Optionally capitalise first letter of the word
#' str_to_camel_case(
#'   string,
#'   capitalise_first = TRUE
#' )
#' #> [1] "AGoodDayForKiteFlying"
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

#' @export
#' @rdname str_to_case
str_to_camel_case <- function(strings, capitalise_first = FALSE) {
  # Replace all non-alphanumeric sequences with an underscore
  strings <- gsub("[^[:alnum:]]+", "_", strings)

  # Remove any leading or trailing underscores
  strings <- gsub("^_+|_+$", "", strings)

  # Split on underscores
  strings <- strsplit(strings, "_", fixed = TRUE)

  strings <- lapply(
    strings,
    \(string) {
      string <- strsplit(tolower(string), "")
      # Convert the first character of each word to upper case
      for (index in seq_along(string)) {
        string[[c(index, 1)]] <- toupper(string[[c(index, 1)]])
      }
      if (!capitalise_first) {
        string[[c(1, 1)]] <- tolower(string[[c(1, 1)]])
      }
      string <- paste(unlist(string), collapse = "")
    }
  )

  unlist(strings)
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
#' str_indent(c("Hello", "World"), 3, indent = ".")
#' #> [1] "...Hello" "...World"
#' str_indent(c("Hello", "World"), 3, "both", "-")
#' #> [1] "---Hello---" "---World---"
#'
#' # Get extra with it
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

#' Pad a string to a minimum length
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param min_length
#' The minimum number of characters in the padded string. The default is one more
#' character than the longest string in `strings`. Strings already longer than
#' `min_length` will be unchanged.
#' @param side
#' Which side of the string should the indent be applied, one of
#' "left", "right" or "both". The left side is the default. When "both", half
#' the required padding length with be added to each side, with `prefer_side`
#' controlling where odd additions go.
#' @param pad
#' A single character used to pad the space
#' @param prefer_side
#' If `side` is "both", which side should get one extra pad when `min_length`
#' is odd? Default to the right side.
#'
#' @return
#' A character vector equal in length to `strings`, with each string having at
#' least as many characters as `min_length`. Strings already longer than `min_length`
#' will be unchanged.
#'
#' @export
#'
#' @seealso
#' [str_indent()] for adding a specific number of characters.
#'
#' [str_concat()] and [str_glue()] for combining strings together.
#'
#' Base [paste0()] and [strrep()] used by this function.
#'
#' @examples
#' str_pad(c("Hello", "World"))
#' #> [1] " Hello" " World"
#'
#' str_pad(c("Hello", "World"), 10)
#' #> [1] "     Hello" "     World"
#'
#' str_pad(c("Hello", "World"),
#'         10,
#'         side = "both")
#' #> [1] "  Hello   " "  World   "
#'
#' str_pad(c("Hello", "World"),
#'         10,
#'         side = "both",
#'         pad = ".")
#' #> [1] "..Hello..." "..World..."
#'
#' str_pad(c("Hello", "World"),
#'         10,
#'         side = "both",
#'         pad = ".",
#'         prefer_side = "left")
#' #> [1] "...Hello.." "...World.."
str_pad <- function(strings,
                    min_length = max(nchar(strings)) + 1,
                    side = c("left", "right", "both"),
                    pad = " ",
                    prefer_side = c("right", "left")
                    ) {
  stopifnot(nchar(pad) == 1)
  stopifnot(length(pad) == 1)

  side <- match.arg(side)
  prefer_side <- match.arg(prefer_side)

  pad_length <- min_length - nchar(strings)

  left_pad <- right_pad <- 0

  if (side == "both") {
    side_length <- pad_length/2
    if (prefer_side == "right") {
      left_pad <- floor(side_length)
      right_pad <- ceiling(side_length)
    } else {
      right_pad <- floor(side_length)
      left_pad <- ceiling(side_length)
    }
  }

  if (side == "left") {
    left_pad <- pad_length
  }

  if (side == "right") {
    right_pad <- pad_length
  }

  paste0(strrep(pad, left_pad), strings, strrep(pad, right_pad))
}

#' Wrap character strings to format as paragraphs
#'
#' @description
#' Each character string in the input is first split into paragraphs
#' (or lines containing whitespace only).
#' The paragraphs are then formatted by breaking lines at word boundaries.
#' The target columns for wrapping lines and the indentation of the first
#' and all subsequent lines of a paragraph can be controlled independently.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param min_width
#' A positive integer giving the minimum line width for for wrapping lines in the output.
#' @param indent
#' A non-negative integer giving the indentation of the first line in a paragraph.
#' @param exdent
#' A non-negative integer specifying the indentation of subsequent lines in paragraphs.
#' @param initial
#' A character string to be used as a prefix for the first line in each
#' paragraph.
#' @param prefix
#' A character string to be used as a prefix for subsequent lines in each
#' paragraph.
#'
#' @return
#' A list of a character vector for each input string in `strings`.
#' The new strings in each character vector are each a single line of the
#' the string formatted by paragraph.
#' @export
#'
#' @examples
#' text <- paste(readLines(file.path(R.home("doc"), "THANKS")), collapse = "\n")
#'
#' writeLines(str_wrap(text, min_width = 60))
#' writeLines(str_wrap(text, min_width = 60, indent = 5))
#' writeLines(str_wrap(text, min_width = 60, exdent = 5))
#' writeLines(str_wrap(text, min_width = 60, prefix = "> "))
str_wrap <- function(strings,
                     min_width = 80,
                     indent = 0,
                     exdent = 0,
                     prefix = "",
                     initial = prefix
) {
  strwrap(strings, min_width, indent, exdent, prefix, initial = initial)
}

#' Get the length or width of strings
#'
#' @description Get the length in characters or width of strings, which differ where some
#' characters take up multiple characters worth of space, like emojis.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#'
#' @return
#' A numeric vector the same length as string.
#'
#' `str_length()`: The number of characters in a string.
#'
#' `str_width()`: The amount of space the string will take up when printed with
#' a fixed-width font, such as in the console.
#'
#' @export
#' @rdname str_length
#'
#' @seealso
#' Base [nchar()] which these functions are built with.
#'
#' @examples
#' str_length(c("Hello", "everyone"))
#' #> [1] 5 8
#'
#' str_length("😊")
#' #> [1] 1
#' str_width("😊")
#' #> [1] 2
str_length <- function(strings) {
  nchar(strings)
}

#' @export
#' @rdname str_length
str_width <- function(strings) {
  nchar(strings, type = "width")
}

#' Repeat character sequences
#'
#' @description
#' Repeat the contents of character strings a given number of times.
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param times
#' The number of times each string should be repeated.
#' @param separator
#' A string to place in-between each repetition of the main string.
#'
#' @return
#' A character vector the length of whichever is longest out of `strings`,
#' `times` and `separator`, where the shorter vectors will be recycled.
#'
#'
#' Each element in the result contains the same character sequence of the
#' corresponding element in strings, repeated the given number of times.
#'
#' @export
#'
#' @examples
#' str_repeat("hello")
#' #> [1] "hellohello"
#' str_repeat("hello", 3)
#' #> [1] "hellohellohello"
#' str_repeat("hello", 3, ", ")
#' #> [1] "hello, hello, hello"
#'
#' str_repeat(c("hello", "world"))
#' #> [1] "hellohello" "worldworld"
#' str_repeat(c("hello", "world"), c(3, 1))
#' #> [1] "hellohellohello" "world"
#'
#' str_repeat(".", 0:3)
#' #> [1] ""    "."   ".."  "..."
str_repeat <- function(strings, times = 2, separator = "") {
  stopifnot(times >= 0)

  strings <- strrep(paste0(strings, separator), times)
  return(substr(strings, 1, nchar(strings) - nchar(separator)))
}

#' Shorten a string to a specified size
#'
#' @param strings
#' A character vector, where each element of the vector is a character string.
#' @param length
#' The maximum number of characters per string in the output vector.
#' @param side
#' Where the truncation should happen i.e. the location to remove characters from.
#' @param ellipsis
#' Characters to replace removed characters, indicating shortening, an ellipsis
#' "..." by default
#'
#' @return
#' @export
#'
#' @examples
#' str_truncate("Help me make this shorter", 15)
#' #> [1] "Help me make..."
#' str_truncate("Help me make this shorter", 15, "left")
#' #> [1] "...this shorter"
#' str_truncate("Help me make this shorter", 15, "center")
#' #> [1] "Help m...horter"
str_truncate <-
  function(strings,
           length,
           side = c("right", "left", "center"),
           ellipsis = "...") {
  stopifnot(length >= 0)
  stopifnot(length >= nchar(ellipsis))

  side <- match.arg(side)

  vapply(
    strings,
    function(string) {
      if (nchar(string) <= length || nchar(ellipsis) >= nchar(string)) {
        return(string)
      }

      switch(
        side,
        "right" = paste0(
          substr(string, 1, length - nchar(ellipsis)),
          ellipsis
        ),
        "left" = paste0(
          ellipsis,
          substr(
            string,
            nchar(strings) - length + nchar(ellipsis) + 1,
            nchar(string)
            )
        ),
        "center" = {
          half_length <- (length - nchar(ellipsis)) %/% 2
          start <- substr(string, 1, half_length)
          end <- substr(string, nchar(string) - half_length + 1, nchar(string))
          paste0(start, ellipsis, end)
        }
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}
