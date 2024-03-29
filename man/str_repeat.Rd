% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string-transformation.R
\name{str_repeat}
\alias{str_repeat}
\title{Repeat character sequences}
\usage{
str_repeat(strings, times = 2, separator = "")
}
\arguments{
\item{strings}{A character vector, where each element of the vector is a character string.}

\item{times}{The number of times each string should be repeated.}

\item{separator}{A string to place in-between each repetition of the main string.}
}
\value{
A character vector the length of whichever is longest out of \code{strings},
\code{times} and \code{separator}, where the shorter vectors will be recycled.

Each element in the result contains the same character sequence of the
corresponding element in strings, repeated the given number of times.
}
\description{
Repeat the contents of character strings a given number of times.
}
\examples{
str_repeat("hello")
#> [1] "hellohello"
str_repeat("hello", 3)
#> [1] "hellohellohello"
str_repeat("hello", 3, ", ")
#> [1] "hello, hello, hello"

str_repeat(c("hello", "world"))
#> [1] "hellohello" "worldworld"
str_repeat(c("hello", "world"), c(3, 1))
#> [1] "hellohellohello" "world"

str_repeat(".", 0:3)
#> [1] ""    "."   ".."  "..."
}
\seealso{
Base \code{\link[=strrep]{strrep()}} which this function wraps.
}
