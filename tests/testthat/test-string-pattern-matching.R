# str_detect ----------------------------------------------------

test_that("str_detect_match() detects a pattern in a string", {
  string <- c("apple", "banana", "cherry")
  pattern <- "a"

  expect_equal(str_detect_match(string, pattern), c(TRUE, TRUE, FALSE))
})

# str_locate ----------------------------------------------------
## str_locate_first----------------------------------------------

test_that("str_locate_first finds the first match correctly", {
  expect_equal(str_locate_first("Hello world", "world"),
               matrix(c(7, 11), ncol = 2, dimnames = list(NULL, c("start", "end"))))
  expect_equal(str_locate_first("abcdef", "cd"),
               matrix(c(3, 4), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_first handles no match", {
  expect_equal(str_locate_first("Hello world", "xyz"),
               matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_first handles empty pattern", {
  expect_equal(str_locate_first("Hello world", ""),
               matrix(c(1, 0), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_first handles empty string", {
  expect_equal(str_locate_first("", "pattern"),
               matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

## str_locate_all() ----------------------------------------------

test_that("str_locate_all finds all matches correctly", {
  result <- str_locate_all(c("abc def abc", "ghi jkl"), "abc")
  expect_equal(result[[1]],
               matrix(c(1, 3, 9, 11), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
  expect_equal(result[[2]],
               matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_all handles no matches", {
  result <- str_locate_all(c("abc", "def"), "xyz")
  expect_equal(result,
               list(matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))),
                    matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end")))))
})

test_that("str_locate_all handles empty pattern", {
  result <- str_locate_all(c("abc", "def"), "")
  expect_equal(result[[1]],
               matrix(c(1, 0, 2, 1, 3, 2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
  expect_equal(result[[2]],
               matrix(c(1, 0, 2, 1, 3, 2), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("start", "end"))))
})

test_that("str_locate_all handles empty string", {
  result <- str_locate_all(c("", ""), "pattern")
  expect_equal(result,
               list(matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end"))),
                    matrix(c(NA_integer_, NA_integer_), ncol = 2, dimnames = list(NULL, c("start", "end")))))
})

# str_extract ---------------------------------------------------
## str_extract_first -------------------------------------------

test_that("str_extract_first extracts the first match correctly", {
  expect_equal(str_extract_first("abcdef", "[aeiou]"), "a")
  expect_equal(str_extract_first("Hello world", ".o"), "lo")
})

test_that("str_extract_first handles no match", {
  expect_equal(str_extract_first("Hello world", "xyz"), NA_character_)
})

test_that("str_extract_first handles empty pattern", {
  expect_equal(str_extract_first("Hello world", ""), "")
})

test_that("str_extract_first handles empty string", {
  expect_equal(str_extract_first("", "pattern"), NA_character_)
})


test_that("str_extract_first extracts the first match from a vector", {
  expect_equal(str_extract_first(c("abcde", "defghi"), "[aeiou]"), c("a", "e"))
})

## str_extract_all ---------------------------------------------

test_that("str_extract_first extracts the first match from a vector", {
  expect_equal(str_extract_all(c("abcde", "defghi"), "[aeiou]"),
               list(c("a", "e"), c("e", "i")))
})

test_that("str_extract_all handles handles non-matches", {
  expect_equal(str_extract_all(c("abcde", "xyz", "defghi"), "[aeiou]"),
               list(c("a", "e"), character(0), c("e", "i")))
})

test_that("str_extract_all handles empty pattern", {
  expect_equal(str_extract_all("abc", ""), list(rep("", nchar("abc"))))
})

test_that("str_extract_all handles empty string", {
  expect_equal(str_extract_all("", "pattern"), list(character(0)))
})


# str_replace ---------------------------------------------------
## str_replace_first -------------------------------------------

test_that("str_replace_first replaces the first match correctly", {
  expect_equal(str_replace_first("Hello world", "o", "O"), "HellO world")
  expect_equal(str_replace_first("abcdef", "cd", "CD"), "abCDef")
})

test_that("str_replace_first handles no match", {
  expect_equal(str_replace_first("Hello world", "xyz", "XYZ"), "Hello world")
})

test_that("str_replace_first handles empty pattern", {
  expect_equal(str_replace_first("Hello world", "", "X"), "XHello world")
})

test_that("str_replace_first handles empty string", {
  expect_equal(str_replace_first("", "pattern", "replacement"), "")
})

test_that("str_replace_first handles empty replacement", {
  expect_equal(str_replace_first("Hello world", "o", ""), "Hell world")
})


## str_replace_all ---------------------------------------------

test_that("str_replace_all replaces all matches correctly", {
  expect_equal(str_replace_all("Hello world", "o", "O"), "HellO wOrld")
  expect_equal(str_replace_all("abc abc abc", "abc", "XYZ"), "XYZ XYZ XYZ")
})

test_that("str_replace_all handles no matches", {
  expect_equal(str_replace_all("Hello world", "xyz", "XYZ"), "Hello world")
})

test_that("str_replace_all handles empty pattern", {
  expect_equal(str_replace_all("abc", "", "X"), "XaXbXcX")
})

test_that("str_replace_all handles empty string", {
  expect_equal(str_replace_all("", "pattern", "replacement"), "")
})

test_that("str_replace_all handles empty replacement", {
  expect_equal(str_replace_all("Hello world", "o", ""), "Hell wrld")
})





