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




