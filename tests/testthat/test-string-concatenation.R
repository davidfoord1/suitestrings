# str_concat ----------------------------------------------------

test_that("str_concat handles basic string concatenation", {
  expect_equal(str_concat("Hello", ", ", "world!"), "Hello, world!")
})

test_that("str_concat correctly applies the separator", {
  expect_equal(str_concat("Hello", "world", separator = ", "), "Hello, world")
  expect_equal(str_concat("Hello", "world", separator = " -- "), "Hello -- world")
})

test_that("str_concat handles emptry strings", {
  expect_equal(str_concat("Hello", "", "world"), "Helloworld")
  expect_equal(str_concat("Hello", "", "world", separator = "-"), "Hello--world")
})

test_that("str_concat ignores NULL values", {
  expect_equal(str_concat("Hello ", NULL, "world"), "Hello world")
})

test_that("str_concat handles NA values", {
  expect_equal(str_concat("Hello ", NA_character_), "Hello NA")
})

# chr_collapse --------------------------------------------------

test_that("chr_collapse combines elements of a chr vector", {
  expect_equal(chr_collapse(letters), "abcdefghijklmnopqrstuvwxyz")
  expect_equal(chr_collapse(1:10), "12345678910")
  expect_equal(chr_collapse(c("Hello ", "world")), "Hello world")
})

test_that("chr_collapse correctly applies the separator", {
  expect_equal(chr_collapse(c("Hello", "world"), separator = " "),
               "Hello world")
  expect_equal(chr_collapse(letters, separator = "-"),
               "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q-r-s-t-u-v-w-x-y-z")
  expect_equal(chr_collapse(1:10, separator = ", "),
               "1, 2, 3, 4, 5, 6, 7, 8, 9, 10")
})

test_that("chr_collapse handles empty strings", {
  expect_equal(chr_collapse(c("Hello", "", " ", "", "world")),
               "Hello world")
})

# str_glue ------------------------------------------------------

test_that("str_glue handles basic string concatenation", {
  expect_equal(str_glue("Hello", ", ", "world!"), "Hello, world!")
})

test_that("str_glue evaluates expressions within braces", {
  x <- 10
  expect_equal(str_glue("x is {x}"), "x is 10")
  expect_equal(str_glue("x squared is {x^2}"), "x squared is 100")
})

test_that("str_glue handles named argument substitution", {
  expect_equal(str_glue("My name is {name}", name = "Alice"), "My name is Alice")
})

test_that("str_glue correctly applies the separator", {
  expect_equal(str_glue("Hello", "world", separator = ", "), "Hello, world")
  expect_equal(str_glue("Hello", "world", separator = " -- "), "Hello -- world")
})

test_that("str_glue throws an error for invalid", {
  expect_error(str_glue("This {will} cause an error"))
})
