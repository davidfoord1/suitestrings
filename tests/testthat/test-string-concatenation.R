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
