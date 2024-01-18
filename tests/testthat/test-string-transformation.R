# str_to_case ---------------------------------------------------i
## str_to_upper_case -------------------------------------------

test_that("str_to_upper_case converts to upper case correctly", {
  expect_equal(str_to_upper_case("hello world"), "HELLO WORLD")
  expect_equal(str_to_upper_case("Hello World"), "HELLO WORLD")
  expect_equal(str_to_upper_case("123 ABC"), "123 ABC")
})


## str_to_lower_case -------------------------------------------

test_that("str_to_lower_case converts to lower case correctly", {
  expect_equal(str_to_lower_case("HELLO WORLD"), "hello world")
  expect_equal(str_to_lower_case("Hello World"), "hello world")
  expect_equal(str_to_lower_case("123 ABC"), "123 abc")
})

## str_to_snake_case -------------------------------------------

test_that("str_to_snake_case converts to snake case correctly", {
  expect_equal(str_to_snake_case("Hello World"), "hello_world")
  expect_equal(str_to_snake_case("HelloWorld"), "hello_world")
  expect_equal(str_to_snake_case("hello-world"), "hello_world")
  expect_equal(str_to_snake_case("helloWorld"), "hello_world")
  expect_equal(str_to_snake_case("Hello-World 123"), "hello_world_123")
  expect_equal(str_to_snake_case("--Hello**World--"), "hello_world")
})


# str_ whitespace ---------------------------------------------------------

# str_trim ----------------------------------------------------------------

test_that("str_trim removes whitespace at the front", {
  expect_equal(str_trim("    Hello"), "Hello")
  expect_equal(str_trim(c("  Hello", "   world")), c("Hello", "world"))
})

test_that("str_trim removes whitespace at the end", {
  expect_equal(str_trim("Hello   "), "Hello")
  expect_equal(str_trim(c("Hello   ", "world    ")), c("Hello", "world"))
})

test_that("str_trim removes whitespace at both ends", {
  expect_equal(str_trim("  Hello   "), "Hello")
  expect_equal(str_trim(c("   Hello   ", "   world    ")), c("Hello", "world"))
})

test_that("str_trim empties a string with only whitespace", {
  expect_equal(str_trim("   "), "")
  expect_equal(str_trim("                                      "), "")
})

test_that("str_trim does nothing to a string without leading/trailing whitespace", {
  expect_equal(str_trim("Hello"), "Hello")
  expect_equal(str_trim(c("Hello", "world")), c("Hello", "world"))
})

# str_squish --------------------------------------------------------------


