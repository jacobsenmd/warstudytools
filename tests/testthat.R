library(testthat)
library(warstudytools)

test_check("warstudytools")

test_that("is_value_in_comma_delim works", {
  expect_equal(is_value_in_comma_delim("100", "100, 200, 300"), TRUE)
  expect_equal(is_value_in_comma_delim("101", "100, 200, 300"), FALSE)
})

test_that("count_new_groups works", {
  expect_equal(count_new_groups("299, 300, 292", "299, 300, 302"), 1)
})

test_that("unique_values_in_comma_sep_vectors works", {
  expect_equal(unique_values_in_comma_sep_vectors(c("100, 200, 300", "100, 301, 302")), "100, 200, 300, 301, 302")
})

