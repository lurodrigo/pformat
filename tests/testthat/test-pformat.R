
library(pformat)

context("Basic parsing")

test_that("Double braces are correctly interpreted", {
  expect_equal(pformat("{{"), "{")
  expect_equal(pformat("}}"), "}")
})

test_that("Unmatching braces trigger error", {
  expect_error(pformat("{"))
  expect_error(pformat("}"))
  expect_error(pformat("{{{"))
  expect_error(pformat("}}}"))
})