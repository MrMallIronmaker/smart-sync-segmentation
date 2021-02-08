context("Smart segmentation validity")
library(smartsyncseg)

cheap_mat <- matrix(c(1,2,3,4), nrow=2)

test_that("streak should be only one element", {
  expect_error(
    smart_segment_cpp(cheap_mat, streak=c(0, 1, 2)),
    "Error: `streak` is a vector of length 3, but should be length 1."
  )
})

test_that("Errors if matrix is smaller than streak", {
  expect_error(
    smart_segment_cpp(cheap_mat, min_length=6),
    pattern = "`streak`` \\(\\d+\\) is larger than number of entries \\(\\d+\\)"
  )
})