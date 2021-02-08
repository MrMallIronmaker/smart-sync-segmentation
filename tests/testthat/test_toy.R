context("Toy examples")
library(smartsyncseg)

small_mat <- matrix(c(
  50, 1,   1, 1, 1,
   0, 0, 100, 0, 0
), ncol=2)

three_row_mat <- matrix(c(
  10,  0,  0, 0, 100, 1,  0, 0, 1,
  20,  0, 10, 5,  20, 1, 20, 1, 2,
   0, 10,  0, 5, -20, 4,  1, 0, 1
), ncol=3)

test_that("Produce larger sum when min_length <= n_items < 2*min_length", {
  expect_equal(
    smart_segment_cpp(small_mat, streak=5),
    c(2, 2, 2, 2, 2)
  )
  expect_equal(
    smart_segment_cpp(small_mat, streak=3),
    c(2, 2, 2, 2, 2)
  )
  expect_equal(
    smart_segment_cpp(three_row_mat, streak=7),
    rep(1, 9)
  )
})

test_that("Test small values", {
  expect_equal(
    smart_segment_cpp(small_mat, streak=2),
    c(1, 1, 2, 2, 2)
  )
  expect_equal(
    smart_segment_cpp(three_row_mat, streak=4),
    c(2, 2, 2, 2, 1, 1, 1, 1, 1)
  )
})

