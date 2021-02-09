context("Smart segmentation syntactic sugar")
library(smartsyncseg)

stream_a <- c(10,  0,  0, 0, 100, 1,  0, 0, 1)
stream_b <- c(20,  0, 10, 5,  20, 1, 20, 1, 2)
stream_c <- c(0, 10,  0, 5, -20, 4,  1, 0, 1)

test_that("Syntactic sugar for conversion to matries", {
  expect_equal(
    smart_segment(stream_a, stream_b, stream_c, streak=2),
    c(2, 2, 2, 2, 1, 1, 2, 2, 2)
  )
})