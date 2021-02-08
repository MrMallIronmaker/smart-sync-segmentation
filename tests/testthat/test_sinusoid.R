context("Large mathematically clear examples.")
library(smartsyncseg)


sinusoids_n_by_n <- function(rows, tau_frac, duration=2*pi*3, step=0.1) {
  omega <- seq(0, duration, by=step)
  omega_mat <- matrix(omega, nrow=length(omega), ncol=rows)
  offset_mat <- matrix(2*pi*seq(rows)/tau_frac, nrow=length(omega), ncol=rows, byrow=T)
  return(sin(omega_mat + offset_mat))
}

sinusoids_2_by_tau3 <- sinusoids_n_by_n(2, 3)
# this sinusoids' length in which one is larger is 2*pi/0.1/2 ~ 31.41

sinusoids_5_by_tau7 <- sinusoids_n_by_n(5, 7)
# do., 8 < 2*pi/0.1/7 < 9

expect_middle_equal <- function(mtx, streak) {
  nrows <- nrow(mtx)
  expect_equal(
    smart_segment_cpp(mtx, streak)[(streak+1):(nrows-streak)],
    max.col(mtx, "first")[(streak+1):(nrows-streak)],
  )
}

expect_middle_unequal <- function(mtx, streak) {
  nrows <- nrow(mtx)
  expect_true(any(
    smart_segment_cpp(mtx, streak)[(streak+1):(nrows-streak)] == 
      max.col(mtx, "first")[(streak+1):(nrows-streak)]
  ))
}

test_that("Sinusoids wider than streak are equivalent to max.col, save for boundary issues.", {
  expect_middle_equal(sinusoids_2_by_tau3, 31)
  expect_middle_equal(sinusoids_2_by_tau3, 10)
  expect_middle_equal(sinusoids_2_by_tau3, 2)
  expect_middle_equal(sinusoids_5_by_tau7, 8)
  expect_middle_equal(sinusoids_5_by_tau7, 6)
  expect_middle_equal(sinusoids_5_by_tau7, 3)
  
})

test_that("Sinusoids narrower than streak are not equivalent to pmax.", {
  expect_middle_unequal(sinusoids_2_by_tau3, 60)
  expect_middle_unequal(sinusoids_2_by_tau3, 32)
  expect_middle_unequal(sinusoids_5_by_tau7, 9)
  expect_middle_unequal(sinusoids_5_by_tau7, 15)
})



