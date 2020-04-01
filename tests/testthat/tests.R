context("sadj")

library(sadj)

test_that("adjust air passengers with default x11 spec", {
  ap <- X13Series(AirPassengers, type = "x11")
  ap.res <- adjust(ap)
  expect_true((nrow(ap) == nrow(sa(ap.res))) & (nrow(ap) == nrow(trend(ap.res))))
})

test_that("adjust air passengers with default seats spec", {
  ap <- X13Series(AirPassengers, type = "seats")
  ap.res <- adjust(ap)
  expect_true((nrow(ap) == nrow(sa(ap.res))) & (nrow(ap) == nrow(trend(ap.res))))
})
