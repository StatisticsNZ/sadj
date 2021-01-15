context("sadj")

library(sadj)

test_that("testairline.spc is parsed correctly", {
  ap <- readSPC("../test_spec/testairline.spc")

  expect_equal(
    getSpecParameter(ap, "series", "title"),
    "International Airline Passengers Data from Box and Jenkins"
  )

  expect_equal(
    getSpecParameter(ap, "series", "start"),
    "1949.01"
  )

  ## data doesn't look to have been imported all the nicely, so skip that step.

  expect_equal(
    getSpecParameter(ap, "series", "span"),
    "(1952.01,)"
  )

  # this fails because their is a trailing tab left on the parameter
  expect_equal(
    getSpecParameter(ap, "spectrum", "savelog"),
    "peaks"
  )

  expect_equal(
    getSpecParameter(ap, "transform", "function"),
    "auto"
  )

  expect_equal(
    getSpecParameter(ap, "transform", "savelog"),
    "autotransform"
  )

  expect_equal(
    getSpecParameter(ap, "regression", "aictest"),
    "(td easter)"
  )

  expect_equal(
    getSpecParameter(ap, "regression", "savelog"),
    "aictest"
  )

  expect_equal(
    getSpecParameter(ap, "automdl", "savelog"),
    "automodel"
  )

  # this fails because:
  # first has args attribute that is `list()`
  # second has args attribute that is `NULL`
  expect_equal(
    getSpec(ap, "outlier"),
    X13Spec("outlier")
  )

  # this fails because:
  # first has args attribute that is `list()`
  # second has args attribute that is `NULL`
  expect_equal(
    getSpec(ap, "x11"),
    X13Spec("x11")
  )
})

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
