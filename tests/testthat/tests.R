# no longer recommended to use context()
# context("sadj")

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

test_that("concrete.spc is parsed correctly", {
  concrete <- readSPC("../test_spec/concrete.spc")
  expect_equal(
    getSpecParameter(concrete, "regression", "variables")
    , "(td1nolpyear ao2020.2)"
  )

  expect_equal(
    getSpecParameter(concrete, "regression", "save")
    , "ao"
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

test_that("adjust air passengers setSpec example works", {
  specl <- X13SpecList(
    series = list(
      start = "1949.1", period = "12", title = "AirPassengers",
      file = "AirPassengers.dat", format = "datevalue", save = "(b1)"
    )
  )
  setSpec(specl, "x11") <- list(
    mode = "mult", sigmalim = "(1.8,2.8)", save = "(d8 d10 d11 d12 d13 c17)"
  )
  setSpec(specl) <- X13Spec(specname = "transform", `function`="log")

  expect_equal(getSpecParameter(specl,"series","start"),"1949.1")
  expect_equal(getSpecParameter(specl,"x11","save"),"(d8 d10 d11 d12 d13 c17)")
  expect_equal(getSpecParameter(specl,"transform","function"),"log")
})

test_that("adjust group from HLFS fake data", {
  memp <- X13Series(HLFS[HLFS$sname == "mlem",], sname = "memp")
  setSpecParameter(memp, "series", "comptype") <- "add"

  femp <- X13Series(HLFS[HLFS$sname == "fmle",], sname = "femp")
  setSpecParameter(femp, "series", "comptype") <- "add"

  emp  <- X13Series(
    data.frame(memp[,c("year","period")], value = memp[,"value"] + femp[,"value"]),
    sname = "emp",
    composite = list(title = "emp", save = "(isf isa itn b1 id8 iir)"),
    x11 = list(mode = "add", sigmalim = "(1.8,2.8)", save = "(e1 c17 d8 d9 d10 d11 d12 d13)")
  )

  empgrp <- X13SeriesGroup("employment", memp, femp, emp)

  empgrp.res <- adjust(empgrp)
  expect_true((nrow(empgrp[[3]]) == nrow(sa(empgrp.res[[3]]))) &
                (nrow(empgrp[[3]]) == nrow(trend(empgrp.res[[3]]))))
  expect_equal(round(tail(empgrp.res[[3]][["d11"]],1),3),2511.741)
  expect_equal(round(tail(empgrp.res[[3]][["d12"]],1),3),2513.785)
  expect_equal(round(tail(empgrp.res[[3]][["isa"]],1),3),2511.886)
  expect_equal(round(tail(empgrp.res[[3]][["itn"]],1),3),2513.856)

})


