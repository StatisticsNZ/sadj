## ----setup, include=FALSE, message = FALSE, warning = FALSE-------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, warning = FALSE,
                      message = FALSE, comment = NA, 
                      fig.align = "center", 
                      fig.height = 4, fig.width = 5)
library(sadj)

## ----simple-------------------------------------------------------------------
ap <- X13Series(AirPassengers)
ap.res <- adjust(ap)

## ----seats01------------------------------------------------------------------
ap <- X13Series(AirPassengers, type = "seats")
getSpecList(ap)

## ----spec01-------------------------------------------------------------------
series_spec <- X13Spec(
  "series", save = "(b1)"
)

x11_spec <- X13Spec(
  "x11",
  mode = "mult", sigmalim = "(1.8,2.8)", save="(d8 d10 d11 d12 d13 c17)"
)

## ----spec02-------------------------------------------------------------------
tryCatch(
   X13Spec("series", foo = "bar"), 
   error = function(e) e$message
)

## ----spec03-------------------------------------------------------------------
spec <- X13SpecList(
  series = series_spec,
  x11 = x11_spec
)

## ----spec04-------------------------------------------------------------------
spec <- X13SpecList(
  series = list(
    save = "(b1)"
  ),
  x11 = list(
    mode = "mult", sigmalim="(1.8,2.8)", save = "(d8 d10 d11 d12 d13 c17)"
  )
)

## ----spec05-------------------------------------------------------------------
series <- X13Series(AirPassengers, speclist = spec)

## ----spec06-------------------------------------------------------------------
setSpecParameter(spec, "x11", "mode") <- "add"
spec

## ----spec07-------------------------------------------------------------------
setSpec(spec, "outlier") <- list()
spec

