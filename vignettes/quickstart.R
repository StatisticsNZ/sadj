## ----setup, include=FALSE, message = FALSE, warning = FALSE-------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, warning = FALSE,
                      message = FALSE, comment = NA, 
                      fig.align = "center", 
                      fig.height = 4, fig.width = 5)
library(sadj)

## ----listsofliststab, echo = FALSE--------------------------------------------
data.frame(object=c("X13Batch","X13SeriesGroup", "X13BatchResult", "X13SeriesGroupResult"),
           list_of=c("X13SeriesGroup","X13Series","X13SeriesGroupResult","X13SeriesResult"),
           comments=c("The groups are created by inserting empty lines in the .mta file between series.",
                      "There can be zero or one composite series in a group.  The composite must go at the end.",
                      "adjust produces result objects.",
                      "")) %>% 
  knitr::kable(row.names = FALSE, format = "markdown")

## ----getsettab, echo = FALSE--------------------------------------------------
data.frame(get=c("getSpecList","getSpec","getSpecParameter", "getFacFile", "getRegFile"),
           set=c("setSpecList","setSpec","setSpecParameter", "setFacFile", "setRegFile"),
           comments=c("Method gets passed to the X13SpecList object which is attached to X13Series as an attribute",
                      "Same as comment above",
                      "Same as comment above",
                      "get/set the factor file dataframe attached to the the X13Series",
                      "get/set the regression file dataframe attached to the the X13Series")) %>% 
  knitr::kable(row.names = FALSE, format = "markdown")

## ----simpleex-----------------------------------------------------------------
ap <- X13Series(AirPassengers)

## ----simpleextab, echo = FALSE------------------------------------------------
knitr::kable(head(ap, 10), row.names = FALSE, format = "markdown")

## ----simpleex.spec------------------------------------------------------------
getSpecList(ap)

## ----seats01------------------------------------------------------------------
ap2 <- X13Series(AirPassengers, type = "seats")
getSpecList(ap2)

## ----simpleex.adjust----------------------------------------------------------
ap.res <- adjust(ap)

## ----simpleex.res, echo = FALSE-----------------------------------------------
knitr::kable(head(ap.res, 10), row.names = FALSE, format = "markdown")

## ----simpleex.trend, echo = FALSE---------------------------------------------
knitr::kable(head(trend(ap.res), 10), row.names = FALSE, format = "markdown")

## ----simpleex.res.summary, echo = FALSE---------------------------------------
knitr::kable(summary(ap.res), row.names = FALSE, format = "markdown")

## ----simpleex.res.plot1-------------------------------------------------------
plot(ap.res)

## ----simpleex.res.plot2-------------------------------------------------------
plot(ap.res, type = "d10")

