## ----setup, include=FALSE, message = FALSE, warning = FALSE-------------------
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, warning = FALSE,
                      message = FALSE, comment = NA, 
                      fig.align = "center", 
                      fig.height = 4, fig.width = 5)
library(sadj)

## ----ex01---------------------------------------------------------------------
memp <- X13Series(HLFS[HLFS$sname == "mlem",], sname = "memp")
setSpecParameter(memp, "series", "comptype") <- "add"

femp <- X13Series(HLFS[HLFS$sname == "fmle",], sname = "femp")
setSpecParameter(femp, "series", "comptype") <- "add"

## ----ex02---------------------------------------------------------------------
emp  <- X13Series(
  data.frame(memp[,c("year","period")], value = memp[,"value"] + femp[,"value"]),
  sname = "emp",
  composite = list(title = "emp", save = "(isf isa itn b1 id8 iir)"),
  x11 = list(mode = "add", sigmalim = "(1.8,2.8)", save = "(e1 c17 d8 d9 d10 d11 d12 d13)")
)

## ----ex03---------------------------------------------------------------------
empgrp <- X13SeriesGroup("employment", memp, femp, emp)

## ----ex04---------------------------------------------------------------------
empgrp.res <- adjust(empgrp)

## ----ex05---------------------------------------------------------------------
plot(empgrp.res[["emp"]], type = "d10")

