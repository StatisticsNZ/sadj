#' @export
"addOutliers<-" <- function(x, ...){
  UseMethod("addOutliers<-", x)
}

#' @export
"addParamVals<-" <- function(x, ...){
  UseMethod("addParamVals<-", x)
}


#' @export
"appendSpecComments<-" <- function(x, ...){
  UseMethod("appendSpecComments<-", x)
}

#' @export
correctARIMA <- function(x, ...){
  UseMethod("correctARIMA", x)
}

#' @export
correctSeriesSpec <- function(x, ...){
  UseMethod("correctSeriesSpec", x)
}

#' @export
getFacFile <- function(x, ...){
  UseMethod("getFacFile", x)
}

#' @export
getParamVals <- function(x, ...){
  UseMethod("getParamVals", x)
}

#' @export
getPeriod <- function(x, ...){
  UseMethod("getPeriod", x)
}

#' @export
getRegFile <- function(x, ...){
  UseMethod("getRegFile", x)
}

#' @export
getRegVars <- function(x, ...){
  UseMethod("getRegVars", x)
}

#' @export
getSpec <- function(x, ...){
  UseMethod("getSpec", x)
}

#' @export
getSpecComments <- function(x, ...){
  UseMethod("getSpecComments", x)
}

#' @export
getSpecList <- function(x, ...){
  UseMethod("getSpecList", x)
}

#' @export
getSpecParameter <- function(x, ...){
  UseMethod("getSpecParameter", x)
}

#' @export
hasFac <- function(x, ...){
  UseMethod("hasFac", x)
}

#' @export
hasReg <- function(x, ...){
  UseMethod("hasReg", x)
}

#' @export
"removeOutliers<-" <- function(x, ...){
  UseMethod("removeOutliers<-", x)
}

#' @export
"removeParamVals<-" <- function(x, ...){
  UseMethod("removeParamVals<-", x)
}

#' @export
removeFacFile <- function(x, ...){
  UseMethod("removeFacFile", x)
}

#' @export
removeSpec <- function (x, ...){
  UseMethod("removeSpec", x)
}

#' @export
selectSeries <- function (x, ...){
  UseMethod("selectSeries", x)
}

#' @export
"setFacFile<-" <- function(x, ...){
  UseMethod("setFacFile<-", x)
}

#' @export
"setParamVals<-" <- function(x, ...){
  UseMethod("setParamVals<-", x)
}

#' @export
"setRegFile<-" <- function(x, ...){
  UseMethod("setRegFile<-", x)
}

#' @export
setRegVars <- function(x, ...){
  UseMethod("setRegVars", x)
}

#' @export
"setSpecParameter<-" <- function(x, ...){
  UseMethod("setSpecParameter<-", x)
}


#' @export
"setSpec<-" <- function(x, ...){
  UseMethod("setSpec<-", x)
}

#' @export
"setSpecComments<-" <- function(x, ...){
  UseMethod("setSpecComments<-", x)
}

#' @export
"setSpecList<-" <- function(x, ...){
  UseMethod("setSpecList<-", x)
}


#' @export
writeSpecToFile <- function(x, ...){
  UseMethod("writeSpecToFile", x)
}


#' @export
adjust <- function(x, ...){
  UseMethod("adjust", x)
}

#' @export
`%+%` <- function(x, ...){
  UseMethod("%+%", x)
}

#' @export
sname <- function(x, ...){
  UseMethod("sname", x)
}

#' @export
lname <- function(x, ...){
  UseMethod("lname", x)
}

#' @export
view <- function(x, ...){
  UseMethod("view", x)
}

#' @export
specType <- function(x, ...){
  UseMethod("specType", x)
}

#' @export
tvals <- function(x, ...){
  UseMethod("tvals", x)
}

#' @export
X13Messages <- function(x, ...){
  UseMethod("X13Messages", x)
}

#' @export
Sum <- function(x, ...){
  UseMethod("Sum", x)
}

#' @export
path <- function(x, ...){
  UseMethod("path", x)
}

#' @export
X11AddMult <- function(x, ...){
  UseMethod("X11AddMult", x)
}
