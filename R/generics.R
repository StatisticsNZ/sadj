#' @export
getFacFile <- function(x, ...){
  UseMethod("getFacFile", x)
}

#' @export
"setFacFile<-" <- function(x, ...){
  UseMethod("setFacFile<-", x)
}

#' @export
getRegFile <- function(x, ...){
  UseMethod("getRegFile", x)
}

#' @export
"setRegFile<-" <- function(x, ...){
  UseMethod("setRegFile<-", x)
}

#' @export
getSpecParameter <- function(x, ...){
  UseMethod("getSpecParameter", x)
}

#' @export
getSpec <- function(x, ...){
  UseMethod("getSpec", x)
}

#' @export
getSpecList <- function(x, ...){
  UseMethod("getSpecList", x)
}

#' @export
getSpecComments <- function(x, ...){
  UseMethod("getSpecComments", x)
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
"setSpecList<-" <- function(x, ...){
  UseMethod("setSpecList<-", x)
}


#' @export
"setSpecComments<-" <- function(x, ...){
  UseMethod("setSpecComments<-", x)
}

#' @export
"appendSpecComments<-" <- function(x, ...){
  UseMethod("appendSpecComments<-", x)
}

#' @export
writeSpecToFile <- function(x, ...){
  UseMethod("writeSpecToFile", x)
}

#' @export
removeSpec <- function (x, ...){
  UseMethod("removeSpec", x)
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
