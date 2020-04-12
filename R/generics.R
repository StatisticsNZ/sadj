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
"setSpecParameter<-" <- function(x, ...){
  UseMethod("setSpecParameter<-", x)
}

#' @export
"setSpec<-" <- function(x, ...){
  UseMethod("setSpec<-", x)
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
