#' Trim whitespace on left of string.
#'
#' @keywords internal
ltrim <- function(s){
  t <- s
  while(substr(t, 1, 1) == ' ' & nchar(t) > 1)
    t <- substr(t, 2, nchar(t))
  t
}

#' Trim whitespace on right of string.
#'
#' @keywords internal
rtrim <- function(s){
  t <- s
  while(substr(t, nchar(t), nchar(t)) == ' ' & nchar(t) > 1)
    t <- substr(t, 1, nchar(t) - 1)
  t
}

#' Trim whitespace
#'
#' @keywords internal
trim <- function(s) ltrim(rtrim(s))

#' Remove quotes from string.
#'
#' @keywords internal

unquote <- function(s) {
  expr <- "^\" ?(.*?) ?\"$|^\' ?(.*?) ?\'$"
  res <- str_match(s, expr)
  if(is.na(res[,1])) return(s)
  if(!is.na(res[,2])) str_trim(res[,2]) else str_trim(res[,3])

}

unparen <- function(s) {
  expr <- "^\\( ?(.*?) ?\\)$"
  res <- str_match(s, expr)
  if(is.na(res[,1])) return(s)
  str_trim(res[,2])

}

unquote0 <- function(s) {
  # if (grepl(" ", s)) return(s)
  if (substr(s, 1, 1) %in% c("\'", "\"") &
      substr(s, nchar(s), nchar(s)) %in% c("\'", "\""))
    substr(s, 2, nchar(s) - 1)
  else s
}

#' Replace blocks adjacent matching characters with single character in string.
#'
#' @keywords internal
rmdups <- function(s, c = ' '){
  if (nchar(s) < 2) return(s)
  t <- substr(s, 1, 1)
  pos <- 1
  for (i in 2:nchar(s)){
    if (substr(s, i, i) != c || substr(t, pos, pos) != c){
      t <- paste0(t, substr(s, i, i))
      pos <- pos + 1
    }
  }
  return(t)
}

#' String concatenation that eats unnecessary whitespace.
#'
#' @keywords internal
`%+%` <- function(a, b) {
  lr <- substr(a, nchar(a), nchar(a)) == " "
  rl <- substr(b, 1, 1) == " "
  rr <- substr(b, nchar(b), nchar(b)) == " "
  if (a == "") ltrim(b)
  else if (b == "") paste0(ltrim(a), if (lr) " ")
  if (a == " " & b == " ") ""
  else if (a == " ") ltrim(b)
  else if (b == " ") paste0(trim(a), " ")
  else paste0(trim(a), if (lr | rl) " ", trim(b), if (rr) " ")
}

#' Parse a string into individual values.
#'
#' @keywords internal
split <-function(str) {
  squote <- 0
  dquote <- 0
  buff <- ""
  accum <- c()
  for (i in 1:nchar(str)) {
    ch <- substr(str, i, i)
    quote <- (squote + dquote) %% 2 != 0
    if (ch == "'") {
      squote <- squote + 1
      if (quote) {
        accum <- c(accum, trim(paste0(buff, ch)))
        buff <- ""
      } else {
        buff <- paste0(buff, ch)
      }
    } else if (ch == '"') {
      if (ch == "'") {
        dquote <- dquote + 1
        if (quote) {
          accum <- c(accum, trim(paste0(buff, ch)))
          buff <- ""
        } else {
          buff <- paste0(buff, ch)
        }
      }
    } else if (ch == " " & !quote) {
      if (buff != "") {
        accum <- c(accum, trim(buff))
        buff <- ""
      }
    } else {
      buff <- paste0(buff, ch)
    }
  }
  if (nchar(trim(buff)) != 0) c(accum, buff)
  else accum
}

#' Gather a list of values into a string, with linebreaks where required.
#'
#' @keywords internal
gather <- function(xs, max = 132, indent = 4) {
  buff <- ""
  accum <- ""
  for (x in xs) {
    if (nchar(sprintf("%s %s", buff, x)) > (max - indent)) {
      accum <- sprintf(
        "%s\n%s%s", accum, paste(rep(' ', indent), collapse = ""), buff
      )
      buff <- x
    } else
      buff <- sprintf("%s%s%s", buff, if (nchar(buff) == 0) "" else " ", x)
  }
  if (nchar(buff) != 0)
    sprintf("%s\n%s%s", accum, paste(rep(' ', indent), collapse = ""), buff)
  else
    accum
}

#' Convert time series object to a suitable data frame.
#'
#' @param x Object of class \code{\link{ts}}.
#'
#' @export
#'
#' @examples
#' ap <- tsdf(AirPassengers)
#' ap
tsdf <- function(x){
  year <- floor(time(x))
  pd   <- cycle(x)
  data.frame(year = as.numeric(year),
             period = as.numeric(pd),
             value = as.numeric(x))
}

#' @keywords internal
date <- function(x, y = "year", p = "period"){
  pd <- unique(x[, p])
  if (length(pd) == 4 & max(pd) == 4) x[, p] <- x[, p] * 3
  as.Date(sprintf("%04d-%02d-01", data.frame(x)[, y], data.frame(x)[,p]), origin = "1970-01-01")
}

#' @keywords internal
workdir <- function(){
  res <- sprintf("%s/%s", tempdir(), "sadj")
  if (!dir.exists(res))
    if(!dir.create(res))
      stop("Failed to create working directory.")
  res
}

#' @keywords internal
outpdir <- function(){
  res <- sprintf("%s/o", workdir())
  if (!dir.exists(res))
    if(!dir.create(res))
      stop("Failed to create output directory.")
  res
}

#' @keywords internal
datdir <- function(){
  res <- sprintf("%s/d", workdir())
  if (!dir.exists(res))
    if(!dir.create(res))
      stop("Failed to create data directory.")
  res
}

#' @keywords internal
facdir <- function(){
  res <- sprintf("%s/f", workdir())
  if (!dir.exists(res))
    if(!dir.create(res))
      stop("Failed to create factor directory.")
  res
}

#' Read DAT file.
#'
#' @param fname file name
#'
#' @export
readDAT <- function(fname){
  if (!file.exists(fname))
    stop(sprintf("No such file: %s.", deparse(substitute(fname))))
  res <- read.table(fname, col.names = c("year", "period", "value"))
  res
}

#' Write DAT file.
#'
#' @keywords internal
writeDAT <- function(x, fname){
  if (inherits(x, "X13Series")){
    # x %<>% mutate(value=sprintf("%12s",as.character(value)))
    # x %<>% mutate(period=sprintf("%-2s",as.character(period)))
    write.table(select(x,year, period, value), file= fname, sep = " "
                , row.names = FALSE,col.names = FALSE, quote=FALSE)
  }
}

#' Write DAT file.
#'
#' @keywords internal
writeDAT1 <- function(x, fname){
  if (inherits(x, "X13Series")){
    sink(fname)
    paste(paste(x$year, x$period, x$value, sep = "\t"), collapse = "\n")
    sink()
  }
}

#' Read FAC file.
#'
#' @param fname file name
#'
#' @export
readFAC <- function(fname){
  if (!file.exists(fname))
    stop(sprintf("No such file: %s.", deparse(substitute(fname))))
  read.table(fname)
}

#' Write FAC file.
#'
#' @keywords internal
writeFAC <- function(x, fname){
  write.table(x, file= fname, row.names = FALSE,col.names = FALSE)
}

#' Read SPC file.
#'
#' @param fname file name
#'
#' @export
readSPC <- function(fname, to_lower=TRUE){

  res <- SPCparser$parseSPC(fname, to_lower=to_lower) %>% parsedSpecToX13SpecList()

  # add a fac_name if a `file` argument exists in the `transform` specification
  fac_name <- getSpecParameter(res,"transform","file")
  expr_facname <- "([a-zA-Z]+[a-zA-Z0-9]*)[\\.]fac$"

  if(!is_empty(fac_name) && str_detect(fac_name,expr_facname)) {
    attr(res, "fac_name") <- str_match(fac_name,expr_facname)[,2]
  } else attr(res, "fac_name") <- character()

  return(res)
}

#' Read all SPC's in path.
#'
#' @param fname file name
#'
#' @keywords internal
readSPCsFromDir <- function(path){
  if (!dir.exists(path))
    stop("Directory does not exist.")

  # path %<>% str_replace("[^////]$",function(x){
  #   paste0(x,"/")
  # })

  path %<>% str_remove("[////]$")

  all_spcs <- list.files(path, full.names = TRUE, pattern = "\\.spc$")
  names(all_spcs) <- list.files(path, pattern = "\\.spc$") %>% str_remove("\\.spc$")
  res <- all_spcs %>% map(readSPC)
  class(res) <- "X13specCol"
  return(res)

}


#' Read SPC file.
#'
#' @param fname file name
#'
#' @keywords internal
readSPC1 <- function(fname){
  if (!file.exists(fname))
    stop("File does not exist.")
  if (!isOpen(f <- file(fname, "rb")))
    stop("Unable to open file for reading.")
  res <- list()
  comments <- vector(mode="character")
  is_comment <- FALSE        # after '#' until end of line
  block <- FALSE          # inside { ... }
  innerblock <- FALSE     # inside ( ... )
  rhs <- FALSE            # after = until } or newline if !block
  quote <- FALSE          # inside a quoted string
  numsquote <- 0          # number of single quotes
  numdquote <- 0          # number of double quotes
  spc <- ""               # name of spec, e.g. x11, series, outlier, etc.
  name <- ""              # name of parameter, e.g. title, sigmalim, etc.
  val <- ""               # value of paramter
  i <- 1                  # index of spec
  while (length(ch <- readChar(f, 1)) > 0){

    '
    cat(sprintf(
      "ch: % 2s, spc: %s, name: %s, val: %s, dquote: %d, squote: %d, quote: %s\n",
      if (ch == "\n") "\\n" else if (ch == "\r") "\\r" else ch,
      spc, name, val,
      numdquote, numsquote, quote
    ))
    '

    if (ch == "#" & !quote) is_comment <- TRUE
    if (ch %in% c("\n", "\r")) {
      if(is_comment) {
        comments <- c(comments, val)
        val <- ""
      }
      is_comment <- FALSE
    }
    if (is_comment) {
      val <- val %+% ch
      next
    }

    quote <- (numdquote %% 2 != 0) | (numsquote %% 2 != 0)

    # _should_ only encounter parenthesis on the RHS of parameters values.
    if (ch == '"') {
      numdquote <- numdquote + 1
      if (innerblock) val <- if (trim(val) == "(") "(" %+% ch else val %+% ch
      else if (rhs) {
        val <- trim(val) %+% ch
        if (quote) {
          res[[i]][["args"]][[name]] <- unquote(val)
          rhs <- FALSE
          name <- ""
          val <- ""
        }
      }
    }
    else if (ch == "'") {
      numsquote <- numsquote + 1
      if (innerblock) val <- if (trim(val) == "(") "(" %+% ch else val %+% ch
      else if (rhs) {
        val <- trim(val) %+% ch
        if (quote) {
          res[[i]][["args"]][[name]] <- unquote(val)
          rhs <- FALSE
          name <- ""
          val <- ""
        }
      }
    }
    else if (quote) val <- val %+% ch
    else if (ch %in% c("\n", "\r")) {
      if (innerblock)
        val <- trim(val) %+% " "
      else if (rhs) {
        res[[i]][["args"]][[name]] <- trim(val)
        name = ""
        val = ""
        rhs = FALSE
      }
    }
    else if (ch == "=")  rhs <- TRUE
    else if (ch == "{") {
      block <- TRUE
      res[[i]] <- structure(list(), class = "X13Spec")
      res[[i]][["name"]] <- spc
      res[[i]][["args"]] <- list()
    }
    else if (ch == "}"){
      if (name != "" & val != "") res[[i]][["args"]][[name]] <- trim(val)
      rhs <- FALSE
      name <- ""
      val <- ""
      block <- FALSE
      i <- i + 1
      spc <- ""
    }
    else if (ch == "(") {
      val <- val %+% ch
      innerblock <- TRUE
    }
    else if (ch == ")") {
      # arima model can have multiple close parentheses.
      #Should try to handle this as a special case with regex
      # transform format also has multiple close parentheses but they are in quotes
      val <- paste0(trim(val), ch)
      innerblock <- FALSE
      if(!(spc=="arima" && name=="model")) {
        res[[i]][["args"]][[name]] <- trim(val)
        rhs <- FALSE
        name <- ""
        val <- ""
      }

    }
    else {
      if (rhs) val <- if (trim(val) == "(") "(" %+% ch else val %+% ch
      else if (block) name <- tolower(trim(name %+% ch))
      else spc <- tolower(trim(spc %+% ch))
    }
  }
  close(f)
  for (i in 1:length(res))
    names(res)[i] <- res[[i]][[1]]
  comments <- X13SpecComments(comments)
  attr(res, "comments") <- comments
  structure(res, class = "X13SpecList")

}

#' Read SPC file.
#'
#' This is the original version, containing bugs.
#' It will be removed after readSPC has been tested fully.
#'
#' @param fname file name
#'
#' @keywords internal
readSPC0 <- function(fname){
  if (!file.exists(fname))
    stop("File does not exist.")
  if (!isOpen(f <- file(fname, "rb")))
    stop("Unable to open file for reading.")
  res <- list()
  comment <- FALSE
  block <- FALSE
  innerblock <- FALSE
  rhs <- FALSE
  prev <- ""
  spc <- ""
  name <- ""
  val <- ""
  i <- 1
  while (length(ch <- readChar(f, 1)) > 0){
    if (ch == "#") comment <- TRUE
    if (ch %in% c("\n", "\r")) comment <- FALSE
    if (!comment){
      if (ch == "="){
        rhs <- TRUE
      }
      else if (ch == "{"){
        block <- TRUE
        res[[i]] <- structure(list(), class = "X13Spec")
        res[[i]][["name"]] <- spc
        res[[i]][["args"]] <- list()
      }
      else if (ch == "}"){
        if (!prev %in% c("\n", "\r")){
          val <- unquote(ltrim(rtrim(val)))
          if (name != "") res[[i]][["args"]][[name]] <- val
          rhs <- FALSE
          name <- ""
          val <- ""
        }
        block <- FALSE
        i <- i + 1
        spc <- ""
      }
      else if (ch %in% c("\n", "\r")){
        if (!innerblock){
          if (name != "" & val != ""){
            val <- unquote(ltrim(rtrim(val)))
            res[[i]][["args"]][[name]] <- val
          }
          rhs <- FALSE
          name <- ""
          val <- ""
        }
      }
      # if space
      else if (ch == " "){
        # if previous character was not newline or carriage return or space AND it is on RH of equation
        if (!prev %in% c("\n", "\r") & prev != " " & rhs)
          # append to value
          val <- paste(val, ch, sep = "")
      }
      else{
        if (ch == "(") innerblock <- TRUE
        if (ch == ")") innerblock <- FALSE
        if (!block)
          spc <- paste(spc, ch, sep = "")
        else if (!rhs)
          name <- paste(name, ch, sep = "")
        else val <- paste(val, ch, sep = "")
      }
      prev <- ch
    }
  }
  close(f)
  for (i in 1:length(res))
    names(res)[i] <- res[[i]][[1]]
  structure(res, class = "X13SpecList")
}

#' Read output files.
#'
#' @keywords internal
readOutput <- function(x, outpdir, ext="d11"){
  if (!is.X13Series(x))
    stop(sprintf("%s must be of class X13Series", deparse(substitute(x))))

  fname <- sprintf("%s/%s.%s", outpdir, attr(x, "sname"), ext)

  if (!file.exists(fname))
    stop(sprintf("No such file: %s.", deparse(substitute(fname))))

  infile <- file(fname, "rb")

  #burn the first 2 lines
  readLines(infile, n = 2)

  aLine <- readLines(infile, n = 1)
  yr  <- c()
  pd  <- c()
  res <- c()

  while (length(aLine) > 0){
    tmp <- unlist(strsplit(aLine,"\t"))
    yr  <- c(yr, floor(as.numeric(tmp[1]) / 100))
    pd  <- c(pd, as.numeric(tmp[1]) - floor(as.numeric(tmp[1]) / 100) * 100)
    res <- c(res, as.numeric(tmp[2]))
    aLine <- readLines(infile, n=1)
  }

  close(infile)

  return(structure(data.frame(yr, pd, res), names = c("year", "period", ext)))
}

#' Merge list of data frames.
#'
#' @keywords internal
mergeList <- function(x, ...){
  if(!all(sapply(x, is.data.frame)))
    stop(sprintf("%s must be a list of data frames.", deparse(substitute(x))))
  if (length(x) == 1) return(x[[1]])
  res <- x[[1]]
  for (i in 2:length(x))
    # added all=TRUE.  Some output files like e6 were missing some dates
    res <- merge(res, x[[i]], all=TRUE,...)
  res[order(res$year, res$period), ]
}
