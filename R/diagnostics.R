#' Read UDG file.
#'
#' @keywords internal
readUDG <- function(x, outpdir, ext = "udg"){
  if (!is.X13Series(x))
    stop(sprintf("%s must be of class X13Series", deparse(substitute(x))))

  fname <- sprintf("%s/%s.%s", outpdir, attr(x, "sname"), ext)

  if (!file.exists(fname))
    stop(sprintf("UDG file not found:\n  %s.", fname))

  f <- file(fname, "r")
  if (!isOpen(f, "r"))
    stop(sprintf("Failed to open UDG file for reading:\n  %s.", fname))

  l <- readLines(f)
  o <- structure(list(), class = "X13Diagnostics")
  pos <- 1

  for (i in 1:length(l)){
    s <- strsplit(rmdups(ltrim(rtrim(l[i]))), ':')[[1]]
    for (j in 1:length(s))
      s[j] <- rmdups(ltrim(rtrim(s[j])))
    n <- s[1]
    if (!n%in%c("date", "time", "version", "build", "span", "startspec", "srstit")){
      v <- s[2:length(s)]
      o[[pos]] <- v
      names(o)[[pos]] <- n
      pos <- pos + 1
    }
    i <- i + 1
  }
  close(f)
  return(o)
}

#' Coerce \code{X13Diagnostics} object to \code{\link{data.frame}}.
#'
#' @export
as.data.frame.X13Diagnostics <- function(
  x,
  statfilter = c(
    "f2.mcd", "f2.ic", "f2.is", "f2.msf",
    sprintf("f3.m%02d", 1:11), "f3.q", "f3.qm2"
  ),
  colfilter = c("stat", "value")
) {
  ll <- list()
  for (name in names(x)){
    stat <- strsplit(name, '.', fixed = TRUE)[[1]]
    nstat <- length(stat)
    stat <- head(c(stat, rep(NA, 4)), 4)
    this <- do.call('data.frame', as.list(c(name, stat, x[[name]])))
    colnames(this) <- c("stat", sprintf("stat.%s", 1:4), "value")
    ll[[length(ll) + 1]] <- this
  }
  res <- do.call('rbind', ll)
  if (length(statfilter) > 0)
    res <- subset(res, stat %in% statfilter)
  colfilter <- colfilter[colfilter %in% colnames(res)]
  if (length(colfilter) > 0)
    res <- res[, colfilter]
  res
}


