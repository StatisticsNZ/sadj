

#' Get lists of mta groups from mta file.
#'
#' @keywords internal
mtaGroups <- function(fname) {
  mta <- readLines(fname)
  grp <- vector(length = length(mta))
  grp_inc <- 1
  for(i in seq_along(mta)){
    if(mta[[i]] =="") {
      grp_inc <- grp_inc + 1
      grp[[i]] <- 0
    } else
      grp[[i]] <- grp_inc
  }

  mta_grps <- base::split(mta, grp)
  mta_grps[["0"]] <- NULL

  attr(mta_grps,"path") <- fname %>% normalizePath() %>% dirname() %>% sub(path.expand("~"),"~",.)
  for(i in seq_along(mta_grps)){
    # mta_grps[[i]] %>%
    attr(mta_grps[[i]],"path") <- attr(mta_grps,"path")
  }

  mta_grps
}

#' Convert mta group to X13 group
#'
#' @keywords internal
mtaToX13 <- function(mta_grp) {
  # loop through each spec in group and return an X13 Series
  res <- mta_grp %>% map(function(x){
    spec_fname <- strsplit(x, "\\s+")[[1]][[1]] %>% paste0(".spc")
    spec_fname <- findX13File(spec_fname, rel_dir=attr(mta_grp,"path"))
    # readSPC(spec_fname)
    X13Series(spec_fname = spec_fname)


  })

  names(res) <- map_chr(res,sname)
  res
  # turn the resulting list into an X13group
}

#' @keywords internal
X13ListToGroup <- function(ser_list) {


  if(length(ser_list)>1) {
    last_ser <- ser_list[[length(ser_list)]]
    if(specType(last_ser)=="composite") {
      comp_vals <- map(ser_list[1:(length(ser_list)-1)], function(x){
        comptype <- attr(x,"SpecList") %>% getSpecParameter("series","comptype")
        if(comptype =="sub")
          x$value <- x$value * -1
        x$value
      })

      sum_vals <- reduce(comp_vals, `+`)
      comp_dat <- data.frame(date = ser_list[[1]]$date, year = ser_list[[1]]$year
                             , period = ser_list[[1]]$period, value = sum_vals)

      last_ser <- X13Series(x=comp_dat
                ,sname=sname(last_ser)
                ,lname=lname(last_ser)
                ,speclist = getSpecList(last_ser)
                , facfile = getFacFile(last_ser))
    }
    # ser_list[[attr(last_ser,"sname")]] <- last_ser
  }

  # create X13Group
  # ser_list$sname <- x_name
  do.call(sadj:::X13SeriesGroup
          ,ser_list)

}
