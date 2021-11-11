

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

#' Convert mta group to a list of X13Series
#'
#' @keywords internal
mtaToX13Series <- function(mta_grp) {
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

#' Convert mta group to a list of X13Series
#'
#' @keywords internal
X13ListToGroup <- function(ser_list,sname="1") {


  if(length(ser_list)>1) {
    last_ser <- ser_list[[length(ser_list)]]
    # Error if first series are not series
    is_composite <- ser_list[1:(length(ser_list)-1)] %>% map_chr(specType) %>% `==`("composite")
    if(any(is_composite)) {
      stop(sprintf("Composite series found inside group: %s.\nOnly one is permitted at the end."
                   , names(ser_list)[is_composite] %>% paste(collapse=" ")))
    }

    if(specType(last_ser)=="composite") {

      comp_vals <- map(ser_list[1:(length(ser_list)-1)], function(x){
        comptype <- attr(x,"SpecList") %>% getSpecParameter("series","comptype")
        if(comptype =="sub")
          x$value <- x$value * -1
        x %>% dplyr::select(date, year, period, value)
      })

      join_vals <- comp_vals %>% reduce(function(x,y){
        inner_join(x,y, by=c("date", "year", "period"))

      })

      comp_dat <- join_vals %>%
        transmute(date=date, year=year, period=period
                  , value = rowSums(across(starts_with("value"))))

      last_ser <- X13Series(x=comp_dat
                ,sname=sname(last_ser)
                ,lname=lname(last_ser)
                ,speclist = getSpecList(last_ser)
                , facfile = getFacFile(last_ser)
                , regfile=getRegFile(last_ser))

      ser_list[[length(ser_list)]] <- last_ser

    }

  }

  # create X13Group
  ser_list$sname <- sname
  do.call(sadj:::X13SeriesGroup
          ,ser_list)

}


