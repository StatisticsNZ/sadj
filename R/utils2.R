#' @keywords internal
findX13File <- function(fname, rel_dir=getwd(), return_full = TRUE){

  if(file.exists(fname)) return(normalizePath(fname) %>% sub(path.expand("~"),"~",.))

  if(stringr::str_detect(fname,sprintf("\\\\[^\\\\]+?[.][a-zA-Z]{3}$")))
    dir_delim <- "\\\\"
  else dir_delim <-"/"


  rel_base <- basename(rel_dir)
  # rel_base <- rel_dir

  try_file <- sprintf("%s/%s",rel_dir,fname)

  if(file.exists(try_file)) return(normalizePath(try_file) %>% sub(path.expand("~"),"~",.))

  # warning(sprintf("couldn't find file in:\n%s.
  #                 Searching paths relative to:\n%s.", dirname(try_file),rel_dir))

  path_split <- stringr::str_split(fname, dir_delim)[[1]] %>% rev()

  if(dir_delim == "\\\\") {

    for(i in seq_along(path_split)) {
      if(tolower(path_split[[i]])== tolower(rel_base)) {
        end_bit <- paste(path_split[(i-1):1],collapse = "/")
        end_bit <- rel_dir %>%
          list.files(recursive = TRUE) %>%
          grep(rex::rex(start, end_bit, end), . ,value=TRUE, ignore.case = TRUE)

        try_file <- end_bit %>% sprintf("%s/%s",rel_dir,.)

        if(rlang::is_empty(try_file)) {
          stop(sprintf("Couldn't find file:\n%s.", fname))
        } else if(file.exists(try_file)) {

          found_file <- sub(path.expand("~"),"~",normalizePath(try_file))

          warning(sprintf("Couldn't find file:\n%s.\nFound file:\n%s."
                          , fname
                          , found_file)
                  )
          if(return_full)
            return(found_file)
          else
            return(end_bit)
        }

      }
    }

  } else {

    for(i in seq_along(path_split)) {
      if(path_split[[i]]==rel_base) {
        rel_file <- paste(path_split[(i-1):1],collapse = "/")
        try_file <- rel_file %>% sprintf("%s/%s",rel_dir,.)
        if(file.exists(try_file)){
          found_file <- sub(path.expand("~")
                            ,"~",normalizePath(try_file))

          warning(
            sprintf("Couldn't find file:\n%s.\nFound file:\n%s."
                    , fname
                    , found_file
                    )
            )
          if(return_full)
            return(found_file)
          else
            return(rel_file)
        }
      }
    }

  }

  stop(sprintf("Unable to find file:\n%s.", fname))

}

#' @export
outliervarsToDate <- function(otl_vars, pd) {
  otl_vars %>% map_dbl(function(x){
    res <- x %>% str_remove("ao") %>% str_remove("ls") %>% str_remove("tc")
    x13DateToDate(res, pd)
  }) %>% as_date()

}

#' @export
x13DateToDate <- function(x, pd) {

  match_res <- x %>% str_match("^(\\d{4})\\.(\\d{1,2}|[:alpha:]{3})$")
  y <- as.numeric(match_res[2])
  p <- match_res[3] %>% tolower()

  if(p %in% (m_abb <- tolower(month.abb))) {
    if( pd == 12 )
      p <- match(p, m_abb)
    else
      stop("X13 does not accept month abbreviations for quarterly series.")
  }

  p <- as.numeric(p)
  sprintf("%04d-%02d-01", y, 12/pd * p) %>% as.Date(origin = "1970-01-01")
}
