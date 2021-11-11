#' @keywords internal
findX13File <- function(fname, rel_dir=getwd()){

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
        if(file.exists(try_file)){
          warning(sprintf("Couldn't find file:\n%s.\nFound file:\n%s.", fname
                          , (found_file <- sub(path.expand("~"),"~",normalizePath(try_file)))))
          return(found_file)
        }
      }
    }

  } else {

    for(i in seq_along(path_split)) {
      if(path_split[[i]]==rel_base) {
        try_file <- paste(path_split[(i-1):1],collapse = "/") %>% sprintf("%s/%s",rel_dir,.)
        if(file.exists(try_file)){
          warning(sprintf("Couldn't find file:\n%s.\nFound file:\n%s.", fname
                          , (found_file <- sub(path.expand("~"),"~",normalizePath(try_file)))))
          return(found_file)
        }
      }
    }

  }



  stop(sprintf("Unable to find file:\n%s.", fname))

}
