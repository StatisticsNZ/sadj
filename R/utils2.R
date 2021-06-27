#' @keywords internal
findX13File <- function(fname, rel_dir=NULL){

  if (file.exists(fname)) return(fname)
  rel_base <- basename(rel_dir)

  # we don't want to error if specdir is null or not supplied
  if(!missing(rel_dir)) {
    if(dir.exists(rel_dir)) {
      userwd <- getwd()
      setwd(rel_dir)
      on.exit(setwd(userwd), add = TRUE)

    } else {
      warning(sprintf("File directory does not exist:\n%s.  Current working directory will be used to find files.",rel_dir))
    }

  }

  warning(sprintf("couldn't find file in:\n%s.
                  Searching paths relative to:\n%s.", fname,getwd()))

  path_split <- stringr::str_split(fname, "/")[[1]] %>% rev()

  for(i in seq_along(path_split)) {
    if(path_split[[i]]==rel_base) {
      try_file <- paste(path_split[(i-1):1],collapse = "/")
      if(file.exists(try_file)){
        try_file <- normalizePath(try_file)
        # could make this path regex friendly and search beginning only ...
        try_file <- sub(path.expand("~"),"~",try_file)
        return(try_file)
      }
    }

  }

  # for(i in seq_along(path_split)) {
  #   try_file <- paste(path_split[i:1],collapse = "/")
  #   if(file.exists(try_file)){
  #     try_file <- normalizePath(try_file)
  #     # could make this path regex friendly and search beginning only ...
  #     try_file <- sub(path.expand("~"),"~",try_file)
  #     return(try_file)
  #   }
  # }

  stop(sprintf("Unable to find file:\n%s.", fname))

}
