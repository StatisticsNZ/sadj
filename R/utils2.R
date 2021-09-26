#' @keywords internal
findX13File <- function(fname, rel_dir=getwd()){

  if(file.exists(fname)) return(normalizePath(fname) %>% sub(path.expand("~"),"~",.))

  if(stringr::str_detect(fname,sprintf("\\\\[^\\\\]+?[.][a-zA-Z]{3}$")))
    dir_delim <- "\\\\"
  else dir_delim <-"/"


  rel_base <- basename(rel_dir)

  try_file <- sprintf("%s/%s",rel_dir,fname)

  if(file.exists(try_file)) return(normalizePath(try_file) %>% sub(path.expand("~"),"~",.))

  warning(sprintf("couldn't find file in:\n%s.
                  Searching paths relative to:\n%s.", dirname(try_file),rel_dir))

  path_split <- stringr::str_split(fname, dir_delim)[[1]] %>% rev()

  for(i in seq_along(path_split)) {
    if(path_split[[i]]==rel_base) {
      try_file <- paste(path_split[(i-1):1],collapse = "/") %>% sprintf("%s/%s",rel_dir,.)
      if(file.exists(try_file)){
        return(normalizePath(try_file) %>% sub(path.expand("~"),"~",.))
      }
    }

  }

  # for(i in seq_along(path_split)) {
  #   try_file <- paste(path_split[i:1],collapse = dir_delim)
  #   if(file.exists(try_file)){
  #     try_file <- normalizePath(try_file)
  #     # could make this path regex friendly and search beginning only ...
  #     try_file <- sub(path.expand("~"),"~",try_file)
  #     return(try_file)
  #   }
  # }

  stop(sprintf("Unable to find file:\n%s.", fname))

}
