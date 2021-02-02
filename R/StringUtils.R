

# From StringUtils.scala ----------------------

# Miscellaneous methods for working with strings.

#' indent
#' @param s: String, size: Int
#' @return String
#' @keywords internal
indent <- function(s, size){
  s %>% str_replace("\n", "\n" %+% (1:size %>% map_chr(~ " ") %>% mkString()))
}

#' unquote
#' @param s: String
#' @return String
#' @keywords internal
unquote <- function(s){
  if ((take(s,1) == "\"" & takeRight(s,1) == "\"") | (take(s,1) == "'" & takeRight(s,1) == "'")) {
    s %>% drop(1) %>% dropRight(1)} else {
      s
    }
}


escape <- function(s){
  str_replace(s,"\"", "\\\"") %>% str_replace("\n", "\\n")
}

