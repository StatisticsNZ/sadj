
#' A closure of functions used to parse an X13 .spc file.
#'
#' @description
#' \code{parseSPC} pipes the child functions together. Its output
#'   gets passed to parsedSpectoX13Spec which creates X13 classes.
#'
#' @return A list of functions useful for parsing spec files.
#'
#' @section stripComment:
#'
#' Remove comments from a specification. Takes mapped output from readLines
#' Comments are denoted by #.  Remove any lines that start with #, or remove
#' content after # if it appears part-way through a line.
#' \subsection{Arguments}{
#' \describe{
#'   \item{str}{The body of a SPC file.}
#' }}
#' \subsection{Value}{A 1-element character: a SPC file line with comments removed.}
#'
#' @section preprocess:
#' Remove newlines, remove comments, and squish whitespace from the content
#'   of a SPC file.
#' \subsection{Arguments}{
#' \describe{
#'   \item{str}{multiple-length chatacter: The body of a SPC file.}
#' }}
#' \subsection{Value}{A character vector of length 1 containing a tidier
#'   , squished version of the body of a SPC file.}
#'
#' @section parseSpecs:
#' Split body of SPC files into individual specs:
#'
#' Extract instances of the pattern spec \{ stuff \}, where spec is
#'   , for example, \code{series}, \code{x11}, etc.
#'
#' \subsection{Arguments}{
#' \describe{
#'   \item{str}{The output of preprocess: a character vector of length 1.}
#' }}
#' \subsection{Value}{A character vector with length equal to the number of specs.}
#'
#' @section parseSpecNameAndBody:
#' Convert a list of specifications a named list of values.
#'
#' \subsection{Arguments}{
#' \describe{
#'   \item{str}{The output of parseSpecs:
#'     A character vector with length equal to the number of specs.}
#' }}
#' \subsection{Value}{A named list of values.}
#'
#'
#' @section parseArgs:
#' Parse Arguments:
#'   The output of parseSpecNameAndBody maps to this function.
#' \subsection{Arguments}{
#' \describe{
#'   \item{str}{A 1-element character containing raw spec values.}
#' }}
#' \subsection{Value}{A named list of named parameters.}
#'
#'
#' @keywords internal
SpecificationParser <- function() {


  # utility functions to imitate ones in scala -------------

  startsWith <- function(s, pattern) {
    grepl(sprintf('^%s',pattern), s, fixed = FALSE)
  }

  endsWith <- function(s, pattern) {
    grepl(sprintf('%s$',pattern), s, fixed = FALSE)
  }

  contains <- function(s, pattern) {
    grepl(pattern, s, fixed = TRUE)
  }

  indexOf <- function(s, pattern) {
    regexpr(pattern=pattern,s, fixed = TRUE)[[1]]
  }

  take <- function(s, stop) {
    substr(s, start = 1, stop = stop)
  }

  takeRight <- function(s, n) {
    substr(s, nchar(s)-n+1, nchar(s))
  }

  drop <- function(s, x) {
    substr(s,start = x + 1, stop=nchar(s))
  }

  dropRight <- function(s, x) {
    substr(s,start = 1, stop=nchar(s) - x)
  }

  # concatenate vector of characters
  mkString <- function(s,sep=""){
    paste(s, collapse=sep)

  }

  # concatenate characters
  `%+%` <- function(a, b) paste0(a, b)

  # sprintf used as string interpolator
  # linebreak - match carriage return followed by newline or just newline.
  # (?: means do not remember groups
  linebreak <- "(?:(?:\r\n)|\n)"
  # whitespace - match zero to many number of spaces carriage returns or newlines.
  # \\s should do the same job
  ws <- "[ \r\n\t]*"
  # left brace with surrounding whitespace / linebreaks
  lbrace <- sprintf(" *%s? *\\{ *%s? *", linebreak, linebreak)
  # right brace with surrounding whitespace / linebreaks
  rbrace <- sprintf(" *%s? *\\} *%s? *", linebreak, linebreak)
  # anything that could be a specificaton or parameter name
  name <- "(?:[a-zA-Z]+[a-zA-Z0-9]*)"

  # anything permitted inside a spec value
  # anytext <- "(?:[a-zA-Z]+[a-zA-Z0-9\r\n\\(\\)\"\'\\.,=\\\\/ -]*)"
  # anytext <- "(?:[a-zA-Z]+[a-zA-Z0-9_\\(\\)\"\'\\.,=\\\\/ :;\\<\\>\\$-]*)"
  anyspecbody <- "(?:[a-zA-Z]+[^\\{\\}]*)"
  anytext <- "(?:.*)"

  # anything permitted on the rhs of a spec parameter
  # similar to anytext but first character can be open parentheses as well
  # Also does not match =
  # anyrhs <-  "(?:[a-zA-Z\\(]+[a-zA-Z0-9\r\n\\(\\)\"\'\\.,\\\\/ -]*)"
  # anyrhs <-  "(?:[a-zA-Z0-9\\(\\.\\\\/-]+[a-zA-Z0-9_\\(\\)\"\'\\.,\\\\/ :;\\<\\>\\$-]*)"
  # strictrhs <- "(?:[a-zA-Z0-9\\(\\.\\\\/+-]+[a-zA-Z0-9_\\(\\)\\.,\\\\/ :;\\<\\>+-]*)"
  # strictrhs <- "(?:[a-zA-Z0-9\\.,\\\\/+-]+[a-zA-Z0-9_\\.,\\\\/:;\\<\\>+-]*?)"
  strictrhs <- "(?:[a-zA-Z0-9\\.\\\\/+-]+[a-zA-Z0-9_\\.\\\\/\\<\\>+-]*?)"
  qrhs <- "(?:\".*?\"|\'.*?\')"
  singlerhs <- sprintf("(?:%s|%s)",strictrhs,qrhs)

  singleparen <- sprintf("^\\( ?(%s) ?\\)$", singlerhs)
  singleqrhs <- sprintf("^%s$",qrhs)

  multrhs <- sprintf("(?: ?,? ?%s ?,? ?)+", singlerhs)
  grouprhs <- sprintf("(?:%s|\\(%s\\))", singlerhs,multrhs)
  anyrhs <- sprintf("(?: ?%s ?)+", grouprhs)

  # anyrhs includes anytext in quotes
  # anyrhs <-  sprintf("(?:%s|\" ?%s ?\"|\' ?%s ?\')", strictrhs,anytext,anytext)
  # openparen <- "(?:\\( ?)"
  # closeparen <- "(?: ?\\))"
  # anyrhs <-  sprintf("(?:%s|\".*?\"|\'.*?\'|%s\".*?\"%s|%s\'.*?\'%s)", strictrhs,openparen,closeparen,openparen,closeparen)



  # high-level match for entire spec, i.e. spec { specname}
  # match the name, the lbrace.
  # Match any amount of whitespace (or none) or any amount of text (at least one character)
  specification <- sprintf("%s ?\\{ ?%s? ?(?:\\}|$)"
                          , name, anyspecbody)

  # high-level match for entire spec 'file', i.e. name { param = value} name { param = value}...
  # this group will get remembered.
  specifications <- sprintf("^( ?%s ?)+$", specification)



  # Make a single regex which matches 'any of' several others.
  # argument: rs - A list of regular expression strings.
  # return - A regular expression string
  anyOf <- function(rs){
    "(" %+%
      (purrr::map_chr(rs, function(r){sprintf("(?:%s)",r)}) %>%
         paste(., collapse="|")) %+%
      ")"
  }


  # see roxygen documentation above
  stripComment <- function(str){
    # if (startsWith(str,"#")) ""
    if (contains(str,"#")) take(str,(indexOf(str,"#")- 1))
    else str
  }


  # see roxygen documentation above
  preprocess <-function(str){

    #stringr::str_split("\\R") %>% unlist() %>% # split into character vector of lines. Matches any newline char
    str <- stringr::str_replace_all(str,"\x92","'")
    res <- str %>%
      purrr::map_chr(stripComment) %>% # strip out comments from each line
      purrr::map_chr(stringr::str_squish) #%>% purrr::map_chr(~ stringr::str_replace_all(.x,"[\"\']",""))  # squish whitespace
    res[res != ""] %>% # throw away blank lines.  How can I turn this into pipe?
      mkString(" ") # recombine lines with a space character
  }

  # see roxygen documentation above
  parseSpecs <- function(processed){
    if (!grepl(specifications, processed, perl = TRUE)) {
      rlang::abort("Specification is not formatted correctly.")
    }

    if(!endsWith(processed,"}")) warning("Last specification is missing `}`")
    stringr::str_extract_all(processed, specification) %>% unlist()
  }

  # see roxygen documentation above
  parseSpecNameAndBody <- function(str, to_lower=TRUE){
    # be sure to deal with case of empty brackets
      spec_expr <- sprintf("^(%s) ?\\{ ?(%s?) ?\\}?$"
                           , name, anyspecbody)
      res <- str_match(str, spec_expr)
      # chec res.  We expect 1 x 3 matrix
      # check case when nothing matches and case with partial match

      if(to_lower) names <- tolower(res[,2]) else names <- res[,2]

      res[,3] %>% setNames(names) %>% return()

  }

  # see roxygen documentation above
  parseArgs <- function(str, to_lower=TRUE) {

    re_escape <- function(strings){
      vals <- c("\\\\", "\\[", "\\]", "\\(", "\\)",
                "\\{", "\\}", "\\^", "\\$","\\*",
                "\\+", "\\?", "\\.", "\\|")
      replace.vals <- paste0("\\\\", vals)
      for(i in seq_along(vals)){
        strings <- gsub(vals[i], replace.vals[i], strings)
      }
      strings
    }

    # trim whitespace inside parentheses
    # expr_paren <- "\\(([^()]+)\\)"
    # trim_parens <- function(x){
    #   inside_paren <- x %>% stringr::str_match(expr_paren)
    #   inside_paren[,2] %>% stringr::str_trim() %>% sprintf("(%s)",.)
    # }
    trim_parens <- function(rhs){
      expr_openparen <- sprintf("\\( (,? ?%s)", singlerhs)
      rhs <- stringr::str_replace_all(rhs, expr_openparen,function(x){
        inside_paren <-x %>% stringr::str_match(expr_openparen)
        inside_paren[,2] %>% sprintf("(%s",.)
      })

      expr_closeparen <- sprintf("(%s ?,?) \\)", singlerhs)
      rhs <- stringr::str_replace_all(rhs, expr_closeparen,function(x){
        inside_paren <-x %>% stringr::str_match(expr_closeparen)
        inside_paren[,2] %>% sprintf("%s)",.)
      })

      rhs
    }

    append_by_name <- function(x, name, y){
      x[[name]] <- y
      return(x)
    }
    # expr to match groups: lhs and rhs
    # we also want to match from start or from middle
    expr_notlast <- sprintf("(?: %s ?=)",name)
    expr_last <- sprintf("(?: ?$)")

    expr_lh_rh <- sprintf("^(%s) ?= ?(%s)(?:%s|%s)",name,anyrhs,expr_notlast, expr_last)

    parse <- function(str,accum) {
      str %<>% stringr::str_trim()
      if(str=="") return(accum)
      # We replace with nothing and then parse again
      # Ideally anyrhs is made smart enough to match all parameters in one go with str_match_all
      parsed_args <- stringr::str_match(str,expr_lh_rh)
      # browser(expr = is.na(parsed_args[[1]]))
      whole_match <- parsed_args[[1]]
      lhs <- parsed_args[[2]] %>% stringr::str_trim()
      rhs <- parsed_args[[3]] %>% stringr::str_trim()

      # this should be whole match minus name =$ or minus just $
      expr_parsed <- sprintf("^%s ?= ?%s",lhs,re_escape(rhs))
      rest <- stringr::str_replace(str,expr_parsed,"") %>% stringr::str_trim()

      if(to_lower) {
        lhs <- tolower(lhs)
        rhs <- stringr::str_replace_all(rhs, singlerhs,function(x){
          if(!str_detect(x,qrhs)) x <- tolower(x)
          x
        })
      }

      if(grepl(singleparen,rhs,perl=TRUE)) rhs <- unparen(rhs)
      rhs <- trim_parens(rhs)
      if(grepl(singleqrhs,rhs,perl=TRUE)) rhs <- unquote(rhs)

      # if whole rhs is quoted then unquote, else trim whitespace in parentheses
      # if(str_detect(rhs,"^\" ?(.*) ?\"$|^\' ?(.*) ?\'$")) rhs <- unquote(rhs) else
      #   rhs <- stringr::str_replace_all(rhs, expr_paren, trim_parens)

      if(rest=="") append_by_name(accum,lhs,rhs) else
        parse(rest, append_by_name(accum,lhs,rhs))
    }

    #Strip out NA's
    parse(str, vector(mode="list"))

  }



  # Split body of spec into name-value pairs. Not currently used
  parseArgs0 <- function(str) {
    partextpar <- "(?:\\([\\d\\w\\s]*\\))"

    append_by_name <- function(x, name, y){
      x[[name]] <- y
      return(x)
    }
    # when '(' is detected, find the remaining sequence up to ')'
    # Stuff like data()
    # param (s: String, numLeft: Int, numRight: Int, accum: String)
    # return list()
    complete <- function(s, numLeft, numRight, accum) {
      if (s == "") sprintf("Invalid specification value: \"%s\"",str) %>% rlang::abort()
      else if (take(s,1) == "(") complete(drop(s,1), numLeft + 1, numRight, accum %+% "(")
      else if (take(s,1) == ")") {
        if (numLeft == (numRight + 1)) {
          rest <- drop(s,1) #%>% stringr::str_trim()
          if(take(stringr::str_trim(rest),1)=="(") complete(rest, numLeft, numRight + 1, accum %+% ")")
          else list(rest, value = accum %+% ")")
        } else complete(drop(s,1), numLeft, numRight + 1, accum %+% ")")
      }
      else complete(drop(s,1), numLeft, numRight, accum %+% take(s,1))
    }

    # s: String, accum: Seq[(String, String)]
    # returns Seq[(String, String)]
    parse <- function(s, accum) {
      if (s == "") accum
      else if (contains(s,"=")) {
        i <- indexOf(s,"=")
        left <- take(s,i -1) %>% stringr::str_trim() #stringr::str_squish()
        right <- drop(s, i) %>% stringr::str_trim() #stringr::str_squish()
        if (take(right,1) == "(") {
          value <- stringr::str_match_all(right,partextpar)[[1]][,1]
          dropped <- stringr::str_replace_all(right,partextpar,"") # sub with nothing. trim it
          parse(dropped, value %>% append_by_name(accum,left,.))


          # rv <- complete(right, 0, 0, "")
          # parse(rv$rest, append_by_name(accum, left, unquote(rv$value)))
        } else {
            j <- indexOf(right,"\n")
            if (j == -1) append_by_name(accum,left,right)
            else {
              parse(drop(right,j), (unquote(take(right, j - 1)) %>% append_by_name(accum,left,.)))
          }

        }
      } else sprintf("Could not parse parameters from \"%s\"",s) %>% rlang::abort()
    }

    parse(str, vector(mode="list"))
  }

  readSPCLines <- function(con, ...) {
    withCallingHandlers(
      warning = function(cnd) {
        if(str_detect(cnd$message,"^incomplete final line found on")) rlang::cnd_muffle(cnd)
      },
      message = function(cnd) {
        # code to run when message is signalled
      },
      base::readLines(con, ...)
    )

  }

  parseSPC <- function(fname, to_lower=TRUE) {
    if (!file.exists(fname))
      stop("File does not exist.")

    if (!isOpen(con <- file(fname, "rb")))
      stop("Unable to open file for reading.")

    on.exit(close(con), add = TRUE)

    con %>% readSPCLines() %>% preprocess() %>% parseSpecs() %>%
      parseSpecNameAndBody(to_lower=to_lower) %>% map(~ parseArgs(.x,to_lower=to_lower))

  }

  list(readSPCLines=readSPCLines, stripComment=stripComment
       , preprocess=preprocess, parseSpecs=parseSpecs
       , parseSpecNameAndBody=parseSpecNameAndBody
       , parseArgs=parseArgs, parseSPC=parseSPC)

}

#' emulate a singleton object
#' @keywords internal
SPCparser <- SpecificationParser()

#' @keywords internal
parsedSpecToX13SpecList <- function(parsed_spec){
  res <- parsed_spec %>% map2(names(parsed_spec), function(x,y){
    class(x) <- "X13Spec"
    attr(x,"name") <- y
    x
  })

  class(res) <- "X13SpecList"
  res
}
