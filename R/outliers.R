#' Removes regression Variables out of Span
#'
#' @param reg_var
#' @param end_date
#' @param speriod
#'
#' @return
#' @export
#'
#' @examples
removeOutOfSpanReg <- function(reg_vars, s_period, s_end_date) {
  # reg_vars <- c("Ao.")
  reg_vars %<>% tolower()

  # Is it monthly, quarterly or annual?
  if(s_period ==1)
    period_regex <- "[0-9]{4}"
  else if(s_period == 4)
    period_regex <- "([0-9]{4})[.]([1-4]{1})"
  else if(s_period == 12) {
    month_abb <- month.abb[1:12] %>% tolower() %>% paste(collapse = "|")
    period_regex <- sprintf("([0-9]{4})[.]([1]{1}[0-2]{1}|[1-9]{1}|%s)", month_abb)
  } else
    stop(sprintf("Non-valid period: %s", s_period))


  regvar_regex <- sprintf("^(?:ao|ls|tc|so)%s$",period_regex)
  # need to add (aos|lss|rp|qd|qi|tl)date-date see page 150 of X13 manual
  stringr::str_match(reg_vars, regvar_regex) #%>% table(useNA = "always") %>% as_tibble()
  # could check if match wrong period

  # Parse the reg variables using s_period to get dates for reg variables
  # filter out the variable that are past the end date

}
