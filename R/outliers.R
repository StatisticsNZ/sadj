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
removeRegAfterEnd <- function(reg_vars, s_period, s_end_date) {
  pfac <- if_else(s_period==4,3,1)
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

  res <- stringr::str_match(reg_vars, regvar_regex) %>% t() %>% as.data.frame(stringsAsFactors = FALSE)

  # For non-NA entries in column 1, use the year and period to make dates
  res <- res %>% map_dbl(function(x){
    if(!is.na(x[[1]])){
      as.Date(sprintf("%s-%s-01"
                      ,as.numeric(x[[2]])
                      , pfac * as.numeric(x[[3]])
      ))
      # "hello"
    } else as.Date(NA_character_)
  }) %>% lubridate::as_date()

  res <- tibble(reg_vars = reg_vars, date = res, s_end_date = s_end_date)

  # Compare dates to end of series
  # Keep if NA and if less than or equal to s_end_date
  res %>% filter(is.na(date) | (date <= s_end_date)) %>% pull(reg_vars)
}
