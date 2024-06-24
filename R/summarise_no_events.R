#' Computes event counts
#'
#' This is a helper function for `summary_table()`.
#'
#' @noRd
#'
#' @param df a data.frame
#' @param var a variable name
#' @param event the name of the event indicator or event counter
#'
#' @return a data.frame with event rates for each level of `var`.

summarise_no_events <- function(df, var, event){

  no_events <- NULL # Due to NSE notes in R CMD check

  # Get no. of events and person-years
  count_dt <- data.table::copy(df)

  count_dt <- count_dt[!is.na(get(var)),
                       list(no_events = sum(get(event))),
                       by = var]

  return(cbind(varname = paste(var, count_dt[[var]]), count_dt[, -1]))

}
