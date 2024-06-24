#' Computes event rates
#'
#' This is a helper function for `summary_table()`.
#'
#' @noRd
#'
#' @param df a data.frame
#' @param var a variable name
#' @param event the name of the event indicator or event counter
#' @param st the name of the person-time varialbe
#' @param control a list of control parameters passed to `epiR::epi.conf()`
#'
#' @return a data.frame with event rates for each level of `var`.

summarise_rates <- function(df, var, event, st, control){

  no_events <- t_at_risk <- p_events <- NULL # Due to NSE notes in R CMD check

  # Get no. of events and person-years
  rate_dt <- data.table::copy(df)

  rate_dt <- rate_dt[!is.na(get(var)),
                     list(no_events = sum(get(event)),
                          t_at_risk = sum(get(st))),
                     by = var]

  rate_dt[, p_events := no_events / sum(no_events)]

  # Calculate rates with CIs
  args_epi.conf     <- control
  args_epi.conf$dat <- as.matrix(rate_dt[, c("no_events", "t_at_risk")])

  rate <- do.call(epiR::epi.conf, args_epi.conf)

  # Improve naming
  names(rate) <- paste0("rate_", names(rate))

  return(cbind(varname = paste(var, rate_dt[[var]]), rate_dt[, -1], rate))

}
