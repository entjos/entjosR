#' Summary Table
#'
#' Creates stratified summary table with rates.
#'
#' @param data
#'    A `data.frame` that includes the data that should be summarised.
#'
#' @param vars
#'    A character vector specifying the variables that should be summarised.
#'    `summary_table` can only summarise variables of class `numeric` or
#'    `factor`.
#'
#' @param strata
#'    An optional character vector specifying the variables on which the
#'    summary table should be stratified on.
#'
#' @param overall
#'    If `TRUE` an additionall summary table will be created including the
#'    complete dataset. This can be used in combination with `strata`.
#'
#' @param get_rates
#'    If `TRUE` the summary tables includes events rates for events specified
#'    in the `event` argument for each level of variables in `var`. Rates can
#'    only be estimated for factor variables in `var`.
#'
#' @param get_no_events
#'    If `TRUE` the summary tables includes the number of events specified
#'    in the `event` argument for each level of variables in `var`. This
#'    argument is obsolete if `get_rates == TRUE` as the number of events is
#'    automatically added to the table in this case.
#'
#' @param  event
#'    Name of the event indicator that should be used to compute rates.
#'    Needs to be specified if `get_rates == TRUE`.
#'
#' @param st
#'    Name of the varaible that includes the survival time that should be used
#'    to comptue rates. Needs to be specified if `get_rates == TRUE`.
#'
#' @param control
#'    A `list` of control parameters passed to the `epiR::epi.conf()` call when
#'    estimating confidence intervals for rates. This can be used to change the
#'    type and method used for estimating confidence intervals. Check out the
#'    documentation of `epiR::epi.conf()` for more information.
#'
#' @return
#'    A `data.frame` with the following columns:
#'    - `varname`: Including the name of the variable or the name of the category for factor variables
#'    - `mean`: The mean of the variable, i.e. the proportion for binary variables
#'    - `p25`: The 25th percentile (only for metric variables.)
#'    - `p75`: The 75th percentile (only for metric varaibles.)
#'    - `min`: Minimum of the variable
#'    - `max`: Maximum of the variable
#'    - `n_category`: The number of non-NA observations for each variable or cateogry
#'    - `n_total`: The number of non-NA observations for the variable
#'    - `n_NA`: The number of NA observations for this variable or category
#'
#'    If strata variables were supplied to `summary_table` the output
#'    `data.table` will include one row for each strata variable with the
#'    corresponding value for the specific strata the row belongs to.
#'
#' @examples
#' require(rstpm2)
#' data("brcancer")
#'
#' # Simple example
#' summary_table(brcancer,
#'               vars = c("x1", "x2", "x3"))
#'
#' # Example with stratas
#' summary_table(brcancer,
#'               vars = c("x1", "x2", "x3"),
#'               strata = c("hormon", "x4"))
#'
#' # Example with strata and rates
#' brcancer$x4 <- as.factor(brcancer$x4)
#'
#' summary_table(brcancer,
#'               vars      = c("x1", "x2", "x3", "x4"),
#'               strata    = c("hormon"),
#'               get_rates = TRUE,
#'               event     = "censrec",
#'               st        = "rectime")
#'
#' @importFrom data.table :=
#' @export summary_table

summary_table <- function(data,
                          vars,
                          strata = NULL,
                          overall = FALSE,
                          get_rates = FALSE,
                          get_no_events = FALSE,
                          event = NULL,
                          st = NULL,
                          control = list(ctype = "inc.rate",
                                         method = "exact")){

  # Convert data.table to data.frame ------------------------------------------

  df <- data.table::setDT(data.table::copy(data))

  # Checks --------------------------------------------------------------------

  if(get_rates && is.null(event) & is.null(st)){
    cli::cli_abort(
      c(x = paste("For computing rates both the `event` and `st`",
                  "argument need to be specified."))
    )
  }

  if(get_no_events && is.null(event)){
    cli::cli_abort(
      c(x = paste("For computing the number of events the `event`",
                  "argument needs to be specified."))
    )
  }

  if(!all(vars %in% colnames(df))){

    temp <- vars[!(vars %in% colnames(df))]

    cli::cli_abort(
      c(x = "Could not find {temp} in `data`.",
        i = "All variables specified in `vars` must be included in `data`")
    )

  }

  if(!all(strata %in% colnames(df))){

    temp <- strata[!(strata %in% colnames(df))]

    cli::cli_abort(
      c(x = "Could not find {temp} in `data`.",
        i = "All variables specified in `strata` must be included in `data`")
    )

  }


  if(any(vapply(df[, vars, with = FALSE], is.character, logical(1)))){

    temp <- vars[vapply(df[, vars, with = FALSE],
                        is.character, logical(1))]

    cli::cli_abort(
      c(x = "{temp} {?is/are} of type character.",
        i = paste("Summary tables can only be created for variables of class",
                  "`numeric` or `factor`."))
    )
  }

  if(overall && is.null(strata)){
    cli::cli_alert_info(
      paste("Setting overall to TRUE without specifying the strata argument",
            "is most likely useless.")
    )
  }

  if(get_rates){

    if(any(vapply(df[, vars, with = FALSE], is.factor, logical(1)))){
      cli::cli_alert_warning(
        c(i = "Rates will only be estimted for factor variables in `vars`.")
      )
    } else {
      cli::cli_abort(
        c(x = "Rates can only be estimted for factor variables in `vars`.",
          i = paste("Please make sure that at least one variable specified in",
                    "`vars` is of class `factor`."))
      )
    }

    # no. of events is already outputted from the summary_rate()
    get_no_events <- FALSE

  }

  if(!is.null(event) && any(!(unique(df[[event]]) %in% 0:1))){
    cli::cli_alert_warning(
      c(i = paste("The event variable is not 0/1 coded. If this is intendet,
                  you may ignore this warning.")
      )
    )
  }

  # Create stratified summary table -------------------------------------------

  if(!is.null(strata)){

    # Get unique values of strata variables
    strata_values <- lapply(strata, function(x){unique(df[[x]])})
    strata_values <- stats::setNames(strata_values, strata)

    # Create matrix with possible combination of unique strata values
    stratas <- expand.grid(strata_values)

    # Create a vector of concatenated strata values
    concat_stratas <- do.call(paste, df[, strata, with = FALSE])

    # Subset each strata and obtain summary variables
    out_strat <- lapply(seq_len(nrow(stratas)), function(i){

      if(length(strata) > 1){

        concat_strata <- do.call(paste, stratas[i, ])

        strat_sub <- concat_stratas == concat_strata

      } else {

        strat_sub <- df[, get(strata)] == as.character(stratas[i, ])

      }

      strata_df <- subset(df, strat_sub)

      temp_out  <- lapply(vars,
                          function(x){
                            summarise_var(df  = strata_df,
                                        var = x)
                          }) |> data.table::rbindlist()

      if(get_rates){

        temp_out_rates <-
          lapply(vars[vapply(df[, vars, with = FALSE], is.factor, logical(1))],
                 function(x){
                   summarise_rates(df      = strata_df,
                                 var     = x,
                                 event   = event,
                                 st      = st,
                                 control = control)
                 }) |> data.table::rbindlist()

        return(merge(temp_out, temp_out_rates,
                     by = "varname",
                     all.x = TRUE))

      } else if (get_no_events){

        temp_out_no_events <-
          lapply(vars[vapply(df[, vars, with = FALSE], is.factor, logical(1))],
                 function(x){
                   summarise_no_events(df      = strata_df,
                                     var     = x,
                                     event   = event)
                 }) |> data.table::rbindlist()

        out_overall <- merge(temp_out, temp_out_no_events,
                             by = "varname",
                             all.x = TRUE)

      } else {
        return(temp_out)
      }

    })

    for(i in seq_len(nrow(stratas))) {

      data.table::set(out_strat[[i]], j = strata,
                      value = stratas[i, ])

      data.table::setcolorder(out_strat[[i]], strata)

    }

  }

  # Create non-stratified summary table ---------------------------------------

  if(is.null(strata) | overall){

    out_overall <- lapply(vars, summarise_var, df = df) |>
      data.table::rbindlist()

    if(get_rates){

      temp_out_rates <-
        lapply(vars[vapply(df[, vars, with = FALSE], is.factor, logical(1))],
               function(x){
                 summarise_rates(df      = df,
                               var     = x,
                               event   = event,
                               st      = st,
                               control = control)
               }) |> data.table::rbindlist()

      out_overall <- merge(out_overall, temp_out_rates,
                           by = "varname",
                           all.x = TRUE)

    } else if (get_no_events){

      temp_out_no_events <-
        lapply(vars[vapply(df[, vars, with = FALSE], is.factor, logical(1))],
               function(x){
                 summarise_no_events(df      = df,
                                   var     = x,
                                   event   = event)
               }) |> data.table::rbindlist()

      out_overall <- merge(out_overall, temp_out_no_events,
                           by = "varname",
                           all.x = TRUE)

    }

  }

  # Define output object ------------------------------------------------------

  if(is.null(strata)){

    out <- out_overall

  } else if(overall){

    data.table::set(out_overall, j = strata, value = "NA")
    data.table::setcolorder(out_overall, strata)

    out <- append(out_strat, list(out_overall))

  } else {

    out <- out_strat

  }

  if(inherits(out, "list")){

    # Combine data.tables
    out <- data.table::rbindlist(out)

  }

  return(as.data.frame(out))

}
