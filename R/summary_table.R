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
#'    A `data.table` with the following columns:
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
                          event = NULL,
                          st = NULL,
                          control = list(ctype = "inc.rate",
                                         method = "exact")){

  # Convert data.table to data.frame ------------------------------------------

  df <- data.table::setDT(data.table::copy(data))

  # Checks --------------------------------------------------------------------

  if(get_rates & is.null(event) & is.null(st)){
    stop("For competing rates both the `event` and `st`
         argument need to be specified.")
  }

  if(!all(vars %in% colnames(df))){
    stop("Could not find all variables specified in `vars` in `df`.")
  }

  if(!all(strata %in% colnames(df))){
    stop("Could not find all variables specified in `strata` in `df`.")
  }


  if(any(vapply(df[, vars, with = FALSE], is.character, logical(1)))){
    stop("One of the specified variables is of class `character`.
         Summary tables can only be created for variables of class
         `numeric` or `factor`.")
  }

  if(overall & is.null(strata)){
    message("Setting overall to TRUE without specifying the strata argument
            is most likely useless.")
  }

  if(get_rates){

    if(any(vapply(df[, vars, with = FALSE], is.factor, logical(1)))){

      message("Rates will only be estimted for factor variables in `vars`.")

    } else {

      stop(paste("Rates can only be estimted for factor variables in `vars`.",
                 "Please make sure that at least one variable specified in",
                 "`vars` is of class `factor`."))

    }

  }

  # Create stratified summary table -------------------------------------------

  if(!is.null(strata)){

    # Get unique values of strata variables
    strata_values <- lapply(strata, function(x){unique(df[[x]])})
    strata_values <- setNames(strata_values, strata)

    # Create matrix with posssible combination of unique strata values
    stratas <- expand.grid(strata_values)

    # Create a vector of concatenated strata values
    concat_stratas <- do.call(paste, df[, strata, with = FALSE])

    # Subset each strat and obtain summary variables
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
                            var_summary(df  = strata_df,
                                        var = x)
                          }) |> data.table::rbindlist()

      if(get_rates){

        temp_out_rates <-
          lapply(vars[vapply(df[, vars, with = FALSE], is.factor,  logical(1))],
                 function(x){
                   summary_rates(df      = strata_df,
                                 var     = x,
                                 event   = event,
                                 st      = st,
                                 control = control)
                 }) |> data.table::rbindlist()

        return(merge(temp_out, temp_out_rates,
                     by = "varname",
                     all.x = TRUE))

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

    out_overall <- lapply(vars, var_summary, df = df) |>
      data.table::rbindlist()

    if(get_rates){

      temp_out_rates <-
        lapply(vars[vapply(df[, vars, with = FALSE], is.factor, logical(1))],
               function(x){
                 summary_rates(df      = df,
                               var     = x,
                               event   = event,
                               st      = st,
                               control = control)
               }) |> data.table::rbindlist()

      out_overall <- merge(out_overall, temp_out_rates,
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

    # Cobine data.tables
    out <- data.table::rbindlist(out)

  }

  return(data.table::data.table(out))

}

# var_summary function --------------------------------------------------------

var_summary <- function(df, var){

  # Calculate numbers and proportion in each category for factor variables
  if(class(df[[var]]) == "factor"){

    lev <- levels(df[[var]])

    out <- lapply(seq_along(lev), function(i){

      mean <- mean(as.numeric(df[[var]] == lev[i]),
                   na.rm = TRUE) |>
        round(3)

      p25  <-  NA

      p75  <-  NA

      min  <- 0

      max  <- 1

      n_category <- sum(df[[var]] == lev[i], na.rm = TRUE)

      n_total    <- sum(!is.na(df[[var]]))

      n_NA       <- sum(is.na(df[[var]]))

      varname <- paste(var, levels(df[[var]])[i]) |>
        as.character()

      return(data.frame(varname, mean, p25, p75, min, max,
                        n_category, n_total, n_NA))

    }) |> data.table::rbindlist()

    # Calculate numbers and proportion in each category for dichotomiesed
    # integer variables
  } else if(class(df[[var]]) %in% c("integer", "numeric") &
            max(df[[var]], na.rm = TRUE) == 1 &
            min(df[[var]], na.rm = TRUE) == 0){

    mean <- mean(df[[var]],
                 na.rm = TRUE) |>
      round(3)

    p25  <- NA

    p75  <- NA

    min  <- 0

    man  <- 1

    n_category <- sum(df[, var], na.rm = TRUE)

    n_total    <- sum(!is.na(df[[var]]))

    n_NA       <- sum(is.na(df[[var]]))

    out <- data.frame(varname = var, mean, p25, p75, min, max,
                      n_category, n_total, n_NA)

    # Calculate median, q25, q75 for numeric variables
  } else if(class(df[[var]]) %in% c("integer", "numeric") &
            max(df[[var]], na.rm = TRUE) != 1){

    mean <- stats::quantile(df[[var]], 0.5 , na.rm = TRUE,
                            names = FALSE) |>
      round(1)

    p25  <- stats::quantile(df[[var]], 0.25, na.rm = TRUE,
                            names = FALSE) |>
      round(1)

    p75  <- stats::quantile(df[[var]], 0.75, na.rm = TRUE,
                            names = FALSE) |>
      round(1)

    min <- min(df[[var]], na.rm = TRUE)

    max <- max(df[[var]], na.rm = TRUE)

    n_category <- NA

    n_total    <- sum(!is.na(df[[var]]))

    n_NA       <- sum(is.na(df[[var]]))

    out <- data.frame(varname = var, mean, p25, p75, min, max,
                      n_category, n_total, n_NA)

  }
  return(out)
}

# summary_rates function ------------------------------------------------------

summary_rates <- function(df, var, event, st, control){

  no_events <- t_at_risk <- p_events <- NULL # Due to NSE notes in R CMD check

  # Get no. births by variable
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
