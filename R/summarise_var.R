#' Computes summary statistics for a variable
#'
#' This is a helper function for `summary_table()`.
#'
#' @noRd
#'
#' @param df a data.frame
#' @param var a variable name
#'
#' @return a vector with summary statistics of `var`.

summarise_var <- function(df, var){

  # Calculate numbers and proportion in each category for factor variables
  if(is.factor(df[[var]])){

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
  } else if(is.numeric(df[[var]]) &
            max(df[[var]], na.rm = TRUE) == 1 &
            min(df[[var]], na.rm = TRUE) == 0){

    mean <- mean(df[[var]],
                 na.rm = TRUE) |>
      round(3)

    p25  <- NA

    p75  <- NA

    min  <- 0

    max  <- 1

    n_category <- sum(df[[var]], na.rm = TRUE)

    n_total    <- sum(!is.na(df[[var]]))

    n_NA       <- sum(is.na(df[[var]]))

    out <- data.frame(varname = paste(var), mean, p25, p75, min, max,
                      n_category, n_total, n_NA)

    # Calculate median, q25, q75 for numeric variables
  } else if(is.numeric(df[[var]]) &
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
