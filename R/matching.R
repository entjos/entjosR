#' Simpel matching
#'
#' Flexible and fast matching functions.
#'
#' @examples
#' require(rstpm2)
#'
#' matching(brcancer,
#'          case_indicator = "hormon",
#'          id = "id",
#'          matching_factors = list(x2 = "exact",
#'                                  rectime = function(x, y) y >= x),
#'          no_controls = 1,
#'          verbose = TRUE)
#'
#' @import data.table
#' @export matching

matching <- function(data,
                     case_indicator,
                     id,
                     matching_factors,
                     no_controls,
                     replace = TRUE,
                     seed,
                     verbose = FALSE){

  # Data preperations ----------------------------------------------------------
  data <- data.table::copy(data.table::as.data.table(data))

  exact_vars <- names(matching_factors[matching_factors == "exact"])
  matching_f <- matching_factors[vapply(matching_factors, is.function, TRUE)]

  # Create strata variables for fast matching of exact variables
  data[, strata := do.call(paste0, c(.SD)), .SDcols = exact_vars]

  cases    <- data[get(case_indicator) == 1]
  controls <- data[get(case_indicator) == 0]

  # Key control dataset to make matching faster
  controls <- controls[, list(subset = list(.SD)), by = strata]

  # Matching -------------------------------------------------------------------
  matched_controls <- lapply(seq_len(nrow(cases)), function(i){

    # Set seed if supplied
    if(!is.null(seed)){
      set.seed(seed + i)
    }

    case <- cases[i, ]

    # Check that exact matching is possible for all strata variables
    if(controls[strata == case$strata, .N] < 1){

      print(case[, .SD, .SDcols = c(id, exact_vars)])

      cli::cli_abort(
        c(
          x = paste(
            "Exact matching is not possible, because no control is",
            "available for the case listed above"
          )
        )
      )
    }

    # Find matches within same strata
    matches <- controls[strata == case$strata][["subset"]][[1]]

    # Apply additional matching criteria
    for (x in names(matching_f)) {

      matches <- matches[matching_f[[x]](case[, get(x)], get(x))]

    }

    if(nrow(matches) < no_controls){

      if (nrow(matches) == 0){
        # If no controls are availabe ==========================================
        cli::cli_warn(
          c(
            x = "No controls available for {case[[id]]}",
            i = "Will return case to output dataset without controls"
          )
        )

        # Combine riskset
        comb    <- case[   , .(id = get(..id), case = 1)]
        comb$riskset <- i
      } else {
        # If to few controls are availabe ======================================

        cli::cli_warn(
          c(
            x = "Less controls available than needed for case {case[[id]]}",
            i = "Will only take those controls available"
          )
        )

        # Combine riskset
        case    <- case[   , .(id = get(..id), case = 1)]
        matches <- matches[, .(id = get(..id), case = 0)]

        comb <- rbind(case, matches)
        comb$riskset <- i
      }

    } else {
      # If enough controls are available =======================================

      # Do matching
      matches <- matches[sample(.N, no_controls, replace)]

      # Combine riskset
      case    <- case[   , .(id = get(..id), case = 1)]
      matches <- matches[, .(id = get(..id), case = 0)]

      comb <- rbind(case, matches)
      comb$riskset <- i

    }

    return(comb)

  }) |> data.table::rbindlist()

  # Create output dataset ------------------------------------------------------

  # Add all additional data
  out <- merge(matched_controls,
               data,
               by.x = "id",
               by.y = id,
               all.x = TRUE)

  # Cleaning
  out[, strata := NULL]

  # Report diagnostics if needed -----------------------------------------------
  if(verbose){

    total_comp  <- out[get(case_indicator) == 0, .N]
    unique_comp <- unique(out, by = "id")[case == 0, .N]
    prop_unique <- round(unique_comp/total_comp, 3) * 100

    cat("----------------------------------------\n")
    cat("No. of comparators:", total_comp, "\n")
    cat("No. of unique comparators:", unique_comp, "\n")
    cat("Proportion of unique compartors:", prop_unique, "%\n")
    cat("----------------------------------------\n\n")


  }

  return(out)

}
