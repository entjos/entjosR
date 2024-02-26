#' Simpel matching
#'
#' Flexible and fast matching functions.
#'
#' @param data  A `data.frame` including both cases and potential controls.
#'
#' @param case_indicator Name of the variable that indicates if an observation
#'    is a case or a controls, codes as 1 if case and 0 if control.
#'
#' @param id Name of the variable that identifies unique observations.
#'
#' @param matching_factors A list of matching factors and their accompanying
#'    matching criteria either supplied as a function or as 'exact', if
#'    exact matching is requested for a specific variable. The matching
#'    function should take two arguments as input where the first argument
#'    is the value of the case and the second argument is the value of the
#'    control. The function `function(x,y) abs(x - y) < 1` can for example
#'    be used to match cases to compactors with a maximal age difference of
#'    one year. The function `function(x,y) y >= x` can be used for concurrent
#'    matching, i.e., requiering the control to be alive at the time of matching.
#'
#'    Example:
#'    ```
#'    list(sex  = 'exact',
#'         age  = function(x,y) abs(x - y) < 1,
#'         time = function(x,y) y >= x)
#'    ```
#'
#' @param no_controls The number of controls that should be matched to each
#'   case.
#'
#' @param replace Logical indicator if sampling of controls should be done with
#'   or without replacement. Default: `FALSE`.
#'
#' @param seed Optional seed used for the matching. Usefull for reproducibility.
#'
#' @param verbose Logical indicator if default checks should be printed.
#'   Default: `TRUE`.
#'
#' @return A dataset of matched cases and controls.
#'   \describe{
#'     \item{`id`}{Individuals unique id}
#'     \item{`case`}{Case indicator}
#'     \item{`riskset`}{Riskset number/identifier}
#'     }
#'  All the variables included in the supplied dataset are also returned.
#'
#' @details
#'   The function returns a warning if the number of available controls is
#'   smaller than the number of controls requested for each case. In this
#'   case all availabe controls are matched to the case.
#'
#'   If there are no matched availabe for a particular case, the case will
#'   be removed from the output dataset and a warning will be shown indicating
#'   the `id` of the case that has been removed.
#'
#'   If you want to match exact on a variable, please use the 'exact' indicator
#'   as shown above. This will improve the computation time.
#'
#' @examples
#' require(rstpm2)
#'
#' matching(brcancer,
#'          case_indicator = "hormon",
#'          id = "id",
#'          matching_factors = list(x2 = "exact",
#'                                  rectime = function(x, y) y >= x),
#'          no_controls = 1)
#'
#' @import data.table
#' @export matching

matching <- function(data,
                     case_indicator,
                     id,
                     matching_factors,
                     no_controls,
                     replace = FALSE,
                     seed = NULL,
                     verbose = TRUE){

  # Data preperations ----------------------------------------------------------

  # Please R CMD Check
  strata <- ..id <- case <- NULL

  # Only keep variables that are needed for matching
  vars_needed <- c(id, case_indicator, names(matching_factors))

  min_data <- data.table::copy(data.table::as.data.table(data[, vars_needed]))

  # Find out which variables should be matched exact
  exact_vars <- names(matching_factors[matching_factors == "exact"])
  matching_f <- matching_factors[vapply(matching_factors, is.function, TRUE)]

  if(length(exact_vars) > 0){
    # Create strata variables for fast matching of exact variables
    min_data[, strata := do.call(paste0, c(.SD)), .SDcols = exact_vars]
  }

  cases    <- min_data[get(case_indicator) == 1]
  controls <- min_data[get(case_indicator) == 0]

  if(length(exact_vars) > 0){
    # Key control dataset to make matching faster
    controls <- controls[, list(subset = list(.SD)), by = strata]
  }

  # Matching -------------------------------------------------------------------
  matched_controls <- lapply(seq_len(nrow(cases)), function(i){

    # Set seed if supplied
    if(!is.null(seed)){
      set.seed(seed + i)
    }

    case <- cases[i, ]

    if(length(exact_vars) > 0){
      # Find matches within same strata
      matches <- controls[strata == case$strata][["subset"]][[1]]
    } else {
      matches <- controls
    }

    # Apply additional matching criteria
    for (x in names(matching_f)) {

      subset_var <- matching_f[[x]](case[, get(x)], matches[, get(x)])
      matches <- matches[subset_var]

    }

    if(nrow(matches) < no_controls){

      if (nrow(matches) == 0){
        # If no controls are availabe ==========================================
        cli::cli_warn(
          c(
            x = "No controls available for {case[[id]]}.",
            i = "I'll remove this control form the output dataset."
          )
        )

        return(NULL)

      } else {
        # If to few controls are availabe ======================================

        cli::cli_warn(
          c(
            x = "Less controls available than needed for case {case[[id]]}.",
            i = "I'll only take those controls available."
          )
        )

        # Combine riskset
        case    <- case[   , list(id = get(..id), case = 1)]
        matches <- matches[, list(id = get(..id), case = 0)]

        comb <- rbind(case, matches)
        comb$riskset <- i
      }

    } else {
      # If enough controls are available =======================================

      # Do matching
      matches <- matches[sample(.N, no_controls, replace)]

      # Combine riskset
      case    <- case[   , list(id = get(..id), case = 1)]
      matches <- matches[, list(id = get(..id), case = 0)]

      comb <- rbind(case, matches)
      comb$riskset <- i

    }

    return(comb)

  }) |> data.table::rbindlist()

  # Create output dataset ------------------------------------------------------

   additional_vars <- colnames(data)[!(colnames(data) %in% vars_needed)]

  if(length(additional_vars) > 0){
    # Add all additional data
    out <- merge(matched_controls,
                 data[, c("id", additional_vars)],
                 by.x = "id",
                 by.y = id,
                 all.x = TRUE)
  } else {
    out <- matched_controls
  }

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
