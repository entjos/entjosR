#' Simpel matching
#'
#' Flexible and fast matching functions.
#'
#' @param cases  A `data.frame` including cases.
#'
#' @param controls  A `data.frame` including potential controls.
#'
#' @param id_name Name of the variable that identifies unique observations.
#'    Please make sure that your dataset does not include a varaible named
#'    `id_name`.
#'
#' @param by A list of matching factors and their accompanying
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
#' @param seed Optional seed used for the matching. Useful for reproducibility.
#'
#' @param return_case_values If `TRUE` the case value of the matching variable
#'   will be returned in the output data set, e.g., if you match on time,
#'   the controls in the output dataset will have a variable `case_time` which
#'   has the value of the case's time variable. This can be useful if you
#'   would like to start follow-up at the time of matching in your later
#'   analysis.
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
#'
#' @details
#'   The function returns a warning if the number of available controls is
#'   smaller than the number of controls requested for each case. In this
#'   case all available controls are matched to the case.
#'
#'   If there are no matched available for a particular case, the case will
#'   be removed from the output dataset and a warning will be shown indicating
#'   the `id` of the case that has been removed.
#'
#'   If you want to match exact on a variable, please use the 'exact' indicator
#'   as shown above. This will improve the computation time.
#'
#' @examples
#'
#' require(rstpm2)
#'
#' risksets <- cc_match(cases    = brcancer[brcancer$hormon == 1, ],
#'                      controls = brcancer[brcancer$hormon == 0, ],
#'                      id = "id",
#'                      by = list(x2 = "exact",
#'                                rectime = function(x, y) y >= x),
#'                      no_controls = 1,
#'                      replace = TRUE)
#'
#' head(risksets)
#'
#' @import data.table
#' @export cc_match

cc_match <- function(cases,
                     controls,
                     by,
                     id_name,
                     no_controls,
                     replace = TRUE,
                     seed = NULL,
                     return_case_values = FALSE,
                     verbose = TRUE){

  # Data checks ----------------------------------------------------------------

  if(nrow(controls) < 1){
    cli::cli_abort(
      c(
        x = "There are no controls included in {.var data}.",
        i = "Check that you included your controls in {.var data}."
      )
    )
  }

  if("id_name" %in% c(colnames(cases), colnames(controls))){
    cli::cli_abort(
      x = "You're datasets are not allows to include a varaible names `id_name`",
      i = "Please rename your variable and try again."
    )
  }
  # Data preperations ----------------------------------------------------------

  # Please R CMD Check
  strata <- case <- riskset <- NULL

  # Only keep variables that are needed for matching
  vars_needed <- c(id_name, names(by))

  cases    <- data.table::copy(data.table::as.data.table(cases[, vars_needed]))
  controls <- data.table::copy(data.table::as.data.table(controls[, vars_needed]))

  colnames(cases)    <- gsub(id_name, "id", colnames(cases), fixed = TRUE)
  colnames(controls) <- gsub(id_name, "id", colnames(controls), fixed = TRUE)

  # Find out which variables should be matched exact
  exact_vars <- names(by[by == "exact"])
  matching_f <- by[vapply(by, is.function, TRUE)]

  if(length(exact_vars) > 0){
    # Create strata variables for fast matching of exact variables
    cases[   , strata := do.call(paste0, c(.SD)), .SDcols = exact_vars]
    controls[, strata := do.call(paste0, c(.SD)), .SDcols = exact_vars]
  }

  if(length(exact_vars) > 0){
    # Key control dataset to make matching faster
    controls <- controls[, list(subset = list(.SD)), by = strata]
  }

  # Matching -------------------------------------------------------------------
  risksets <- lapply(seq_len(nrow(cases)), function(i){

    # Set seed if supplied
    if(!is.null(seed)){
      set.seed(seed + i)
    }

    case <- cases[i, ]

    if(length(exact_vars) > 0){
      # Find matched within same strata
      matched <- controls[strata == case$strata][["subset"]][[1]][id != case$id]
    } else {
      matched <- controls[id != case$id]
    }

    # Apply additional matching criteria
    for (x in names(matching_f)) {

      subset_var <- matching_f[[x]](case[, get(x)], matched[, get(x)])
      matched <- matched[subset_var]

    }

    if(nrow(matched) < no_controls){

      if (nrow(matched) == 0){
        # If no controls are availabe ==========================================
        cli::cli_warn(
          c(
            x = "No controls available for {case$id}.",
            i = "I'll remove this control form the output dataset."
          )
        )

        return(NULL)

      } else {
        # If to few controls are availabe ======================================

        cli::cli_warn(
          c(
            x = "Less controls available than needed for case {case$id}.",
            i = "I'll only take those controls available."
          )
        )

      }

    } else {
      # If enough controls are available =======================================

      # Do matching
      matched <- matched[sample(.N, no_controls, replace)]
    }

    # Combine riskset ==========================================================

    if(return_case_values){
      case    <- case[ , .SD, .SDcols = c("id", names(by))]
    } else {
      case    <- case[ , list(id)]
    }

    case[, case := 1]
    case[, riskset := i]

    matched <- matched[, .SD, .SDcols = "id"]
    matched[, case := 0]
    matched[, riskset := i]

    data.table::setcolorder(case   , c("id", "case", "riskset"))
    data.table::setcolorder(matched, c("id", "case", "riskset"))

    comb <- rbindlist(list(case, matched), fill = TRUE)

    if(!replace){
      # Remove used controls if replace = FALSE
      controls <<- eval(
        substitute(
          controls[,
                   list(subset = lapply(.SD[[1]], "[", !(id %in% matched$id))),
                   by = strata],
          list(matched = matched)
        )
      )

    }

    return(comb)

  }) |> data.table::rbindlist()

  # Prepare output -------------------------------------------------------------

  if(return_case_values){
    # Fill character values ====================================================

    # Find character values
    char_cols <- colnames(risksets)[vapply(risksets, is.character, TRUE)]

    # Convert to factors
    risksets[ , (char_cols) := lapply(.SD, factor),
              .SDcols = char_cols]

    # Save factor levels
    lev <- lapply(char_cols, function(x) levels(risksets[[x]])) |>
      setNames(char_cols)

    # Convert factors to integer
    risksets[ , (char_cols) := lapply(.SD, as.integer), .SDcols = char_cols]

    # Replace missing values
    risksets[ , (char_cols) := lapply(.SD, nafill, "locf"),
              .SDcols = char_cols]

    # Convert integers to characters again
    for (col in char_cols) set(risksets, NULL, col, lev[[col]][risksets[[col]]])

    # Fill non-character values ================================================
    setnafill(risksets, type = "locf",
              cols = colnames(risksets)[!(colnames(risksets) %in% char_cols)])

    colnames(risksets) <- c("id", "case", "riskset", paste0("case_", names(by)))
  }

  # Report diagnostics if needed -----------------------------------------------
  if(verbose){

    total_comp  <- risksets[case == 0, .N]
    unique_comp <- unique(risksets, by = id)[case == 0, .N]
    prop_unique <- round(unique_comp/total_comp, 3) * 100

    cat("----------------------------------------\n")
    cat("No. of comparators:", total_comp, "\n")
    cat("No. of unique comparators:", unique_comp, "\n")
    cat("Proportion of unique compartors:", prop_unique, "%\n")
    cat("----------------------------------------\n\n")


  }

  # Return original id name
  colnames(risksets)[[1]] <- id_name

  return(as.data.frame(risksets))

}
