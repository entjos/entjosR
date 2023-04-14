#' Summary Table to Workbook
#'
#' Saves a list of summary tables in separate sheets of an excel workbook. This
#' function can be handy when creating tables for publication in combination
#' with some additional formatting in Excel.
#'
#' @param workbook
#'    Path to an `.xlsx` workbook in which the summary tables should be saved
#'    in.
#'
#' @param summary_tables
#'    A list of summary tables created with `summary_table`.
#'
#' @param overwrite
#'    This function will change the excel workbook specified in `workbook`.
#'    In order to avoid unintended loss of data, the function will list the
#'    workbook sheets that will be overwritten and asks you whether you would
#'    like to continue. You can set `overwrite` to `TRUE` in order to avoid
#'    the prompt. This can be useful if the function is used outside of an
#'    interactive mode.
#'
#' @return
#'    Write the data stored in the summary tables in the specified
#'    excel workbook.
#'
#' @export summary_table_to_workbook

summary_table_to_workbook <- function(workbook,
                                      summary_tables,
                                      overwrite = FALSE){

  if(is.null(attr(workbook, "no_summary_tables"))){

    stop("`summary_tables` must be a stratified summary table.")

  }

  # Load Excel workbook
  wb <- openxlsx::loadWorkbook(workbook)

  if(!overwrite & any(names(summary_tables) %in% wb$sheet_names)){

    cat("This function will overwrite the content in the following sheets of ",
        "your specified excel workbook(s):\n",
        paste0("   - ",
               names(summary_tables)[names(summary_tables) %in% wb$sheet_names],
               "\n"),
        "\nPlease specify one of the following options:\n",
        "   0: No, please stop here\n",
        "   1: Yes, I'd like to continue\n",
        sep  = "")

    test <- readline(prompt = "Enter here: ")

    if(test == 1){
      message("To avoid the prompting please specify `overwrite = TRUE` in ",
              "your next call to `summary_table_to_workbook`.")
    } else {

      stop("No worries nothing has changed. Please create a new workbook that",
           "you can overwrite for your next call to `summary_table_to_workbook`.")

    }

  }

  # Add additional sheets if not already included in the workbook
  if(!all(names(summary_tables) %in% wb$sheet_names)){

    message("Added the following sheets to the workbook(s):")

    add <- names(summary_tables)[!(names(summary_tables) %in% wb$sheet_names)]

    for(x in add){

      openxlsx::addWorksheet(wb, x)

      message("   - ", x)

    }

  }

  # Write summary tables to workbook
  for(i in seq_along(summary_tables)){
    openxlsx::writeData(wb,
                        sheet = names(summary_tables)[[i]],
                        x = summary_tables[[i]])
  }

  # Save updated workbook
  openxlsx::saveWorkbook(wb = wb,
                         file = workbook,
                         overwrite = TRUE)

  message("Summary tables were succesfully saved in:\n",
          workbook)

}
