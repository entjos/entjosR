#' Save Datasets in Excel Workbook
#'
#' Saves a list of datasets in separate sheets of an excel workbook.
#'
#' @param x
#'    A named list of datasets to be stored in a excel workbook. The list
#'    should be of the structure `list(<sheet name> = data)`. The
#'    dataset will be stored in the excel book work sheet with the name
#'    `<sheet name>`.
#'
#' @param workbook
#'    Path to an `.xlsx` workbook in which the datasets should be saved in.
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
#'    None. Writes the data specified excel workbook sheets.
#'
#' @export write_to_workbook

write_to_workbook <- function(x,
                              workbook,
                              overwrite = FALSE){

  # Check if Excel workbook already exists
  if(file.exists(workbook)){

    # Load Excel workbook
    wb <- openxlsx2::wb_load(workbook)

  } else {

    # Create empty workbook
    wb <- openxlsx2::wb_workbook()

  }

  if(!overwrite & any(names(x) %in% wb$sheet_names)){

    cat("This function will overwrite the content in the following sheets of ",
        "your specified excel workbook(s):\n",
        paste0("   - ",
               names(x)[names(x) %in% wb$sheet_names],
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
  if(!all(names(x) %in% wb$sheet_names)){

    message("Added the following sheets to the workbook(s):")

    additional_names <- names(x)[!(names(x) %in% wb$sheet_names)]

    for(name in additional_names){

      wb$add_worksheet(name)

      message("   - ", name)

    }

  }

  # Write summary tables to workbook
  for(i in seq_along(x)){

    # Clean sheet before adding data
    wb$clean_sheet(sheet = names(x)[[i]])

    wb$add_data(sheet = names(x)[[i]],
                x = x[[i]])
  }

  # Save updated workbook
  wb$save(file = workbook,
          overwrite = TRUE)

  message("Datasets were succesfully saved in:\n",
          workbook)

}
