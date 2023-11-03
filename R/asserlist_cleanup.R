#' This program mimics Stata's asserlist_cleanup program. It cleans up the two dataframes assertlist generates and export them to an Excel file.
#'
#' @param summary_dataframe the result summary dataframe that assertlist generates
#' @param output_dataframe the detailed result dataframe that assertlist generates
#' @param filename The name of the Excel file
#' @param sheetname The sheetname of the sheet of the Excel file
#' @param fix TRUE or FALSE, default to be FALSE. If set to be true, asserlist generates additional columns to help data managers correct (or 'fix') errant data values
#'
#' @return An Excel file that holds the output of assertlist
#'
#' @import stringr
#' @import openxlsx
#' @import dplyr

# asserlist_cleanup R version 1.00 - Biostat Global Consulting - 2023-11-01
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-11-01  1.00      Mia Yu          Original R package version
# *******************************************************************************

asserlist_cleanup <- function(summary_dataframe, output_dataframe, filename, sheetname,fix = FALSE){

  wb <- createWorkbook()
  addWorksheet(wb, "assertlist summary")
  addWorksheet(wb, sheetname)

  summary_df = get(summary_dataframe, envir = .GlobalEnv)
  names(summary_df) <- c("Assertion Syntax That Failed", "User Specified Additional Information", "Total Number of Observations Included in Assertion", "Number That Failed Assertion", "Number That Passed Assertion","Note","Variables Provided in IDLIST Option","Variables Provided in CHECKLIST Option")


  if (vcqi_object_exists(output_dataframe)){
    output_df = get(output_dataframe, envir = .GlobalEnv)

    start = which(names(output_df) == "function_used")
    output_df <- output_df %>% rename("Assertion Syntax That Failed" = function_used,
                                      "User Specified Additional Information" = tag_used)

    output_df <- rename_with(output_df,
                             ~gsub("check_var", "Checklist: variable ", .x, fixed = TRUE),
                             .cols = starts_with("check_var"))
    output_df <- rename_with(output_df,
                             ~gsub("check_value", "Checklist: variable value ", .x, fixed = TRUE),
                             .cols = starts_with("check_value"))
    output_df <- rename_with(
      output_df,
      ~ gsub("list_var", "Variable used Assertion Syntax ", .x, fixed = TRUE),
      .cols = starts_with("list_var"))
    output_df <- rename_with(
      output_df,
      ~ gsub("list_value", "Current Value of the Variable ", .x, fixed = TRUE),
      .cols = starts_with("list_value"))

    if (fix == TRUE){
      output_df <- rename_with(
        output_df,
        ~ gsub("list_correct", "Blank Space for User to Provide Correct Value of Variable ", .x, fixed = TRUE),
        .cols = starts_with("list_correct")
      )
    }
  }



  writeData(wb, 1, summary_df)
  if (vcqi_object_exists(output_dataframe)){
    writeData(wb, 2, output_df)
  }

  saveWorkbook(wb, filename, overwrite = TRUE)

}
