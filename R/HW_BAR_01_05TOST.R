#' Export datasets to Excel for HW_BAR_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# HW_BAR_01_05TOST R version 1.00 - Biostat Global Consulting - 2023-08-25
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-25  1.00      Mia Yu          Original R package version
# *******************************************************************************

HW_BAR_01_05TOST <- function(VCP = "HW_BAR_01_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_HW_BAR_01", "TO_HW_BAR_01_columnlabel", "TO_HW_BAR_01_formatnum","TO_HW_BAR_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  print("No barriers")
  vid <- 0

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #No Barriers (N)

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #No Barriers (%)

  print("Knowledge barriers")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Knowledge Barriers (N)

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Knowledge Barriers (%)

  print("Attitude barriers")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Attitude Barriers (N)

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Attitude Barriers (%)

  print("Any barriers")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Any Barriers (N)

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Any Barriers (%)

  print("Both barriers")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Both Barriers (N)

  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_49"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Both Barriers (%)

  # Now grab the N for overall
  make_table_column(
    tablename = "TO_HW_BAR_01",
    dbfilename = paste0("HW_BAR_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_74")) #Total (N)

  export_table_to_excel(indicator = "HW_BAR_01",brief = FALSE)

  rm(list = c("TO_HW_BAR_01", "TO_HW_BAR_01_columnlabel", "TO_HW_BAR_01_formatnum","TO_HW_BAR_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_HW_BAR_01_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
