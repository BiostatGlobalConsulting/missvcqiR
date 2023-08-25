#' Export datasets to Excel for HW_PRAC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# HW_PRAC_01_05TOST R version 1.00 - Biostat Global Consulting - 2023-08-25
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-25  1.00      Mia Yu          Original R package version
# *******************************************************************************

HW_PRAC_01_05TOST <- function(VCP = "HW_PRAC_01_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_HW_PRAC_01", "TO_HW_PRAC_01_columnlabel", "TO_HW_PRAC_01_formatnum","TO_HW_PRAC_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  print("No Improper Vx practices")
  vid <- 0

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_50"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #No Improper Vx Practices (N)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_50"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #No Improper Vx Practices (%)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_48")) #N

  print("Improper Vx practices - Part 1")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_40"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Improper Vx practices - Part 1 (N)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_40"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Improper Vx practices - Part 1 (%)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_48")) #N

  print("Improper Vx practices - Part 2")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_41"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Improper Vx practices - Part 2 (N)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_41"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Improper Vx practices - Part 2 (%)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_48")) #N

  print("Improper Vx practices - Any")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_38"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Improper Vx practices - Any (N)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_38"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Improper Vx practices - Any (%)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_48")) #N

  print("Improper Vx practices - Both")
  vid <- vid + 1

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_39"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Improper Vx practices - Both (N)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_39"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Improper Vx practices - Both (%)

  make_table_column(
    tablename = "TO_HW_PRAC_01",
    dbfilename = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_",vid,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_48")) #N

  export_table_to_excel(indicator = "HW_PRAC_01",brief = FALSE)

  rm(list = c("TO_HW_PRAC_01", "TO_HW_PRAC_01_columnlabel", "TO_HW_PRAC_01_formatnum","TO_HW_PRAC_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_HW_PRAC_01_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
