#' Generate output databases for HW_BAR_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER


# HW_BAR_01_04GO R version 1.00 - Biostat Global Consulting - 2023-08-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# *******************************************************************************


HW_BAR_01_04GO <- function(VCP = "HW_BAR_01_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print("No barriers")
  make_unwtd_output_database_MV(variable = "no_barriers",
                             estlabel = "No Barriers",
                             vid = 0,
                             measureid = "HW_BAR_01")
  print("Knowledge barriers")
  make_unwtd_output_database_MV(variable = "knowledge_barriers",
                             estlabel = "Knowledge Barriers",
                             vid = 1,
                             measureid = "HW_BAR_01")
  print("Attitude barriers")
  make_unwtd_output_database_MV(variable = "attitude_barriers",
                             estlabel = "Attitude Barriers",
                             vid = 2,
                             measureid = "HW_BAR_01")
  print("Any barriers")
  make_unwtd_output_database_MV(variable = "any_barriers",
                             estlabel = "Any Barriers",
                             vid = 3,
                             measureid = "HW_BAR_01")
  print("Both barriers")
  make_unwtd_output_database_MV(variable = "both_barriers",
                             estlabel = "Both Barriers",
                             vid = 4,
                             measureid = "HW_BAR_01")

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
