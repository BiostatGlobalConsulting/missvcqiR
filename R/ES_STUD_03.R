#' Comparison of invalid, valid and MOVs before and on the study day
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' ES_STUD_03()

# ES_STUD_03 R version 1.00 - Biostat Global Consulting - 2023-09-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-11  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_03 <- function(VCP = "ES_STUD_03"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP))

  print("Checking global macros")
  ES_STUD_03_00GC()

  if (VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    ES_STUD_03_01PP()
  }

  if (VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    ES_STUD_03_03DV()
  }

  if (VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    ES_STUD_03_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    ES_STUD_03_05TOST()
  }

  # if (MAKE_PLOTS %in% 1){
  #   print("Making plots")
  #   ES_STUD_03_06PO()
  # }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
