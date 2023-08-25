#' Improper Vaccine Practice Barriers
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' HW_PRAC_01()

# HW_PRAC_01 R version 1.00 - Biostat Global Consulting - 2023-08-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# *******************************************************************************

HW_PRAC_01 <- function(VCP = "HW_PRAC_01"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP))

  if (VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    HW_PRAC_01_01PP()
  }

  if (VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    HW_PRAC_01_03DV()
  }

  if (VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    HW_PRAC_01_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    HW_PRAC_01_05TOST()
  }

  if (MAKE_PLOTS %in% 1){
    print("Making plots")
    HW_PRAC_01_06PO()
  }

vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
