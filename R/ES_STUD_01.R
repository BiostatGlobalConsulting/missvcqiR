#' Invalid doses administered on study day
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' ES_STUD_01()

# ES_STUD_01 R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_01 <- function(VCP = "ES_STUD_01"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP))

  print("Checking global macros")
  ES_STUD_01_00GC()

  if (VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    ES_STUD_01_01PP()
  }

  if (VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    ES_STUD_01_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    ES_STUD_01_05TOST()
  }

  if (MAKE_PLOTS %in% 1){
    print("Making plots")
    ES_STUD_01_06PO()
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
