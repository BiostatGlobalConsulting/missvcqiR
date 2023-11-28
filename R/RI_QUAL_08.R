#' Calculate percent of visits with missed opportunities for simultaneous vaccination (MOV)
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases, tables, and plots
#' @export
#'
#' @examples
#' RI_QUAL_08()

# RI_QUAL_08 R version 1.01 - Biostat Global Consulting - 2022-10-12
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Mia Yu          Original R version
# 2022-10-12  1.01      Mia Yu          Package version
# *******************************************************************************

RI_QUAL_08 <- function(VCP = "RI_QUAL_08"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_NO_DOBS == 1){
    vcqi_log_comment(VCP, 2, "Warning", "User requested RI_QUAL_08 but no respondents have full date of birth info, so the indicator will be skipped.")
  } else{

    print(paste0("Calculating ", VCP))

    print("Checking global macros")
    RI_QUAL_08_00GC()

    if (VCQI_PREPROCESS_DATA %in% 1){
      print("Pre-processing dataset")
      RI_QUAL_08_01PP()
    }

    if (VCQI_GENERATE_DATABASES %in% 1){
      print("Generating output databases")
      RI_QUAL_08_04GO()
    }

    if(EXPORT_TO_EXCEL %in% 1){
      print("Exporting to Excel")
      RI_QUAL_08_05TOST()
    }

    if (MAKE_PLOTS %in% 1){
      print("Making plots")
      RI_QUAL_08_06PO()
    }
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
