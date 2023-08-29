#' Response to multiple-choice question where the respondent may select more than one response option.
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' DESC_03_MV()

# DESC_03_MV R version 1.00 - Biostat Global Consulting - 2023-06-04
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-06-04  1.00      Mia Yu          Original R package version
# *******************************************************************************

DESC_03_MV <- function(VCP = "DESC_03_MV", cleanup = FALSE){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, " ..."))

  print("Checking global macros")
  DESC_03_00GC()

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    DESC_03_01PP()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    DESC_03_03DV()
  }

  if(VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    DESC_03_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    DESC_03_05TOST_MV()
  }

  # * Clear out globals if specified
  if (cleanup == TRUE){
    rm(list = c("DESC_03_DATASET","DESC_03_VARIABLES","DESC_03_WEIGHTED","DESC_03_DENOMINATOR","DESC_03_TO_TITLE",
                "DESC_03_TO_SUBTITLE","DESC_03_SHORT_TITLE","DESC_03_SELECTED_VALUE","DESC_03_N_LABEL","DESC_03_NTWD_LABEL"),envir = .GlobalEnv)%>% suppressWarnings()

    if (vcqi_object_exists("DESC_03_N_RELABEL_LEVELS")){
      for (i in 1:DESC_03_N_RELABEL_LEVELS){
        rm(list = c(
          paste0("DESC_03_RELABEL_LEVEL_", i),
          paste0("DESC_03_RELABEL_LABEL_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    if (vcqi_object_exists("DESC_03_N_SUBTOTALS")){
      for (i in 1:DESC_03_N_SUBTOTALS){
        rm(list = c(
          paste0("DESC_03_SUBTOTAL_LEVELS_", i),
          paste0("DESC_03_SUBTOTAL_LABEL_", i),
          paste0("DESC_03_SUBTOTAL_LIST_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    for (i in 1:100){
      rm(list = c(paste0("DESC_03_TO_FOOTNOTE_", i)),envir = .GlobalEnv) %>% suppressWarnings()
    }

    rm(list = c("DESC_03_N_SUBTOTALS","DESC_03_N_RELABEL_LEVELS","DESC_03_SHOW_SUBTOTALS_ONLY",
                "DESC_03_LIST_N_BEFORE_PCT","DESC_03_LIST_NWTD_BEFORE_PCT", "DESC_03_SUPPRESS_CI_OUTPUT"),
       envir = .GlobalEnv) %>% suppressWarnings()

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
