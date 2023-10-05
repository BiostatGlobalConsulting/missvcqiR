#' Response to multiple-choice question with only one response
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' DESC_02()

# DESC_02 R version 1.00 - Biostat Global Consulting - 2023-05-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-24  1.00      Mia Yu          Original R package version
# *******************************************************************************

DESC_02 <- function(VCP = "DESC_02", cleanup = FALSE){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (vcqi_object_exists("DESC_02_VARIABLES")){
    print(paste0("Calculating ", VCP, " for ", DESC_02_VARIABLES, "..."))
  } else{
    print(paste0("Calculating ", VCP, " for DESC_02 ..."))
  }

  print("Checking global macros")
  DESC_02_00GC()

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    DESC_02_01PP()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    DESC_02_03DV()
  }

  if(VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    DESC_02_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    DESC_02_05TOST()
  }

  # * Clear out globals if specified
  if (cleanup == TRUE){
    rm(list = c("DESC_02_DATASET","DESC_02_VARIABLES","DESC_02_WEIGHTED","DESC_02_DENOMINATOR","DESC_02_TO_TITLE",
                "DESC_02_TO_SUBTITLE","DESC_02_N_LABEL","DESC_02_NTWD_LABEL"),envir = .GlobalEnv)%>% suppressWarnings()

    if (vcqi_object_exists("DESC_02_N_RELABEL_LEVELS")){
      for (i in 1:DESC_02_N_RELABEL_LEVELS){
        rm(list = c(
          paste0("DESC_02_RELABEL_LEVEL_", i),
          paste0("DESC_02_RELABEL_LABEL_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    if (vcqi_object_exists("DESC_02_N_SUBTOTALS")){
      for (i in 1:DESC_02_N_SUBTOTALS){
        rm(list = c(
          paste0("DESC_02_SUBTOTAL_LEVELS_", i),
          paste0("DESC_02_SUBTOTAL_LABEL_", i),
          paste0("DESC_02_SUBTOTAL_LIST_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    for (i in 1:100){
      rm(list = c(paste0("DESC_02_TO_FOOTNOTE_", i)),envir = .GlobalEnv) %>% suppressWarnings()
    }

    rm(list = c("DESC_02_N_SUBTOTALS","DESC_02_N_RELABEL_LEVELS","DESC_02_SHOW_SUBTOTALS_ONLY",
                "DESC_02_LIST_N_BEFORE_PCT","DESC_02_LIST_NWTD_BEFORE_PCT", "DESC_02_SUPPRESS_CI_OUTPUT"),
       envir = .GlobalEnv)%>% suppressWarnings()

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
