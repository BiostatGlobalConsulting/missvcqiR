#' Generate output databases for DESC_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER


# DESC_03_04GO R version 1.00 - Biostat Global Consulting - 2023-06-06
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-06-06  1.00      Mia Yu          Original R package version
# *******************************************************************************


DESC_03_04GO <- function(VCP = "DESC_03_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c(paste0("DESC_03_labels_",DESC_03_COUNTER)), envir = .GlobalEnv) %>% suppressWarnings()

  make_DESC_0203_output_database(variable = paste0("desc03_",ANALYSIS_COUNTER,"_",DESC_03_COUNTER),
                                 label = DESC_03_TO_TITLE,
                                 vid = DESC_03_COUNTER,
                                 measureid = "DESC_03")

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
