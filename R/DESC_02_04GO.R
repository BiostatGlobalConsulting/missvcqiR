#' Generate output databases for DESC_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER


# DESC_02_04GO R version 1.00 - Biostat Global Consulting - 2023-05-26
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-26  1.00      Mia Yu          Original R package version
# *******************************************************************************


DESC_02_04GO <- function(VCP = "DESC_02_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vid <- 1
  for (d in seq_along(DESC_02_VARIABLES)){
    rm(list = c(paste0("DESC_02_labels_",vid)), envir = .GlobalEnv) %>% suppressWarnings()
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_", DESC_02_COUNTER, ".rds"))
    tempvar <- get(DESC_02_VARIABLES[d], dat)
    varlabel <- attr(tempvar,"label")
    make_DESC_0203_output_database(variable = DESC_02_VARIABLES[d],
                                   label = varlabel,
                                   vid = vid,
                                   measureid = "DESC_02")
    vid <- vid + 1
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
