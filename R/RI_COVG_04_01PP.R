#' Pre-process dataset for RI_COVG_04
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_04_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import haven

# RI_COVG_04_01PP R version 1.00 - Biostat Global Consulting - 2022-12-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-14  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_04_01PP <- function(VCP = "RI_COVG_04_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # This analysis uses derived variables that are calculated as part
  # of RI_COVG_03, so load up the output from that program.

  check_RI_COVG_03_03DV()

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_03_",ANALYSIS_COUNTER,".rds"))

  dat <- haven::zap_label(dat)
  dat <- haven::zap_labels(dat)

  dat <- dat %>% select(-c(starts_with("fully_vaccinated")))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_04_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("RI_COVG_04_TEMP_DATASETS")){
    RI_COVG_04_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_COVG_04_TEMP_DATASETS,
              c(RI_COVG_04_TEMP_DATASETS,
                paste0("RI_COVG_04_", ANALYSIS_COUNTER, ".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
