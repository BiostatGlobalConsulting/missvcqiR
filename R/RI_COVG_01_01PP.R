#' Pre-process dataset for RI_COVG_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# RI_COVG_01_01PP R version 1.02 - Biostat Global Consulting - 2023-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-03  1.00      Mia Yu          Original R version
# 2022-10-05  1.01      Mia Yu          Package version
# 2023-07-18  1.02      Mia Yu          Keep level3name and match Stata version
# *******************************************************************************

RI_COVG_01_01PP <- function(VCP = "RI_COVG_01_01PP"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))

  # this program assumes dates and tick marks have been cleaned upstream

  # keep only the variables that pertain to evidence of vaccination
  # for those doses being analyzed
  dlist <- NULL
  for (d in seq_along(RI_DOSE_LIST)){
    dlist <- c(dlist,paste0(RI_DOSE_LIST[d],"_card_date"),paste0(RI_DOSE_LIST[d],"_register_date"),
               paste0(RI_DOSE_LIST[d],"_card_tick"),paste0(RI_DOSE_LIST[d],"_register_tick"),
               paste0(RI_DOSE_LIST[d],"_history"))

    if (RI_DOSE_LIST[d] == "bcg"){
      dlist <- c(dlist, "bcg_scar_history")
      if (!("bcg_scar_history" %in% names(dat))){
        dat <- mutate(dat, bcg_scar_history = NA)
      }
    } #end of bcg part
  } #end of dose loop

  dat <- select(dat, c(level1id, level2id, level3id, level3name, stratumid, clusterid,
                       respid, RI01, RI03, RI11, RI12,HH02, HH04, psweight,
                       all_of(VCQI_LEVEL4_SET_VARLIST), all_of(dlist),
                       no_card, age_at_interview))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("RI_COVG_01_TEMP_DATASETS")){
    RI_COVG_01_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_COVG_01_TEMP_DATASETS,c(RI_COVG_01_TEMP_DATASETS,paste0("RI_COVG_01_",ANALYSIS_COUNTER,".rds")))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}


