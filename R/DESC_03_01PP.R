#' Pre-process dataset for DESC_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (DESC_03_<ANALYSIS_COUNTER>_<DESC_03_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# DESC_03_01PP R version 1.01 - Biostat Global Consulting - 2024-03-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-06-04  1.00      Mia Yu          Original R package version
# 2024-03-18  1.01      Mia Yu          Add VCQI_PASS_THRU_VARLIST to selection list
# *******************************************************************************


DESC_03_01PP <- function(VCP = "DESC_03_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/",DESC_03_DATASET))

  #* The user may call this measure several times with different combinations
  #* of inputs, so track a counter so each dataset gets saved for later
  #* examination, if necessary

  if (vcqi_object_exists("DESC_03_COUNTER")){
    DESC_03_COUNTER = DESC_03_COUNTER + 1
    assign("DESC_03_COUNTER", DESC_03_COUNTER, envir = .GlobalEnv)
  }

  if (!vcqi_object_exists("DESC_03_COUNTER")){
    assign("DESC_03_COUNTER", 1, envir = .GlobalEnv)
  }

  dat <- dat %>% select(level1id,level2id,level3id,stratumid,clusterid,respid,
                        HH02,HH04,psweight,all_of(VCQI_LEVEL4_SET_VARLIST),all_of(DESC_03_VARIABLES),
                        all_of(VCQI_PASS_THRU_VARLIST))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/DESC_03_",ANALYSIS_COUNTER,"_",DESC_03_COUNTER,".rds"))

  if (!vcqi_object_exists("DESC_03_TEMP_DATASETS")){
    DESC_03_TEMP_DATASETS <- NULL
  }
  vcqi_global(DESC_03_TEMP_DATASETS,
              c(DESC_03_TEMP_DATASETS,
                paste0("DESC_03_",ANALYSIS_COUNTER,"_",DESC_03_COUNTER,".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
