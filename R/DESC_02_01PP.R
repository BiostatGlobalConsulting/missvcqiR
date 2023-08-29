#' Pre-process dataset for DESC_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (DESC_02_<ANALYSIS_COUNTER>_<DESC_02_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# DESC_02_01PP R version 1.00 - Biostat Global Consulting - 2023-05-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-23  1.00      Mia Yu          Original R package version
# *******************************************************************************


DESC_02_01PP <- function(VCP = "DESC_02_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/",DESC_02_DATASET))

  #* The user may call this measure several times with different combinations
  #* of inputs, so track a counter so each dataset gets saved for later
  #* examination, if necessary

  if (vcqi_object_exists("DESC_02_COUNTER")){
    DESC_02_COUNTER = DESC_02_COUNTER + 1
    assign("DESC_02_COUNTER", DESC_02_COUNTER, envir = .GlobalEnv)
  }

  if (!vcqi_object_exists("DESC_02_COUNTER")){
    assign("DESC_02_COUNTER", 1, envir = .GlobalEnv)
  }

  dat <- dat %>% select(level1id,level2id,level3id,stratumid,clusterid,respid,
                        HH02,HH04,psweight,all_of(VCQI_LEVEL4_SET_VARLIST),all_of(DESC_02_VARIABLES))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/DESC_02_",ANALYSIS_COUNTER,"_",DESC_02_COUNTER,".rds"))

  if (!vcqi_object_exists("DESC_02_TEMP_DATASETS")){
    DESC_02_TEMP_DATASETS <- NULL
  }
  vcqi_global(DESC_02_TEMP_DATASETS,
              c(DESC_02_TEMP_DATASETS,
                paste0("DESC_02_",ANALYSIS_COUNTER,"_",DESC_02_COUNTER,".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
