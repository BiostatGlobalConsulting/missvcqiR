#' Pre-process dataset for ES_STUD_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (ES_STUD_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import stringr

# ES_STUD_01_01PP R version 1.00 - Biostat Global Consulting - 2023-09-05
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-05  1.00      Mia Yu          Original R package version
# *******************************************************************************


ES_STUD_01_01PP <- function(VCP = "ES_STUD_01_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/MOV_dvs.rds"))
  vc <- str_to_lower(ES_STUD_01_VALID_OR_CRUDE)

  dlist <- NULL

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    dlist <- c(dlist, paste0("table4_dv_",MOV_OUTPUT_DOSE_LIST[d],"_",vc))
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  dat <- dat %>% select(c(level1id,level2id,level3id,stratumid,clusterid,respid,
                          all_of(VCQI_LEVEL4_SET_VARLIST), all_of(dlist),
                          all_of(paste0("table4_dv_not_elig_",vc))))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_01_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("ES_STUD_01_TEMP_DATASETS")){
    ES_STUD_01_TEMP_DATASETS <- NULL
  }
  vcqi_global(ES_STUD_01_TEMP_DATASETS,
              c(ES_STUD_01_TEMP_DATASETS,
                paste0("ES_STUD_01_",ANALYSIS_COUNTER,".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
