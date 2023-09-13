#' Pre-process dataset for ES_STUD_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (ES_STUD_02_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import stringr

# ES_STUD_02_01PP R version 1.00 - Biostat Global Consulting - 2023-09-05
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-05  1.00      Mia Yu          Original R package version
# *******************************************************************************


ES_STUD_02_01PP <- function(VCP = "ES_STUD_02_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/MOV_dvs.rds"))
  vc <- str_to_lower(ES_STUD_02_VALID_OR_CRUDE)

  dlist <- NULL

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    dlist <- c(dlist, paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_mov_",vc),
               paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_valid_",vc))
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  dat <- dat %>% select(c(level1id,level2id,level3id,stratumid,clusterid,respid,
                          all_of(VCQI_LEVEL4_SET_VARLIST), all_of(dlist),
                          all_of(paste0("table5_any_valid_",vc)),all_of(paste0("table5_any_mov_",vc)),
                          all_of(paste0("table5_mov_count_",vc)),all_of(paste0("table5_elig_count_",vc)),
                          table5_dv_had_card,table5_dv_had_card_endcap))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_02_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("ES_STUD_02_TEMP_DATASETS")){
    ES_STUD_02_TEMP_DATASETS <- NULL
  }
  vcqi_global(ES_STUD_02_TEMP_DATASETS,
              c(ES_STUD_02_TEMP_DATASETS,
                paste0("ES_STUD_02_",ANALYSIS_COUNTER,".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
