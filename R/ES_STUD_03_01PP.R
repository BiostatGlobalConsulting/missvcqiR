#' Pre-process dataset for ES_STUD_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (ES_STUD_03_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import stringr

# ES_STUD_03_01PP R version 1.00 - Biostat Global Consulting - 2023-09-05
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-05  1.00      Mia Yu          Original R package version
# *******************************************************************************


ES_STUD_03_01PP <- function(VCP = "ES_STUD_03_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_by_visit_dose_category.rds"))
  vc <- str_to_lower(ES_STUD_03_VALID_OR_CRUDE)

  dlist <- NULL

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    dlist <- c(dlist, paste0("correct_nodose_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
               paste0("invalid_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
               paste0("mov_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
               paste0("correct_validdose_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
               "has_card_with_dob_and_dose",
               paste0("elig_",MOV_OUTPUT_DOSE_LIST[d],"_",vc))
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  dat <- dat %>% select(c(level1id,level2id,level3id,stratumid,clusterid,respid,
                          all_of(VCQI_LEVEL4_SET_VARLIST), all_of(dlist),
                          visitdate,visit_card_date))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("ES_STUD_03_TEMP_DATASETS")){
    ES_STUD_03_TEMP_DATASETS <- NULL
  }
  vcqi_global(ES_STUD_03_TEMP_DATASETS,
              c(ES_STUD_03_TEMP_DATASETS,
                paste0("ES_STUD_03_",ANALYSIS_COUNTER,".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
