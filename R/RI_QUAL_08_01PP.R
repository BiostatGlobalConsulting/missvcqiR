#' Pre-process dataset for RI_QUAL_08
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_08_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import stringr

# RI_QUAL_08_01PP R version 1.02 - Biostat Global Consulting - 2024-03-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Mia Yu          Original R version
# 2022-10-12  1.01      Mia Yu          Package version
# 2024-03-20  1.02      Mia Yu          Add VCQI_PASS_THRU_VARLIST to selection list
# *******************************************************************************

RI_QUAL_08_01PP <- function(VCP = "RI_QUAL_08_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(RI_QUAL_08_VALID_OR_CRUDE)

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))

  dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_flags_to_merge.rds"))
  dat <- left_join(dat, dat2, by = "respid")

  dlist <- NULL
  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    dlist <- c(dlist,
               paste0("total_mov_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
               paste0("total_elig_",MOV_OUTPUT_DOSE_LIST[d],"_",vc))
  }

  vars <- c(paste0("total_mov_visits_",vc),paste0("total_elig_visits_",vc),paste0("total_movs_",vc))
  dat <- dat %>% select(level1id,level2id,level3id,stratumid,clusterid,respid,RI01,RI03,RI11,RI12,
                        HH02,HH04,psweight,all_of(VCQI_LEVEL4_SET_VARLIST),all_of(dlist),all_of(vars),
                        all_of(VCQI_PASS_THRU_VARLIST))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_08_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("RI_QUAL_08_TEMP_DATASETS")){
    RI_QUAL_08_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_QUAL_08_TEMP_DATASETS,c(RI_QUAL_08_TEMP_DATASETS,paste0("RI_QUAL_08_",ANALYSIS_COUNTER,".rds")))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

