#' Pre-process dataset for RI_COVG_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_03_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# RI_COVG_03_01PP R version 1.01 - Biostat Global Consulting - 2023-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-13  1.00      Mia Yu          Original R package version
# 2023-07-18  1.01      Mia Yu          Keep level3name and match Stata version
# *******************************************************************************


RI_COVG_03_01PP <- function(VCP = "RI_COVG_03_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  check_RI_COVG_01_03DV()

  if (VCQI_NO_DOBS != 1){
    check_RI_COVG_02_03DV()

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_02_",ANALYSIS_COUNTER,".rds"))

    dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))
    dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
    dupname <- dupname[which(!dupname %in% c("respid"))]
    dat2 <- dat2 %>% select(-c(all_of(dupname)))

    dat <- full_join(dat,dat2,by = "respid")
  } else {
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))
  }

  dlist <- NULL

  for (d in seq_along(RI_DOSE_LIST)){
    dlist <- c(dlist, paste0("got_crude_",RI_DOSE_LIST[d],"_to_analyze"))

    if (VCQI_NO_DOBS != 1){
      dlist <- c(dlist,
                 paste0("got_valid_",RI_DOSE_LIST[d],"_to_analyze"),
                 paste0("valid_",RI_DOSE_LIST[d],"_age1_to_analyze"))
    }

  } #end of RI_DOSE_LIST d loop

  dat <- dat %>% select(level1id,level2id,level3id,level3name,stratumid,clusterid,respid,RI01,RI03,RI11,RI12,
                        HH02,HH04,psweight,all_of(VCQI_LEVEL4_SET_VARLIST),age_at_interview,all_of(dlist))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_03_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("RI_COVG_03_TEMP_DATASETS")){
    RI_COVG_03_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_COVG_03_TEMP_DATASETS,
              c(RI_COVG_03_TEMP_DATASETS,
                paste0("RI_COVG_03_", ANALYSIS_COUNTER, ".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
