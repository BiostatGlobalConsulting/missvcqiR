#' Pre-process dataset for HW_PRAC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (HW_PRAC_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom utils glob2rx

# HW_PRAC_01_01PP R version 1.00 - Biostat Global Consulting - 2023-08-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# *******************************************************************************


HW_PRAC_01_01PP <- function(VCP = "HW_PRAC_01_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/HW_with_ids.rds"))

  list <- c(LETTERS[1:11])
  dlist <- NULL
  keeplist <- NULL

  for (i in seq_along(list)){
    dlist <- c(dlist,paste0("HW06A",list[i],"*"))
  }

  for (v in seq_along(dlist)){
    keeplist <- c(keeplist,grep(glob2rx(dlist[v]), names(dat), value=TRUE))
  }

  #TODO: level2id?
  dat <- dat %>% select(c(level1id,level2id,level3id,stratumid,clusterid,respid, all_of(VCQI_LEVEL4_SET_VARLIST), all_of(keeplist)))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("HW_PRAC_01_")){
    HW_PRAC_01_ <- NULL
  }
  vcqi_global(HW_PRAC_01_,c(HW_PRAC_01_,paste0("HW_PRAC_01_",ANALYSIS_COUNTER,".rds")))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
