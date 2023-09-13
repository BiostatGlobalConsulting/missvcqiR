#' Pre-process dataset for HW_BAR_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (HW_BAR_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @importFrom utils glob2rx

# HW_BAR_01_01PP R version 1.00 - Biostat Global Consulting - 2023-08-22
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-22  1.00      Mia Yu          Original R package version
# *******************************************************************************


HW_BAR_01_01PP <- function(VCP = "HW_BAR_01_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/HW_with_ids.rds"))

  list4 <- c(LETTERS[1:10])
  list5 <- c(LETTERS[1:9])
  dlist <- NULL
  keeplist <- NULL

  for (i in seq_along(list4)){
    dlist <- c(dlist,paste0("HW04A",list4[i],"*"))
  }
  for (i in seq_along(list5)){
    dlist <- c(dlist,paste0("HW05A",list5[i],"*"))
  }

  for (v in seq_along(dlist)){
    keeplist <- c(keeplist,grep(glob2rx(dlist[v]), names(dat), value=TRUE))
  }

  dat <- dat %>% select(c(level1id,level2id,level3id,stratumid,clusterid,respid, all_of(VCQI_LEVEL4_SET_VARLIST), all_of(keeplist)))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/HW_BAR_01_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("HW_BAR_01_")){
    HW_BAR_01_ <- NULL
  }
  vcqi_global(HW_BAR_01_,c(HW_BAR_01_,paste0("HW_BAR_01_",ANALYSIS_COUNTER,".rds")))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
