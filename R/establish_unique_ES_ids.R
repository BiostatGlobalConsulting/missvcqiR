#' Establish unique IDs for the ES dataset
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import tidyselect
#' @rawNamespace import(tools, except = makevars_user)
#'
#' @return a dataset
#'
#' @export
#'
#' @examples
#' establish_unique_ES_ids()

# establish_unique_ES_ids R version 1.00 - Biostat Global Consulting - 2023-08-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-29  1.00      Mia Yu          Original R package version
# *******************************************************************************

#NOTE: did not include any code in the Stata version that is related to level2
establish_unique_ES_ids <- function(VCP = "establish_unique_ES_ids"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){

    # now add ID variables to the ES household interview dataset
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(VCQI_RI_DATASET),"_clean.rds"))

    saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/ES_with_ids.rds"))

    if (!vcqi_object_exists("RI_TEMP_DATASETS")){
      RI_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS,"ES_with_ids"))

    dat <- dat %>% mutate(stratumid = ID02AIid)

    # There is no clusterid in an MOV ES survey; each facility is a
    # stratum and the sample is neither a cluster nor a simple random
    # sample...

    dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET)) %>%
      select(c(ID02AIid,urban_cluster,province_id))
    dat <- left_join(dat,dat2,by = "ID02AIid")

    dat <- dat %>% mutate(level1id = 1,level3id = ID02AIid, level3name = ID02AIname,
                          clusterid = NA, HH02 = NA_character_, HH04 = NA_character_,
                          psweight = 1)

    dat2 <- vcqi_read(LEVEL1_NAME_DATASET)
    dat <- inner_join(dat,dat2,by = "level1id")

    # check for level4 stratifiers and merge them in if necessary
    for (v in seq_along(VCQI_LEVEL4_SET_VARLIST)){
      if (VCQI_LEVEL4_SET_VARLIST[v] %in% names(dat)){
        print(paste0("The stratifier ",VCQI_LEVEL4_SET_VARLIST[v]," is already part of the ES dataset."))
      } else {
        print("Trying to merge from CM")

        dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))
        if (VCQI_LEVEL4_SET_VARLIST[v] %in% names(dat2)){
          dat2 <- dat2 %>% select(c(all_of(VCQI_LEVEL4_SET_VARLIST[v])))
          dat <- left_join(dat,dat2,by = "ID02AIid")
          print(paste0("Variable ",VCQI_LEVEL4_SET_VARLIST[v]," found in CM dataset"))
        } else {
          print(paste0("Did not merge ",VCQI_LEVEL4_SET_VARLIST[v]," onto HW dataset"))
        }
      }
    } # end of VCQI_LEVEL4_SET_VARLIST v loop

    dat <- dat %>% mutate(level2id = 1) #NOTE: slightly different from the Stata version

    saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/ES_with_ids.rds"))
    saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
