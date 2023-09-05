#' This program makes one of the datasets needed for later R Shiny visualization
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import tidyselect
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr

#' @return a dataset
#'
#' @export
#'
#' @examples
#' calculate_previsit_MOV_flags()

# calculate_previsit_MOV_flags R version 1.00 - Biostat Global Consulting - 2023-08-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-29  1.00      Mia Yu          Original R package version
# *******************************************************************************

# This program makes one of the datasets needed for later R Shiny
# visualization: MOV flags for all the visits BEFORE the study day.
#
# Outline:
# Make a version of RI_with_ids that excludes the visit date
# Run calculate_MOV_flags
# Set aside the output
# Tidy up

calculate_previsit_MOV_flags <- function(VCP = "calculate_previsit_MOV_flags"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){
    # Set aside a copy of the data
    file.copy(from = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"),
              to = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids_full_dataset.rds"), overwrite = TRUE)

    # Be sure that 'visit' is the last dose in the list
    lower <- str_to_lower(RI_DOSE_LIST)
    RI_no_visit <- lower[-which(lower == "visit")]
    vcqi_global(RI_DOSE_LIST,RI_no_visit)
    vcqi_global(RI_DOSE_LIST, c(RI_DOSE_LIST,"visit"))


    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))
    for (d in seq_along(RI_DOSE_LIST)){
      var <- rlang::sym(paste0(RI_DOSE_LIST[d],"_card_date"))
      dat <- dat %>% mutate(!!var := ifelse((!!var == visit_card_date) %in% TRUE, NA_Date_, !!var))
      dat <- dat %>% mutate(!!var := as_date(!!var, origin = lubridate::origin))
    } #end of RI_DOSE_LIST d loop

    saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))

    if (!vcqi_object_exists("VCQI_TESTING_CODE")){
      vcqi_global(VCQI_TESTING_CODE,0)
    }
    vcqi_global(SAVE_VCQI_TESTING_CODE,VCQI_TESTING_CODE)
    vcqi_global(VCQI_TESTING_CODE,1)

    calculate_MOV_flags()

    # Copy output for later merging
    file.copy(from = paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_flags_to_merge.rds"),
              to = paste0(VCQI_OUTPUT_FOLDER,"/RI_previsit_MOV_flags_to_merge.rds"), overwrite = TRUE)

    # Save a copy of the input file, just in case
    file.copy(from = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"),
              to = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids_previsit.rds"), overwrite = TRUE)

    # Restore RI_with_ids
    file.copy(from = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids_full_dataset.rds"),
              to = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"), overwrite = TRUE)

    # Restore VCQI_TESTING_CODE global
    vcqi_global(VCQI_TESTING_CODE,SAVE_VCQI_TESTING_CODE)

    if (!vcqi_object_exists("RI_TEMP_DATASETS")){
      RI_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_TEMP_DATASETS,
                c(RI_TEMP_DATASETS,"RI_previsit_MOV_flags_to_merge","RI_with_ids_full_dataset","RI_with_ids_previsit"))
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
