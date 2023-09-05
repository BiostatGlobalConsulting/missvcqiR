#' Check ES analysis-related globals, datasets and variables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#'
#' @return Log entries; errors if conditions not met
#'
#' @export
#'
#' @examples
#' check_ES_analysis_metadata()

# check_ES_analysis_metadata R version 1.00 - Biostat Global Consulting - 2023-08-28
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-28  1.00      Mia Yu          Original R package version
# *******************************************************************************


check_ES_analysis_metadata <- function(VCP = "check_ES_analysis_metadata"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Check the generic analysis-related globals
  check_analysis_metadata_MV()

  # Check the CM dataset
  check_miss_VCQI_CM_metadata()

  # Check ES-specific globals
  #
  # The remainder of this program is a copy of a large portion of VCQI's check_RI_analysis_metadata
	# but without the VCQI CM checks.

  exitflag <- 0
  errormsgs <- NULL

  # Check that user has specified doses to analyze
  vcqi_log_global(RI_SINGLE_DOSE_LIST)
  vcqi_log_global(RI_MULTI_2_DOSE_LIST)
  vcqi_log_global(RI_MULTI_3_DOSE_LIST)

  if (!vcqi_object_exists("RI_SINGLE_DOSE_LIST") &
      !vcqi_object_exists("RI_MULTI_2_DOSE_LIST") &
      !vcqi_object_exists("RI_MULTI_3_DOSE_LIST")){
    errormsgs <- c(errormsgs,"No RI doses are identified for analysis...so quit")
    vcqi_log_comment(VCP,1,"Error", "No RI doses are identified for analysis...so quit")
    exitflag <- 1
  }

  if (exitflag == 1) {
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(halt_message = errormsgs)
  }

  if(!vcqi_object_exists("VCQI_RI_DATASET")){
    errormsgs <- c(errormsgs,
                   "Please set VCQI_RI_DATASET.")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error", "Please set VCQI_RI_DATASET.")
  } else {
    # Check that RI dataset exists
    vcqi_log_global(VCQI_DATA_FOLDER)
    vcqi_log_global(VCQI_RI_DATASET)
    vcqi_log_global(VCQI_RIHC_DATASET)

    RIfile <- paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET)
    if(!file.exists(RIfile)){
      errormsgs <- c(errormsgs,
                     paste0("The file defined by global macros VCQI_OUTPUT_FOLDER/VCQI_RI_DATASET (", RIfile, ") does not exist"))
      exitflag <- 1
      vcqi_log_comment(VCP, 1, "Error", paste0("RI dataset: ", RIfile, " does not exist"))
    } else {
      #read the RI dataset
      dat <- vcqi_read(RIfile)
      if(is.data.frame(dat) == FALSE){
        errormsgs <- c(errormsgs,
                       paste0("The file defined by global macros VCQI_OUTPUT_FOLDER/VCQI_RI_DATASET (", RIfile, ") is not in valid format"))
        exitflag <- 1
        vcqi_log_comment(VCP, 1, "Error", paste0("RI dataset: ", RIfile, " is not in valid format"))
      } else{

        #updated 7/23
        #TODO: ask Dale if we should keep this part
        vcqi_global(RI_DOSE_LIST,str_to_lower(RI_DOSE_LIST))

        #Check that RI variables used across all indicators are present
        #and have the correct variable type
        dlist <- NULL
        for (v in seq_along(RI_DOSE_LIST)){
          dlist <- c(dlist,paste0(RI_DOSE_LIST[v],"_date_card_d"),paste0(RI_DOSE_LIST[v],"_date_card_m"),paste0(RI_DOSE_LIST[v],"_date_card_y"),
                     paste0(RI_DOSE_LIST[v],"_tick_card"),paste0(RI_DOSE_LIST[v],"_history"))

          if(RI_DOSE_LIST[v] == "bcg"){
            dlist <- c(dlist,"bcg_scar_history")
          }
        } #end of RI_DOSE_LIST v loop

        varlist <- c("RI01", "RI03", "RI11", "RI12", dlist, "dob_date_card_d", "dob_date_card_m", "dob_date_card_y", "dob_date_history_m", "dob_date_history_d", "dob_date_history_y")
        for(v in seq_along(varlist)){
          if (varlist[v] %in% names(dat)){
            #If the variable exists, confirm the variable is not missing and has the correct variable type
            if (varlist[v] != "RI11"){
              variable <- get(varlist[v],dat)
              if(all(!class(variable) %in% c("numeric", "double"))){
                errormsgs <- c(errormsgs,
                               paste0(varlist[v], " needs to be a numeric variable in RI dataset."))
                exitflag <- 1
                vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " needs to be a numeric variable in RI dataset."))
              }
            }

            if (varlist[v] %in% c("RI01", "RI03", "RI11", "RI12")){
              variable2 <- get(varlist[v],dat)
              if ("character" %in% class(variable2)){
                if(any(is.na(variable2) | variable2 == "")){
                  errormsgs <- c(errormsgs,
                                 paste0(varlist[v], " cannot have a missing value in the RI dataset."))
                  exitflag <- 1
                  vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " cannot have a missing value in the RI dataset."))
                }
              } else if(any(is.na(variable2))){
                errormsgs <- c(errormsgs,
                               paste0(varlist[v], " cannot have a missing value in the RI dataset."))
                exitflag <- 1
                vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " cannot have a missing value in the RI dataset."))
              }

            } # end of checking "RI01", "RI03", "RI11", "RI12"

          } else {
            errormsgs <- c(errormsgs,
                           paste0("Variable ", varlist[v], " does not exist in RI dataset and is required to run VCQI."))
            exitflag <- 1
            vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " does not exist in RI dataset and is required to run VCQI."))
          }
        }
      }

    }

  }

  # If we are using RIHC records, check that the dataset exists
  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    if(!vcqi_object_exists("VCQI_RIHC_DATASET")) {
      errormsgs <- c(errormsgs,
                     "Please set VCQI_RIHC_DATASET.")
      exitflag <- 1
      vcqi_log_comment(VCP, 1, "Error", paste0("Please set VCQI_RIHC_DATASET."))
    } else if(vcqi_object_exists("VCQI_RIHC_DATASET")){
      RIHCfile <- paste0(VCQI_DATA_FOLDER, "/", VCQI_RIHC_DATASET)
      if(!file.exists(RIHCfile)){
        errormsgs <- c(errormsgs,
                       paste0("The file defined by global macros VCQI_DATA_FOLDER/VCQI_RIHC_DATASET ", RIHCfile, " does not exist"))
        exitflag <- 1
        vcqi_log_comment(VCP, 1, "Error", paste0("RI dataset: ", RIHCfile, " does not exist"))
      } else {
        #read the RIHC dataset
        dat <- vcqi_read(RIHCfile)
        if(is.data.frame(dat) == FALSE){
          errormsgs <- c(errormsgs,
                         paste0("The file defined by global macros VCQI_DATA_FOLDER/VCQI_RIHC_DATASET ", RIHCfile, " is not in valid format"))
          exitflag <- 1
          vcqi_log_comment(VCP, 1, "Error", paste0("RI dataset: ", RIHCfile, " is not in valid format"))
        } else {
          dlist <- NULL
          for (v in seq_along(RI_DOSE_LIST)){
            dlist <- c(dlist,paste0(RI_DOSE_LIST[v],"_date_register_d"),paste0(RI_DOSE_LIST[v],"_date_register_m"),paste0(RI_DOSE_LIST[v],"_date_register_y"),
                       paste0(RI_DOSE_LIST[v],"_tick_register"))
          }

          varlist <- c("RIHC01", "RIHC03", "RIHC14", "RIHC15", dlist)
          for (v in seq_along(varlist)){
            if (varlist[v] %in% names(dat)){
              if (varlist[v] != "RIHC14"){
                variable <- get(varlist[v],dat)
                if(all(!class(variable) %in% c("numeric", "double"))){
                  errormsgs <- c(errormsgs,
                                 paste0(varlist[v], " needs to be a numeric variable in RIHC dataset."))
                  exitflag <- 1
                  vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " needs to be a numeric variable in RIHC dataset."))
                }
              }

              if (varlist[v] %in% c("RIHC01", "RIHC03", "RIHC14", "RIHC15")){
                variable2 <- get(varlist[v],dat)
                if ("character" %in% class(variable2)){
                  if(any(is.na(variable2) | variable2 == "")){
                    errormsgs <- c(errormsgs,
                                   paste0(varlist[v], " cannot have a missing value in the RIHC dataset."))
                    exitflag <- 1
                    vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " cannot have a missing value in the RIHC dataset."))
                  }
                } else if(any(is.na(variable2))){
                  errormsgs <- c(errormsgs,
                                 paste0(varlist[v], " cannot have a missing value in the RIHC dataset."))
                  exitflag <- 1
                  vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " cannot have a missing value in the RIHC dataset."))
                }

              }

            } else{
              errormsgs <- c(errormsgs,
                             paste0("Variable ", varlist[v], " cannot have a missing value in the RIHC dataset.")) ###this is not correct
              exitflag <- 1
              vcqi_log_comment(VCP, 1, "Error", paste0(varlist[v], " cannot have a missing value in the RIHC dataset."))
            }
          }
        }

      }
    }
  }

  # Default is to NOT calculate report on data quality; user can turn it on
  if (!vcqi_object_exists("VCQI_REPORT_DATA_QUALITY")){
    vcqi_global(VCQI_REPORT_DATA_QUALITY,0)
  }

  #If the user specifies an MOV_OUTPUT_DOSE_LIST, check to be sure all the doses in that list are also in RI_DOSE_LIST

  if(exists("MOV_OUTPUT_DOSE_LIST")){
    if (length(MOV_OUTPUT_DOSE_LIST) > 0 &
        !is.null(MOV_OUTPUT_DOSE_LIST)) {
      assign("MOV_OUTPUT_DOSE_LIST", str_to_lower(MOV_OUTPUT_DOSE_LIST), envir = .GlobalEnv)
      vcqi_log_global(MOV_OUTPUT_DOSE_LIST)
      ndoses_not_found <- 0
      for (w in seq_along(MOV_OUTPUT_DOSE_LIST)) {
        dose_found <- 0
        if(MOV_OUTPUT_DOSE_LIST[w] %in% RI_DOSE_LIST){
          dose_found <- 1
        }
        if (dose_found == 0) {
          ndoses_not_found = ndoses_not_found + 1 #double check
          errormsgs <- c(
            errormsgs,
            paste0(
              "The global macro MOV_OUTPUT_DOSE_LIST includes the string ",
              MOV_OUTPUT_DOSE_LIST[w],
              ", which does not appear in the global macro RI_DOSE_LIST; VCQI will remove it from MOV_OUTPUT_DOSE_LIST."
            )
          )
          vcqi_log_comment(
            VCP,
            2,
            "Error",
            paste0(
              "The global macro MOV_OUTPUT_DOSE_LIST includes the string ",
              MOV_OUTPUT_DOSE_LIST[w],
              ", which does not appear in the global macro RI_DOSE_LIST; VCQI will remove it from MOV_OUTPUT_DOSE_LIST."
            )
          )
          MOV_OUTPUT_DOSE_LIST <- MOV_OUTPUT_DOSE_LIST[-w]
        }
        if(ndoses_not_found > 0){vcqi_log_global(RI_DOSE_LIST)}
      }

    } else {
      MOV_OUTPUT_DOSE_LIST <- RI_DOSE_LIST_MINUS_VISIT
      assign("MOV_OUTPUT_DOSE_LIST", str_to_lower(MOV_OUTPUT_DOSE_LIST), envir = .GlobalEnv)
      vcqi_log_global(MOV_OUTPUT_DOSE_LIST)
    }

  } else {

    MOV_OUTPUT_DOSE_LIST <- RI_DOSE_LIST_MINUS_VISIT
    assign("MOV_OUTPUT_DOSE_LIST", str_to_lower(MOV_OUTPUT_DOSE_LIST), envir = .GlobalEnv)
    vcqi_log_global(MOV_OUTPUT_DOSE_LIST)
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
