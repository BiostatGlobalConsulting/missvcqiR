#' Check existence, format, and contents of the cluster metadata (CM) dataset
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors and/or warnings if conditions not met
#'
#' @import dplyr
#'

# check_miss_VCQI_CM_metadata R version 1.00 - Biostat Global Consulting - 2023-08-28
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-28  1.00      Mia Yu          CM check program that mirrors VCQI's check_VCQI_CM_metadata
# *******************************************************************************

check_miss_VCQI_CM_metadata <- function(VCP = "check_miss_VCQI_CM_metadata"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  if(vcqi_object_exists("VCQI_CM_DATASET")) {
    if (vcqi_object_exists("VCQI_DATA_FOLDER")){
      if (file.exists(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET))){
        # Read CM dataset
        CM <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET))

        # Check if the file is in valid format
        if (is.data.frame(CM) == FALSE){
          errormsgs <- c(errormsgs,paste0("The file defined by global macros VCQI_DATA_FOLDER/VCQI_CM_DATASET ", paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET)," is not in valid format"))
          exitflag <- 1
          vcqi_log_comment(VCP, 1, "Error",
                           paste0("CM dataset: ", VCQI_DATA_FOLDER, "/",
                                  VCQI_CM_DATASET, " is not in valid format"))
        } else{

          # Begin checks

          cm_vars <- c("ID02AIid","ID02AIname","province_id","urban_cluster")
          for(i in seq_along(cm_vars)) {
            if(cm_vars[i] %in% names(CM)) {
              # Check variable format
              if (cm_vars[i] %in% "ID02AIname") {
                if(is.character(get(cm_vars[i], CM)) == FALSE) {
                  warningmsgs <- c(warningmsgs,
                                   paste0(cm_vars[i]," should be a character variable in the CM dataset."))
                  vcqi_log_comment(VCP, 2, "Warning",
                                   paste0(cm_vars[i]," should be a character variable in the CM dataset."))
                }
              } else {
                if(is.numeric(get(cm_vars[i], CM)) == FALSE) {
                  warningmsgs <- c(warningmsgs,
                                   paste0(cm_vars[i]," should be a numeric variable in the CM dataset."))
                  vcqi_log_comment(VCP, 2, "Warning",
                                   paste0(cm_vars[i]," should be a numeric variable in the CM dataset."))
                }
              }

              # Check for missing values
              if (cm_vars[i] %in% c("province_id", "urban_cluster")) {
                if (sum(is.na(get(cm_vars[i], CM))) > 0) {
                  warningmsgs <-
                    c(warningmsgs,paste0(cm_vars[i]," should not have a missing value in the CM dataset."))
                  vcqi_log_comment(VCP,2,"Warning",paste0(cm_vars[i]," should not have a missing value in the CM dataset."))
                }
              }

            } else {
              # Error if variable is not present in CM

              errormsgs <-
                c(errormsgs,paste0("Variable ",cm_vars[i]," does not exist in the CM dataset and is required to run VCQI."))
              vcqi_log_comment(VCP,1,"Error",paste0("Variable ",cm_vars[i]," does not exist in the CM dataset and is required to run VCQI."))

              exitflag <- 1
            }
          } # end variable check loop
        }
      } else {
        errormsgs <- c(errormsgs, paste0("The file defined by ", VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET, " does not exist."))
        vcqi_log_comment(VCP, 1, "Error", paste0("The file defined by ", VCQI_DATA_FOLDER, "/",VCQI_CM_DATASET, " does not exist."))
        exitflag <- 1
      } #error message for file not exist
    }
  } else {
    errormsgs <-
      c(errormsgs,
        "Please set VCQI_CM_DATASET.")
    vcqi_log_comment(VCP,1,"Error",
                     "Please set VCQI_CM_DATASET.")
    exitflag <- 1
  } #error messages for check VCQI_CM_DATASET


  if(!is.null(warningmsgs)){
    warning(warningmsgs)
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}


