#' Check HW analysis-related globals, datasets and variables
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
#' check_HW_analysis_metadata()

# check_HW_analysis_metadata R version 1.00 - Biostat Global Consulting - 2023-08-10
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-10  1.00      Mia Yu          Original R package version
# *******************************************************************************


check_HW_analysis_metadata <- function(VCP = "check_HW_analysis_metadata"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Check the generic analysis-related globals
  check_analysis_metadata_MV()
  # Check the CM dataset
  check_miss_VCQI_CM_metadata()

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  # Check that HW dataset was specified
  if (!vcqi_object_exists("HW_SURVEY_DATASET")){
    errormsgs <- c(errormsgs,
                   "Please set HW_SURVEY_DATASET.")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error",
                     "Please set HW_SURVEY_DATASET.")
  } else {
    # Check that HW dataset exists
    vcqi_log_global(VCQI_DATA_FOLDER)
    vcqi_log_global(HW_SURVEY_DATASET)

    if (!file.exists(paste0(VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET))){
      errormsgs <- c(errormsgs,paste0("The file defined by global macros VCQI_DATA_FOLDER/HW_SURVEY_DATASET (",
                                      VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET, ") does not exist"))
      vcqi_log_comment(VCP,1,"Error", paste0("RI dataset: ", VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET, " does not exist"))
      exitflag <- 1
    } else {
      # Check that HW variables used across all DESC indicators are present
      # and have the correct variable type
       dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET))

       if(is.data.frame(dat) == FALSE){
         errormsgs <- c(errormsgs,paste0("The file defined by global macros", VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET," is not in valid format"))
         exitflag <- 1
         vcqi_log_comment(VCP, 1, "Error",
                          paste0("The file defined by global macros", VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET," is not in valid format"))
       } else {

         dlist <- NULL

         var1 <- c("04AF", "04AG", "04AH", "05AA", "05AB", "05AC", "05AD", "05AH","05AI",
                   "06AA", "06AB", "06AC", "06AE", "06AF", "06AG", "06AH", "06AI", "06AJ")
         for (v in seq_along(var1)){
           dlist <- c(dlist,paste0("HW",var1[v]))
         } #end of var1 v loop

         var2 <- c("A", "B", "C", "D", "E", "F", "G", "H")
         for(i in seq_along(var2)){
           dlist <- c(dlist,paste0("ID02A",var2[i]))
         } #end of var2 i loop

         var3 <- c("HW01AA", "HW04AA_1", "HW04AA_2", "HW04AA_3", "HW04AA_4", "HW04AA_5",
                   "HW04AB_1", "HW04AB_2", "HW04AB_3", "HW04AB_4", "HW04AB_5", "HW04AC_1",
                   "HW04AC_2", "HW04AC_3", "HW04AC_4", "HW04AC_5", "HW04AD_1", "HW04AD_2",
                   "HW04AD_3", "HW04AD_4", "HW04AD_5", "HW04AE_1", "HW04AE_2", "HW04AE_3",
                   "HW04AE_4", "HW04AE_5", "HW04AI_1", "HW04AI_2", dlist, "HW04AJ_1",
                   "HW04AJ_2", "HW04AJ_3", "HW04AJ_4", "HW04AJ_5", "HW04AJ_6", "HW04AJ_7",
                   "HW06AD_1", "HW06AD_2", "HW06AD_3", "HW06AD_4", "HW06AD_5", "HW06AD_6",
                   "HW06AK_1", "HW06AK_2", "HW06AK_3", "HW06AK_4",	"ID02AIid", "ID02AIname",
                   "ID02AJm", "ID02AJd", "ID02AJy")

         for (v in seq_along(var3)){
           if (var3[v] %in% names(dat)){

             if (var3[v] %in% c("ID02AB", "ID02AD", "ID02AF", "ID02AH", "ID02AIid")){
               var <- get(var3[v],dat)
               var <- haven::zap_label(var)
               var <- haven::zap_labels(var)
               if ("character" %in% class(var)){
                 if (any(is.na(var)| var == "")){
                   errormsgs <- c(errormsgs,paste0(var3[v], "cannot have a missing value in the HW dataset."))
                   vcqi_log_comment(VCP,1,"Error", paste0(var3[v], "cannot have a missing value in the HW dataset."))
                   exitflag <- 1
                 }
               } else if (any(is.na(var))){
                 errormsgs <- c(errormsgs,paste0(var3[v], "cannot have a missing value in the HW dataset."))
                 vcqi_log_comment(VCP,1,"Error", paste0(var3[v], "cannot have a missing value in the HW dataset."))
                 exitflag <- 1
               }
             }

           } else {
             errormsgs <- c(errormsgs,paste0("Variable ", var3[v],
                                             " does not exist in HW dataset and is required to run MISS-VCQI."))
             vcqi_log_comment(VCP,1,"Error", paste0("Variable ", var3[v],
                                                    " does not exist in HW dataset and is required to run MISS-VCQI."))
             exitflag <- 1
           }
         } #end of var3 v loop
       }
    }
  }

  if (exitflag == 1) {
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(halt_message = errormsgs)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
