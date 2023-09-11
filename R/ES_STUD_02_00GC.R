#' Check global macros for ES_STUD_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr

# ES_STUD_02_00GC R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_02_00GC <- function(VCP = "ES_STUD_02_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  #Confirm global ES_STUD_02_VALID_OR_CRUDE is defined and has the appropriate values
  if (!(str_to_upper(ES_STUD_02_VALID_OR_CRUDE) %in% c("VALID","CRUDE"))) {
    errormsgs <- c(errormsgs,
                   paste0("ES_STUD_02_VALID_OR_CRUDE must be either VALID or CRUDE. The current value is ",
                          ES_STUD_02_VALID_OR_CRUDE))
    vcqi_log_comment(VCP, 1, "Error",
                     paste0("ES_STUD_02_VALID_OR_CRUDE must be either VALID or CRUDE. The current value is ",
                            ES_STUD_02_VALID_OR_CRUDE))
    exitflag <- 1

  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
