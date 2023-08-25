#' Check global macros for RI_QUAL_09
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr

# RI_QUAL_09_00GC R version 1.02 - Biostat Global Consulting - 2022-10-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-29  1.00      Mia Yu          Original R version
# 2022-10-13  1.01      Mia Yu          Package version
# 2022-10-18  1.02      Caitlin Clary   Added vcqi_halt_immediately call
# *******************************************************************************

RI_QUAL_09_00GC <- function(VCP = "RI_QUAL_09_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  vcqi_log_global(RI_QUAL_09_VALID_OR_CRUDE)
  vcqi_log_global(MOV_OUTPUT_DOSE_LIST)

  if (!(str_to_upper(RI_QUAL_09_VALID_OR_CRUDE) %in% c("VALID","CRUDE"))) {
    errormsgs <- c(errormsgs,
                   paste0("RI_QUAL_09_VALID_OR_CRUDE must be either VALID or CRUDE. The current value is ",
                          RI_QUAL_09_VALID_OR_CRUDE))
    vcqi_log_comment(VCP, 1, "Error",
                     paste0("RI_QUAL_09_VALID_OR_CRUDE must be either VALID or CRUDE. The current value is ",
                            RI_QUAL_09_VALID_OR_CRUDE))
    exitflag <- 1

  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

