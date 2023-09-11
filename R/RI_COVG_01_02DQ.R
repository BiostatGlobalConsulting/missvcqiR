#' Check data quality for RI_COVG_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))

# RI_COVG_01_02DQ R version 1.02 - Biostat Global Consulting - 2022-10-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-03  1.00      Mia Yu          Original R version
# 2022-10-05  1.01      Mia Yu          Package version
# 2022-10-18  1.02      Caitlin Clary   Added vcqi_halt_immediately call
# *******************************************************************************

RI_COVG_01_02DQ <- function(VCP = "RI_COVG_01_02DQ"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_", ANALYSIS_COUNTER, ".rds"))

  exitflag <- 0
  errormsgs <- NULL

  # This program assumes dates and tick marks have been cleaned
  # Only validates responses to bcg_scar_history
  # - Interviewer sees evidence of scar on child

  if ("bcg" %in% RI_DOSE_LIST){
    scar <- dat$bcg_scar_history
    if(!(all(scar %in% c(1, 2, 3, NA)))){
      errormsgs <- c(errormsgs,
                     "RI_COVG_01: bcg_scar_history contains values that are not the expected values of 1, 2, 3 or missing")
      exitflag <- 1
      vcqi_log_comment(VCP, 1, "Error",
                       "RI_COVG_01: bcg_scar_history contains values that are not the expected values of 1, 2, 3 or missing")

      scar <- dat %>% count(bcg_scar_history)
      print(scar)
    }
  }

  for (d in seq_along(RI_DOSE_LIST)){
    var <- get(paste0(RI_DOSE_LIST[d],"_history"), dat)
    groupvar <- rlang::sym(paste0(RI_DOSE_LIST[d],"_history"))
    if(!(all(var %in% c(1,2,99,NA)))){
      errormsgs <- c(errormsgs,
                     paste0(RI_DOSE_LIST[d],"_history contains values that are not the expected values of 1,2,99 or missing"))
      exitflag <- 1
      vcqi_log_comment(VCP, 1, "Error",
                       paste0("RI_COVG_01: ",RI_DOSE_LIST[d],"_history contains values that are not the expected values of 1,2,99 or missing"))

      summary <- dat %>% count(!!groupvar)
      print(summary)
    }
  } #end of dose loop

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

