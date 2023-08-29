#' Check the survey-related global values in a VCQI RI run
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors and/or warnings if conditions not met
#'
#' @export
#'
#' @examples
#' check_RI_survey_metadata()


# check_RI_survey_metadata R version 1.05 - Biostat Global Consulting - 2022-10-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-27  1.00      Mia Yu          Original R version
# 2022-07-06  1.01      Caitlin Clary   Updated vcqi_log_global/vcqi_global calls
# 2022-07-19  1.02      Mia Yu          Updated vcqi_object_exists
#                                       Adding a part to check that earliest and
#                                       latest survey dates make sense
# 2022-07-28  1.03      Mia Yu          Updated vcqi_mdy to replace mdy function
# 2022-10-04  1.04      Mia Yu          Package version
# 2022-10-18  1.05      Caitlin Clary   Added vcqi_halt_immediately call
# *******************************************************************************


check_RI_survey_metadata <- function(VCP = "check_RI_survey_metadata"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  vcqi_log_global(RI_DOSE_LIST)

  datelist <- c("EARLIEST_SVY_VACC_DATE_M", "EARLIEST_SVY_VACC_DATE_D", "EARLIEST_SVY_VACC_DATE_Y",
                "LATEST_SVY_VACC_DATE_M", "LATEST_SVY_VACC_DATE_D", "LATEST_SVY_VACC_DATE_Y")

  for (g in seq_along(datelist)){
    if (!vcqi_object_exists(datelist[g])){
      errormsgs <- c(errormsgs,
                     paste0("check_RI_survey_metadata: missing expected parameter ", datelist[g]))
      exitflag <- 1
      vcqi_log_comment(VCP, 1, "Error", paste0(datelist[g], " is not specified in the survey parameters"))
    }
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  earliest <- vcqi_mdy(EARLIEST_SVY_VACC_DATE_M,
                       EARLIEST_SVY_VACC_DATE_D,
                       EARLIEST_SVY_VACC_DATE_Y)
  latest <- vcqi_mdy(LATEST_SVY_VACC_DATE_M,
                     LATEST_SVY_VACC_DATE_D,
                     LATEST_SVY_VACC_DATE_Y)

  if(is.na(earliest)){
    errormsgs <- c(errormsgs,
                   "Earliest survey vaccination date dose not make sense")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error", "Earliest survey vaccination date dose not make sense")
  }

  if(is.na(latest)){
    errormsgs <- c(errormsgs,
                   "Latest survey vaccination date dose not make sense")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error", "Latest survey vaccination date dose not make sense")
  }

  if(!is.na(earliest) & !is.na(latest) & earliest > latest){
    errormsgs <- c(errormsgs,
                   "Earliest survey vaccination date should be before latest survey vaccination date.")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error", "Earliest survey vaccination date should be before latest survey vaccination date.")
  }

  all3here <- 1

  soughtlist <- c("RI_RECORDS_NOT_SOUGHT", "RI_RECORDS_SOUGHT_FOR_ALL", "RI_RECORDS_SOUGHT_IF_NO_CARD")
  for(g in seq_along(soughtlist)) {
    var <- soughtlist[g]
    if (!vcqi_object_exists(var)) {
      errormsgs <- c(errormsgs,
                     paste0("check_RI_survey_metadata: missing expected parameter ",var))
      exitflag <- 1
      all3here <- 0
      vcqi_log_comment(VCP,1,"Error",paste0(var, " is not specified in the survey parameters"))
    } else{
      sought_var <- get(var, envir = .GlobalEnv)
      #check that each is either 0 or 1
      #sought_var != 0 & sought_var != 1
      if (!(sought_var %in% c(0, 1))) {
        errormsgs <- c(errormsgs,
                       paste0(var, " should be 0 or 1"))
        exitflag <- 1
        vcqi_log_comment(VCP, 1, "Error", paste0(var, " should be 0 or 1"))
      }
    }
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  if(all3here == 1){
    if(sum(RI_RECORDS_NOT_SOUGHT, RI_RECORDS_SOUGHT_FOR_ALL, RI_RECORDS_SOUGHT_IF_NO_CARD) != 1){
      errormsgs <- c(errormsgs,
                     paste0("Problem in check_RI_survey_metadata: One of the following parameters should be 1 and the other two should be 0: RI_RECORDS_NOT_SOUGHT, RI_RECORDS_SOUGHT_FOR_ALL and RI_RECORDS_SOUGHT_IF_NO_CARD. Currently: ", RI_RECORDS_NOT_SOUGHT, ", " , RI_RECORDS_SOUGHT_FOR_ALL," and ", RI_RECORDS_SOUGHT_IF_NO_CARD, " respectively."))
      exitflag <- 1
      vcqi_log_comment(VCP, 1, "Error", "One and only one of the three RI_RECORDS globals should be set to 1")
    }
  }

  #Specify min and max age of survey eligibility if the user hasn't done so

  if(vcqi_object_exists("VCQI_RI_MIN_AGE_OF_ELIGIBILITY") == FALSE){
    vcqi_global(VCQI_RI_MIN_AGE_OF_ELIGIBILITY, 365)
  }

  if(vcqi_object_exists("VCQI_RI_MAX_AGE_OF_ELIGIBILITY") == FALSE){
    vcqi_global(VCQI_RI_MAX_AGE_OF_ELIGIBILITY, 731)
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
