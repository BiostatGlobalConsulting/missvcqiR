#' Check global macros for RI_COVG_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr

# RI_COVG_03_00GC R version 1.01 - Biostat Global Consulting - 2023-07-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-13  1.00      Mia Yu          Original R package version
# 2023-07-21  1.01      Mia Yu          Keep a copy of RI_DOSES_TO_BE_FULLY_VACCINATED
#                                       and match the Stata version
# *******************************************************************************

RI_COVG_03_00GC <- function(VCP = "RI_COVG_03_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vcqi_log_global(RI_DOSES_TO_BE_FULLY_VACCINATED)
  vcqi_log_global(RI_DOSE_LIST)
  vcqi_log_global(RI_SINGLE_DOSE_LIST)
  #placeholder for multi-dose

  exitflag <- 0
  errormsgs <- NULL

  #Confirm global RI_DOSES_TO_BE_FULLY_VACCINATED is defined
  if (!vcqi_object_exists("RI_DOSES_TO_BE_FULLY_VACCINATED")){
    errormsgs <- c(errormsgs,
                   "RI_DOSES_TO_BE_FULLY_VACCINATED must be defined by the user in order to calculate RI_COVG_03 or RI_COVG_04. It is currently not defined.")
    vcqi_log_comment(VCP, 1, "Error",
                     "RI_DOSES_TO_BE_FULLY_VACCINATED must be defined by the user in order to calculate RI_COVG_03 or RI_COVG_04. It is currently not defined.")
    exitflag <- 1
  }

  #Check to see that each dose listed in RI_DOSES_TO_BE_FULLY_VACCINATED is included in the analysis
  if (vcqi_object_exists("RI_DOSES_TO_BE_FULLY_VACCINATED")){

    # Do vcqi_global in seperate steps since the name depends on another global
    assign(paste0("RI_DOSES_TBFV_AC_",ANALYSIS_COUNTER), RI_DOSES_TO_BE_FULLY_VACCINATED, envir = .GlobalEnv)
    vcqi_log_comment(VCP, 3, "Global", paste0("Global value RI_DOSES_TBFV_AC_",ANALYSIS_COUNTER, " is ", RI_DOSES_TO_BE_FULLY_VACCINATED))

    fullupper <- str_to_upper(RI_DOSES_TO_BE_FULLY_VACCINATED)
    riupper <- str_to_upper(RI_DOSE_LIST)

    for (d in seq_along(fullupper)){

      if (!(fullupper[d] %in% riupper)){
        errormsgs <- c(errormsgs,
                       paste0("RI_DOSES_TO_BE_FULLY_VACCINATED includes ", fullupper[d], ", but ", fullupper[d],
                            " does not appear in the RI_DOSE_LIST which is set using dose global values. Either add ",
                            fullupper[d], " to one of those global values or remove it from RI_DOSES_TO_BE_FULLY_VACCINATED."))
        vcqi_log_comment(VCP, 1, "Error",
                         paste0("RI_DOSES_TO_BE_FULLY_VACCINATED includes ", fullupper[d], ", but ", fullupper[d],
                                " does not appear in the RI_DOSE_LIST which is set using dose global values. Either add ",
                                fullupper[d], " to one of those global values or remove it from RI_DOSES_TO_BE_FULLY_VACCINATED."))
        exitflag <- 1
      }

    } #end of d fullupper loop
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
