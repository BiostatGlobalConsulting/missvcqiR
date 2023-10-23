#' Shift dose dates in specified dose series
#'
#' @param NUM_DOSE_SHIFTS Number of dose series for which dose shifting will be done
#' @param dat Dataset with vaccination date variables
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset returned to cleanup_RI_dates_and_ticks
#'
#' @import stringr

# shift_RI_dates version R version 1.05 - Biostat Global Consulting - 2022-10-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-11  1.00      Mia Yu          Original R version
# 2022-07-20  1.01      Mia Yu          Fixed bugs when shiftto & shiftfrom not
#                                       defined and the changed program names to
#                                       avoid R getting confused
# 2022-09-23  1.02      Mia Yu          Update to make sure shifto exists
# 2022-10-05  1.03      Mia Yu          Package version
# 2022-10-11  1.04      Mia Yu          Update the usage of vcqi_object_exists
# 2022-10-24  1.05      Mia Yu          Add vcqi_halt_immediately call
# *******************************************************************************

shift_RI_dates <- function(NUM_DOSE_SHIFTS, dat = NA, VCP = "shift_RI_dates"){
  exitflag <- 0
  errormsgs <- NULL

  for (i in 1:NUM_DOSE_SHIFTS){
    if(vcqi_object_exists(paste0("SHIFTTO_",i))){
      shiftto <- get(paste0("SHIFTTO_",i),envir = .GlobalEnv)
      assign("shiftto",shiftto,envir = .GlobalEnv)
    } else {
      shiftto <- NULL
      vcqi_global(VCQI_ERROR, 1)
      errormsgs <- paste0("SHIFTTO_",i," is required for dose shifting. Please define SHIFTTO_",i)
      errormsgs <- c(errormsgs,"EXIT program")
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    }

    if(vcqi_object_exists(paste0("SHIFTFROM_",i))){
      shiftfrom <- get(paste0("SHIFTFROM_",i),envir = .GlobalEnv)
      assign("shiftfrom",shiftfrom,envir = .GlobalEnv)
    } else{
      assign("shiftfrom",NULL,envir = .GlobalEnv)
    }

    if(vcqi_object_exists(paste0("SHIFTWITHIN_",i))){
      shiftwithin <- get(paste0("SHIFTWITHIN_",i),envir = .GlobalEnv)
      assign("shiftwithin",shiftwithin,envir = .GlobalEnv)
    } else{
      assign("shiftwithin",0,envir = .GlobalEnv)
    }

    if(vcqi_object_exists(paste0("DROPDUP_",i))){
      dropdup <- get(paste0("DROPDUP_",i),envir = .GlobalEnv)
      assign("dropdup",dropdup,envir = .GlobalEnv)
    } else{
      assign("dropdup",0,envir = .GlobalEnv)
    }
    SHIFTI <- i
    assign("SHIFTI",SHIFTI,envir = .GlobalEnv)

    # ********************************************************************************
    # ********************************************************************************
    # ****						dose_shift_program								****
    # ********************************************************************************
    # ********************************************************************************

    vcqi_log_comment(VCP, 5, "Flow", "Starting")

    if(RI_RECORDS_NOT_SOUGHT == 1){
      resource <- c("card")
    }

    if(RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      resource <- c("card","register")
    }

    #Reset SHIFTTO and SHIFTFROM to be lowercase
    #also check to make sure doses are in RI_DOSE_LIST
    if (!is.null(shiftto)){
      shiftto <- str_to_lower(shiftto)
      for (d in seq_along(shiftto)){
        if (!(shiftto[d] %in% RI_DOSE_LIST)){
          exitflag <- 1
          errormsgs <- c(errormsgs, paste0("Dose ", shiftto[d]," from input SHIFTTO_",SHIFTI," must be part of the RI_DOSE_LIST provided."))
          vcqi_log_comment(VCP, 1, "Error", paste0("Dose ",shiftto[d]," must be part of the RI_DOSE_LIST provided."))
        }
      }
    }

    if (!is.null(shiftfrom)){
      shiftfrom <- str_to_lower(shiftfrom)
      for (d in seq_along(shiftfrom)){
        if (!(shiftfrom[d] %in% RI_DOSE_LIST)){
          exitflag <- 1
          errormsgs <- c(errormsgs, paste0("Dose ", shiftfrom[d]," from input SHIFTFROM_",SHIFTI," must be part of the RI_DOSE_LIST provided."))
          vcqi_log_comment(VCP, 1, "Error", paste0("Dose ",shiftfrom[d]," must be part of the RI_DOSE_LIST provided."))
        }
      }
    }

    #Check that some combination of options will run
    if (length(shiftto) == 1 & shiftwithin == 1){
      warning("WARNING: Unable to SHIFTWITHIN since there is only one dose provided in SHIFTTO global.")
      vcqi_log_comment(VCP, 2, "Warning", "Unable to SHIFTWITHIN since there is only one dose provided in SHIFTTO global.")
    }
    if(is.null(shiftfrom) & dropdup == 0 & (length(shiftto) == 1 | shiftwithin == 0)){
      exitflag <- 1
      errormsgs <- c(errormsgs, "Input options will result in no changes to the dataset.")
      vcqi_log_comment(VCP, 1, "Error", "Input options will result in no changes to the dataset.")
    }

    if(exitflag == 1){
      vcqi_global(VCQI_ERROR, 1)
      errormsgs <- c(errormsgs,"EXIT program")
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    } else{
      print("Begin shifting dose dates and ticks...")

      # Complete for each data source
      for (r in seq_along(resource)){
        result <- preprocess_cleanup(s = resource[r], dat = dat)
        dat <- result$dataset
        vlist <- result$varlist

        # Shift dates within the `shiftto' list
        if (shiftwithin == 1){
          result <- shiftwithinscript(input = shiftto, shifttype = "shiftto",
                                      s = resource[r], vlist = vlist, dat = dat)
          dat <- result$dataset
          vlist <- result$varlist
        }

        # Shift dates from SHIFTFROM to SHIFTTO list
        if (!is.null(shiftfrom)){
          result <- shiftfromscript(s = resource[r], vlist = vlist, dat = dat)
          dat <- result$dataset
          vlist <- result$varlist
        }

        # Now shift within the SHIFTFROM
        if ((shiftwithin == 1 & !is.null(shiftfrom))){
          result <- shiftwithinscript(input = shiftfrom, shifttype = "shiftfrom",
                                      s = resource[r], vlist = vlist, dat = dat)
          dat <- result$dataset
          vlist <- result$varlist
        }

        # Only keep the original variables and newly created variables that show
        # any shifts
        dat <- select(dat, all_of(vlist))
      }

    }

    vcqi_log_comment(VCP, 5, "Flow", "Exiting")
    rm(list = c("shiftto","shiftfrom","shiftwithin","dropdup","SHIFTI"),envir = .GlobalEnv)%>% suppressWarnings()
  }

  return(dat)
}


