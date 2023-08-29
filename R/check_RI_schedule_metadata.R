#' Check dose list(s) and vaccination schedule in a VCQI RI run
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#'
#' @return Errors and/or warnings if conditions not met
#'
#' @export
#'
#' @examples
#' check_RI_schedule_metadata()

# check_RI_schedule_metadata R version 1.03 - Biostat Global Consulting - 2022-10-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-23  1.00      Caitlin Clary   Original R version
# 2022-06-29  1.01      Caitlin Clary   Use vcqi_object_exists() for dose list checks
# 2022-10-05  1.02      Mia Yu          Package version and add envir = .GlobalEnv to get()
# 2022-10-18  1.03      Caitlin Clary   Added vcqi_halt_immediately call
# *******************************************************************************


check_RI_schedule_metadata <- function(
    VCP = "check_RI_schedule_metadata"
){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  # Metadata checks for RI_SINGLE_DOSE_LIST ----

  if(vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
    for(d in 1:length(RI_SINGLE_DOSE_LIST)){

      ds <- RI_SINGLE_DOSE_LIST[d]

      if(nchar(ds) > 6) {
        errormsgs <- c(errormsgs,
                       paste0("Dose name ", ds, " specified in RI_SINGLE_DOSE_LIST is too long. It must be no more than 6 characters. Please reference the VCQI User's Guide Section 3.1 (Vaccination Schedule Metadata: A Note on Dose Names) for more information about the dose naming convention")
        )

        exitflag <- 1

        vcqi_log_comment(VCP, 1, "Error", paste0("RI_SINGLE_DOSE_LIST name ", ds, " is greater than the 6 character limit"))
      } else {
        vcqi_log_comment(VCP, 3, "Comment", paste0("RI_SINGLE_DOSE_LIST name ", ds, " is within the 6 character limit"))
      }

      if(exists(paste0(str_to_lower(ds), "_min_age_days")) == FALSE){
        errormsgs <- c(errormsgs,
                       paste0("check_RI_schedule_metadata: RI_SINGLE_DOSE_LIST includes ", ds, " but the vaccination schedule does not include a value named ", str_to_lower(ds), "_min_age_days"))

        exitflag <- 1

        vcqi_log_comment(VCP, 1, "Error", paste0(str_to_lower(ds),"_min_age_days is not specified in the RI schedule"))

      } else {
        vcqi_log_comment(VCP, 3, "RI_Schedule", paste0(str_to_lower(ds),"_min_age_days is ", get(paste0(str_to_lower(ds),"_min_age_days"))))

      }

    } # End loop: single dose list

  }

  # Metadata checks for RI_MULTI_<X>_DOSE_LIST(s) ----

  multi_dose_list <- lapply(2:9, function(x) if(vcqi_object_exists(paste0("RI_MULTI_", x, "_DOSE_LIST"))){
    data.frame(dosename = get(paste0("RI_MULTI_", x, "_DOSE_LIST"))) %>%
      mutate(n_doses = rep(x, n()))
  }) %>% do.call(rbind, .)

  if(!is.null(multi_dose_list)){
    for(i in 1:nrow(multi_dose_list)){

      ds <- multi_dose_list$dosename[i]
      nd <- multi_dose_list$n_doses[i]

      # Check 1: dose name within character limit
      if(nchar(multi_dose_list$dosename[i]) > 5){

        errormsgs <- c(errormsgs,
                       paste0("Dose name ", ds, " specified in RI_MULTI_", nd, "_DOSE_LIST is too long. It must be no more than 5 characters. Please reference the VCQI User's Guide Section 3.1 (Vaccination Schedule Metadata: A Note on Dose Names) for more information about the dose naming convention")
        )

        exitflag <- 1

        vcqi_log_comment(VCP, 1, "Error", paste0("RI_MULTI_", nd, "_DOSE_LIST name ", ds, " is greater than the 5 character limit"))
      } else {
        vcqi_log_comment(VCP, 3, "Comment", paste0("RI_MULTI_", nd, "_DOSE_LIST name ", ds, " is within the 5 character limit"))
      }

      # Check 2: <dose>n_min_age_days exists
      for(j in nd:1){
        if(exists(paste0(str_to_lower(ds), j, "_min_age_days")) == FALSE){

          errormsgs <- c(errormsgs,
                         paste0("check_RI_schedule_metadata: the dose list includes ", ds, j, " but the vaccination schedule does not include a value named ", str_to_lower(ds), j, "_min_age_days"))

          exitflag <- 1

          vcqi_log_comment(VCP, 1, "Error", paste0(str_to_lower(ds), j, "_min_age_days is not specified in the RI schedule"))
        } else {
          vcqi_log_comment(VCP, 3, "RI_Schedule", paste0(str_to_lower(ds), j, "_min_age_days is ", get(paste0(str_to_lower(ds), j, "_min_age_days"))))
        }
      } # End check 2 loop

      # Check 3: <dose>n_min_interval_days exists
      for(j in nd:2){
        if(exists(paste0(str_to_lower(ds), j, "_min_interval_days")) == FALSE){

          errormsgs <- c(errormsgs,
                         paste0("check_RI_schedule_metadata: the dose list includes ", ds, j, " but the vaccination schedule does not include a value named ", str_to_lower(ds), j, "_min_interval_days"))

          exitflag <- 1

          vcqi_log_comment(VCP, 1, "Error", paste0(str_to_lower(ds), j, "_min_interval_days is not specified in the RI schedule"))
        } else {
          vcqi_log_comment(VCP, 3, "RI_Schedule", paste0(str_to_lower(ds), j, "_min_interval_days is ", get(paste0(str_to_lower(ds), j, "_min_interval_days"))))
        }

      } # End check 3 loop

      # Check 4: schedule due dates equal previous dose due + interval
      check4 <- 1
      for(j in 1:nd){if(exists(paste0(str_to_lower(ds), j, "_min_age_days")) == FALSE){check4 <- 0}}
      for(j in 2:nd){if(exists(paste0(str_to_lower(ds), j, "_min_interval_days")) == FALSE){check4 <- 0}}

      # Perform checks if all age and interval values are present:
      if(check4 == 1){
        for(k in 2:nd){
          intsum <- lapply(2:k, function(x) get(paste0(str_to_lower(ds), x, "_min_interval_days"),envir = .GlobalEnv)) %>% unlist() %>% sum()
          intlist <- lapply(2:k, function(x) paste0(str_to_lower(ds), x, "_min_interval_days")) %>% paste(., collapse = " + ")

          if(get(paste0(str_to_lower(ds), "1_min_age_days"),envir = .GlobalEnv) + intsum != get(paste0(str_to_lower(ds), k, "_min_age_days"),envir = .GlobalEnv)){

            vcqi_log_comment(VCP, 2, "Warning", paste0(str_to_lower(ds), k, "_min_age_days is not equal to ", str_to_lower(ds), "1_min_age_days + ", intlist))
            vcqi_log_comment(VCP, 2, "Warning", "In most African and Asian countries, this might indicate a mistake with the vx schedule values in the control program.")

          }
        } # end k loop
      }

    } # End loop: multi dose list(s)

  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}


