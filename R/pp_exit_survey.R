#' This program will check to see which variables are needed for cleanup dates and ticks....
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @rawNamespace import(tools, except = makevars_user)
#'
#' @return A dataset and log entries; errors if conditions not met
#'
#' @export
#'
#' @examples
#' pp_exit_survey()

# pp_exit_survey R version 1.00 - Biostat Global Consulting - 2023-08-28
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-28  1.00      Mia Yu          Original R package version
# *******************************************************************************


pp_exit_survey <- function(VCP = "pp_exit_survey"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  assign("VCQI_RI_DATASET", ES_SURVEY_DATASET, envir = global_env())

  dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_RI_DATASET))
  saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(VCQI_RI_DATASET),"_miss_vcqi_ready.rds"))

  vcqi_global(VCQI_RI_DATASET, paste0(tools::file_path_sans_ext(VCQI_RI_DATASET),"_miss_vcqi_ready.rds"))

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  # Only keep if survey completed
  dat <- dat %>% filter(ES01AE %in% 1)

  # Confirm that all Exit Survey Specific globals are set

  var <- c("LEVEL_2_ID", "LEVEL_3_ID")
  for (v in seq_along(var)){
    if (!vcqi_object_exists(var[v])){
      errormsgs <- c(errormsgs,paste0("Global ",var[v], " is required to run miss-vcqi programs"))
      vcqi_log_comment(VCP,1,"Error", paste0("Global ",var[v], " is required to run miss-vcqi programs"))
      exitflag <- 1
    }
  } #end of var v loop

  # If there is a dose shift, check the required variables for each shift
  if (!vcqi_object_exists("NUM_DOSE_SHIFTS")){vcqi_global(NUM_DOSE_SHIFTS,0)}
  if (NUM_DOSE_SHIFTS >=  1) {
    for (i in 1:NUM_DOSE_SHIFTS){
      shiftlist <- c("SHIFTTO_", "SHIFTFROM_")
      for (v in seq_along(shiftlist)){
        if (!vcqi_object_exists(paste0(shiftlist[v],i))){
          errormsgs <- c(errormsgs,paste0("Global ",shiftlist[v],i, " is required to run miss-vcqi programs"))
          vcqi_log_comment(VCP,1,"Error", paste0("Global ",shiftlist[v],i, " is required to run miss-vcqi programs"))
          exitflag <- 1
        }
      } #end of shiftlist v loop
    } #end of NUM_DOSE_SHIFTS i loop
  }

  if (!vcqi_object_exists("RI_DOSE_LIST")){
    errormsgs <- c(errormsgs,
                   "Global RI_SINGLE_DOSE_LIST, RI_MULTI2_DOSE_LIST or RI_MULTI3_DOSE_LIST must be populated to run miss-vcqi programs")
    vcqi_log_comment(VCP,1,"Error",
                     "Global RI_SINGLE_DOSE_LIST, RI_MULTI2_DOSE_LIST or RI_MULTI3_DOSE_LIST must be populated to run miss-vcqi programs")
    exitflag <- 1
  }

  # Set these below globals that will always be these values when running miss-vcqi
  vcqi_global(RI_RECORDS_NOT_SOUGHT,    	  1)
  vcqi_global(RI_RECORDS_SOUGHT_FOR_ALL,    0)
  vcqi_global(RI_RECORDS_SOUGHT_IF_NO_CARD, 0)

  # Create dob variables
  source <- c("card", "history")
  sdate <- c("m", "d", "y")

  for (s in seq_along(source)){
    for (m in seq_along(sdate)){
      if (paste0("ES03AA",sdate[m]) %in% names(dat)){
        if (!paste0("dob_date_",source[s],"_",sdate[m]) %in% names(dat)){
          dat <- dat %>% mutate(!!paste0("dob_date_",source[s],"_",sdate[m]) := !!rlang::sym(paste0("ES03AA",sdate[m]) ))
        }
      }
    } #end of sdate m loop
  } #end of source s loop

  # Create RI09 variables survey date variables
  for (m in seq_along(sdate)){
    if (paste0("ID02AJ",sdate[m]) %in% names(dat)){
      if (!paste0("RI09_",sdate[m]) %in% names(dat)){
        dat <- dat %>% mutate(!!paste0("RI09_",sdate[m]) := !!rlang::sym(paste0("ID02AJ",sdate[m]) ))
      }
    }
  } #end of sdate m loop

  # Create variable for RI27 (saw the child's vx card) for quality checks
  if (!"RI27" %in% names(dat)){
    dat <- dat %>% mutate(RI27 = ES06AA) %>% mutate(RI27 = ifelse(RI27 %in% 3, NA, RI27)) #3 is not an option for RI27
  }

  # Create history variables
  for (v in seq_along(RI_DOSE_LIST)){
    if (!paste0(RI_DOSE_LIST[v],"_history") %in% names(dat)){
      dat <- dat %>% mutate(tempvar = NA)
      dat$tempvar <- as.numeric(dat$tempvar)
      dat$tempvar <- haven::labelled(dat$tempvar, label = paste0("Dose ", RI_DOSE_LIST[v]," received via history"))
      names(dat)[which(names(dat) == "tempvar")] <- paste0(RI_DOSE_LIST[v],"_history")
    }
    if (RI_DOSE_LIST[v] == "bcg" & (!"bcg_scar_history" %in% names(dat))){
      dat <- dat %>% mutate(bcg_scar_history = NA)
      dat$bcg_scar_history <- as.numeric(dat$bcg_scar_history)
      dat$bcg_scar_history <- haven::labelled(dat$bcg_scar_history, label = "Dose bcg received via scar")
    }
  } #end of RI_DOSE_LIST v loop

  # Create VCQI RI dataset identifying variable: RI01 RI03 RI11 RI12
  if (!"RI01" %in% names(dat)){
    dat <- dat %>% mutate(RI01 = ID02AB) #State to Stratum
  }
  if (!"RI03" %in% names(dat)){
    dat <- dat %>% mutate(RI03 = 1) #There is no cluster data...setting to 1
    dat$RI03 <- haven::labelled(dat$RI03, label = "Created to run through MOV tool")
  }

  if (!"RI11" %in% names(dat)){
    dat <- dat %>% mutate(RI11 = ID02AIid) #Facility id to household id
  }
  if (!"RI12" %in% names(dat)){
    dat <- dat %>% mutate(RI12 = ES01AA) #questionaire number to childid
  }

  for (m in seq_along(sdate)){
    # Check to see if a visit date variable has been created
    if (!paste0("visit_date_card_",sdate[m]) %in% names(dat)){
      dat <- dat %>% mutate(tempvar = !!rlang::sym(paste0("ID02AJ",sdate[m])))
      dat$tempvar <- haven::labelled(dat$tempvar, label = "Visit date variable created for MOV purposes")
      names(dat)[which(names(dat) == "tempvar")] <- paste0("visit_date_card_",sdate[m])
    } else if (paste0("visit_date_card_",sdate[m]) %in% names(dat)){
      var1 <- get(paste0("visit_date_card_",sdate[m]),dat)
      var2 <- get(paste0("ID02AJ",sdate[m]),dat)
      #TODO: double check here
      if (any(!(var1 == var2) %in% TRUE)){
        warningmsgs <- c(warningmsgs,
                         paste0("visit_date_card_",sdate[m], "should be equal to visit date for the purpose of MOV calculations"))
      }
    }
  } #end of sdate m loop

  if (!"visit_history" %in% names(dat)){
    dat <- dat %>% mutate(visit_history = NA)
    dat$visit_history <- as.numeric(dat$visit_history)
    dat$visit_history <- haven::labelled(dat$visit_history, label = "Created so the survey date may be used in MOV tool")
  }

  if (!"visit_today" %in% names(dat)){
    dat <- dat %>% mutate(visit_today = NA)
    dat$visit_today <- as.numeric(dat$visit_today)
    dat$visit_today <- haven::labelled(dat$visit_today, label = "Created so the survey date may be used in MOV tool")
  }

  if (!"visit_tick_card" %in% names(dat)){
    dat <- dat %>% mutate(visit_tick_card = NA)
    dat$visit_tick_card <- as.numeric(dat$visit_tick_card)
    dat$visit_tick_card <- haven::labelled(dat$visit_tick_card, label = "Created so the survey date may be used in MOV tool")
  }

  # Create psweight variable
  if (!"psweight_1year" %in% names(dat)){
    dat <- dat %>% mutate(psweight_1year = 1)
    dat$psweight_1year <- haven::labelled(dat$psweight_1year, label = "Weight set to 1 for all")
  }

  if (!"respid" %in% names(dat)){
    dat <- dat %>% group_by(RI01, RI03, RI11, RI12) %>%
      mutate(respid = cur_group_id()) %>%
      ungroup()
    dat$respid <- haven::labelled(dat$respid, label = "Unique respondent ID created by grouping ID02AB - Value of 1 - ID02AIid - ES01AA")
  }

  # Create required variables from gen_es_dv, merge_data_for_MOV_dvs and gen_MOV_dvs
  # if they do not exist in dataset.

  varlist <- c("ES04AA","ES03AA_1_1","ES08AA","ES08AA_1A_01","ES08AA_1A_02","ES08AA_1A_03",
               "ES08AA_1B_01","ES08AA_1B_02","ES08AA_1B_03","ES08AA_1B_04","ES08AA_1B_05",
               "ES08AA_1B_06","ES08AA_1B_07","ES08AA_1B_08","ES08AA_1B_09","ES08AA_1B_10",
               "ES08AA_1B_11","ES08AA_1B_12","ES08AA_1C_01","ES08AA_1C_02","ES08AA_1C_03",
               "ES08AA_1C_04","ES08AA_1C_05","ES08AA_1C_06","ES08AA_1C_07","ES08AA_1C_08",
               "ES08AA_1C_09","ES08AA_1A_03_01","ES08AA_1A_03_02","ES08AA_1A_03_03",
               "ES08AA_1A_03_04","ES08AA_1A_03_05","ES08AA_1A_03_06","ES08AA_1A_03_07",
               "ES08AA_1A_03_08","ES08AA_1A_03_09","ES08AA_1A_03_10","ES08AA_1A_03_11")

  for (v in seq_along(varlist)){
    if (!varlist[v] %in% names(dat)){
      dat <- dat %>% mutate(!!varlist[v] := NA)
      warningmsgs <- c(warningmsgs,
                       paste0("Variable ",varlist[v], " does not exist in dataset but is required to run program so it has been generated with a missing value."))
      vcqi_log_comment(VCP,2,"Warning",
                       paste0("Variable ",varlist[v], " does not exist in dataset but is required to run program so it has been generated with a missing value."))
    }
  } #end of varlist v loop

  varlist2 <- c("ES08AA_1A_03_11OS", "ES08AA_1B_12OS", "ES08AA_1C_09OS")
  for (v in seq_along(varlist2)){
    if (!varlist2[v] %in% names(dat)){
      dat <- dat %>% mutate(!!varlist2[v] := NA_character_)
    }
  } #end of varlist2 v loop

  saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER,"/",VCQI_RI_DATASET))

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }
}
