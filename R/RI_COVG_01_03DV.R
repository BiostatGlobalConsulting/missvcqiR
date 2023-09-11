#' Calculate derived variables for RI_COVG_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_01_<ANALYSIS_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import haven

# RI_COVG_01_03DV R version 1.03 - Biostat Global Consulting - 2022-10-17
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-03  1.00      Mia Yu          Original R version
# 2022-08-23  1.01      Caitlin Clary   Fixed tempvar2/tempvar3 typo when handling
#                                       placeholder clusters
# 2022-10-05  1.02      Mia Yu          Package version
# 2022-10-17  1.03      Mia Yu          Add variable labels
# *******************************************************************************

RI_COVG_01_03DV <- function(VCP = "RI_COVG_01_03DV"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))


  for (d in seq_along(RI_DOSE_LIST)){
    carddate <- rlang::sym(paste0(RI_DOSE_LIST[d],"_card_date"))
    cardtick <- rlang::sym(paste0(RI_DOSE_LIST[d],"_card_tick"))
    history  <- rlang::sym(paste0(RI_DOSE_LIST[d],"_history"))
    regdate  <- rlang::sym(paste0(RI_DOSE_LIST[d],"_register_date"))
    regtick  <- rlang::sym(paste0(RI_DOSE_LIST[d],"_register_tick"))

    dat <- dat %>%
      mutate(
        tempvar1 = ifelse((!is.na(!!carddate) | !!cardtick == 1) %in% TRUE, 1, 0),
        tempvar2 = ifelse((!!history == 1) %in% TRUE, 1, 0),
        tempvar3 = ifelse((!is.na(!!regdate) | !!regtick %in% 1) %in% TRUE, 1, 0),
        tempvar4 = ifelse((tempvar1 == 1 | tempvar2 == 1) %in% TRUE, 1, 0),
        tempvar5 = ifelse((tempvar1 == 1 | tempvar3 == 1) %in% TRUE, 1, 0),
        tempvar6 = ifelse((tempvar4 == 1 | tempvar3 == 1) %in% TRUE, 1, 0)
      )

    if (RI_DOSE_LIST[d] == "bcg"){
      dat <- mutate(dat, got_crude_bcg_by_scar = ifelse((bcg_scar_history == 1) %in% TRUE, 1, 0),
                    tempvar4  = ifelse((bcg_scar_history==1) %in% TRUE, 1, tempvar4),
                    tempvar6  = ifelse((bcg_scar_history==1) %in% TRUE, 1, tempvar6))
    } #end of bcg

    if (RI_RECORDS_NOT_SOUGHT == 1){
      dat <- mutate(dat, tempvar7 = tempvar4)
    } #end of RI_RECORDS_NOT_SOUGHT

    if (RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      dat <- mutate(dat, tempvar7 = tempvar4)
      dat <- mutate(dat, tempvar7 = ifelse((no_card == 1) %in% TRUE,
                                           ifelse((tempvar3 == 1 | tempvar2 == 1) %in% TRUE, 1, 0),
                                           tempvar7))

      if (RI_DOSE_LIST[d] == "bcg"){
        dat <- mutate(dat,tempvar7 = ifelse((bcg_scar_history == 1) %in% TRUE,
                                            1, tempvar7))
      } #end of bcg
    } #end of RI_RECORDS_SOUGHT_IF_NO_CARD

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1){
      dat <- mutate(dat, tempvar7 = tempvar6)
    } #end of RI_RECORDS_SOUGHT_FOR_ALL


    # For (uncommon) doses where the age of dose-eligibility is older than
    # the age of survey-eligibility (e.g., 2nd dose of measles in 2nd
    # year of life) remove respondents from the denominator if they are
    # not clearly age-eligible for the dose

    minage <- get(paste0(RI_DOSE_LIST[d],"_min_age_days"),envir = .GlobalEnv)

    dat <- dat %>% mutate(tempvar1 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar1)) %>% suppressWarnings()
    dat <- dat %>% mutate(tempvar2 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar2)) %>% suppressWarnings()
    dat <- dat %>% mutate(tempvar3 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar3)) %>% suppressWarnings()
    dat <- dat %>% mutate(tempvar4 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar4)) %>% suppressWarnings()
    dat <- dat %>% mutate(tempvar5 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar5)) %>% suppressWarnings()
    dat <- dat %>% mutate(tempvar6 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar6)) %>% suppressWarnings()
    dat <- dat %>% mutate(tempvar7 = ifelse(((minage > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview) |age_at_interview < minage)) %in% TRUE,
                                            NA, tempvar7)) %>% suppressWarnings()

    if (RI_DOSE_LIST[d] == "bcg"){
      dat <- dat %>% mutate(got_crude_bcg_by_scar = ifelse(((bcg_min_age_days > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                                            & (is.na(age_at_interview) | age_at_interview < bcg_min_age_days)) %in% TRUE,
                                                           NA,got_crude_bcg_by_scar)) %>% suppressWarnings()
    } #end of bcg

    # Sometimes there will be rows in the dataset with all empty input variables, and a value of 0 for psweight
    # These are placeholders for clusters that did not yield any kids
    # They do not represent actual children, so the outcome should be missing for any row where the weight is 0.

    dat <- dat %>% mutate(tempvar1 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar1),
                          tempvar2 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar2),
                          tempvar3 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar3),
                          tempvar4 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar4),
                          tempvar5 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar5),
                          tempvar6 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar6),
                          tempvar7 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar7)) %>% suppressWarnings()

    if (RI_DOSE_LIST[d] == "bcg"){
      dat <- mutate(dat, got_crude_bcg_by_scar = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, got_crude_bcg_by_scar))
    }

    dat$tempvar1 <- as.numeric(dat$tempvar1)
    dat$tempvar2 <- as.numeric(dat$tempvar2)
    dat$tempvar3 <- as.numeric(dat$tempvar3)
    dat$tempvar4 <- as.numeric(dat$tempvar4)
    dat$tempvar5 <- as.numeric(dat$tempvar5)
    dat$tempvar6 <- as.numeric(dat$tempvar6)
    dat$tempvar7 <- as.numeric(dat$tempvar7)

    dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("Got ", RI_DOSE_LIST[d], ", by card")) %>% suppressWarnings()
    dat$tempvar2 <- haven::labelled(dat$tempvar2, label = paste0("Got ", RI_DOSE_LIST[d], ", by history")) %>% suppressWarnings()
    dat$tempvar3 <- haven::labelled(dat$tempvar3, label = paste0("Got ", RI_DOSE_LIST[d], ", by register")) %>% suppressWarnings()
    dat$tempvar4 <- haven::labelled(dat$tempvar4, label = paste0("Got ", RI_DOSE_LIST[d], ", by card or history")) %>% suppressWarnings()
    dat$tempvar5 <- haven::labelled(dat$tempvar5, label = paste0("Got ", RI_DOSE_LIST[d], ", by card or register")) %>% suppressWarnings()
    dat$tempvar6 <- haven::labelled(dat$tempvar6, label = paste0("Got ", RI_DOSE_LIST[d], ", by card, history or register")) %>% suppressWarnings()
    dat$tempvar7 <- haven::labelled(dat$tempvar7, label = paste0("Got ", RI_DOSE_LIST[d], ", to analyze")) %>% suppressWarnings()


    if (RI_RECORDS_NOT_SOUGHT == 1){
      dat <- select(dat, -c(tempvar3,tempvar5,tempvar6))
    } #end of RI_RECORDS_NOT_SOUGHT

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_by_card")
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_by_history")
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_by_register")
    names(dat)[which(names(dat) == "tempvar4")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_c_or_h")
    names(dat)[which(names(dat) == "tempvar5")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_c_or_r")
    names(dat)[which(names(dat) == "tempvar6")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_c_or_h_or_r")
    names(dat)[which(names(dat) == "tempvar7")] <- paste0("got_crude_",RI_DOSE_LIST[d],"_to_analyze")

  } #end of dose loop

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
