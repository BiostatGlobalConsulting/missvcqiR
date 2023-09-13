#' Calculate derived variables for RI_COVG_04
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_04_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import haven

# RI_COVG_04_03DV R version 1.00 - Biostat Global Consulting - 2022-12-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-14  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_04_03DV <- function(VCP = "RI_COVG_04_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_04_",ANALYSIS_COUNTER,".rds"))

  dat <- dat %>% mutate(not_vaccinated_crude = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE,
                                                      ifelse(fv_count_crude == 0,1,0), NA))

  dat$not_vaccinated_crude <- haven::labelled(dat$not_vaccinated_crude,
                                              label = "Not Vaccinated - crude") %>% suppressWarnings()
  comment(dat$not_vaccinated_crude) <-
    paste0("Did not receive any crude dose from this list: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))

  # Skip valid dose calculations if no respondent had DOB data

  if (VCQI_NO_DOBS != 1){
    dat <- dat %>% mutate(not_vaccinated_valid = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE,
                                                        ifelse(fv_count_valid == 0,1,0), NA))

    comment(dat$not_vaccinated_valid) <-
      paste0("Did not receive any valid dose from this list: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))
    dat$not_vaccinated_valid <- haven::labelled(dat$not_vaccinated_valid,
                                                label = "Not Vaccinated - valid") %>% suppressWarnings()

    dat <- dat %>% mutate(not_vaccinated_by_age1 = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE,
                                                        ifelse(fvb1_count == 0,1,0), NA))
    dat$not_vaccinated_by_age1 <- haven::labelled(dat$not_vaccinated_by_age1,
                                                label = "No Valid Doses from Fully Vaccinated List by Age 1") %>% suppressWarnings()
    comment(dat$not_vaccinated_by_age1) <-
      paste0("Did not receive any valid dose by age 1 from this list: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))

  }

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_04_",ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
