#' Calculate derived variables for RI_COVG_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_03_<ANALYSIS_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import haven

# RI_COVG_03_03DV R version 1.00 - Biostat Global Consulting - 2022-12-13
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-13  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_03_03DV <- function(VCP = "RI_COVG_03_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  #NOTE: for variable notes, we assign comment to that variable.

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_03_",ANALYSIS_COUNTER,".rds"))

  if (VCQI_NO_DOBS != 1){
    cv <- c("crude", "valid")
  } else {
    cv <- "crude"
  }

  for (v in seq_along(cv)){
    dat <- dat %>% mutate(tempvar1 = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE, 0, NA))
    #label variable fv_count_`v' "Count of `v' vaccinations from full vaccination list"

    fullylower <- str_to_lower(RI_DOSES_TO_BE_FULLY_VACCINATED)
    for (d in seq_along(fullylower)){
      gottoa <- rlang::sym(paste0("got_",cv[v],"_",fullylower[d],"_to_analyze"))
      dat <- dat %>% mutate(tempvar1 = ifelse((!!gottoa == 1) %in% TRUE, tempvar1+1, tempvar1))

    } #end of fullylower d loop

    dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("Count of ", cv[v]," vaccinations from full vaccination list")) %>% suppressWarnings()
    names(dat)[which(names(dat) == "tempvar1")] <- paste0("fv_count_",cv[v])

  } #end of cv v loop

  for (v in seq_along(cv)){
    fvcount <- rlang::sym(paste0("fv_count_",cv[v]))

    dat <- dat %>% mutate(tempvar1 = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE,
                                            ifelse((!!fvcount == length(RI_DOSES_TO_BE_FULLY_VACCINATED)) %in% TRUE,1 ,0), NA))

    dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("Fully Vaccinated - ", cv[v])) %>% suppressWarnings()
    comment(dat$tempvar1) <-
      paste0("Received all doses in this list: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("fully_vaccinated_",cv[v])

  } #end of cv v loop


  # Do valid dose by age 1 calcs if some respondents have DOB
  if (VCQI_NO_DOBS != 1){
    dat <- dat %>% mutate(fvb1_count = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE, 0, NA))

    fullylower <- str_to_lower(RI_DOSES_TO_BE_FULLY_VACCINATED)
    for (d in seq_along(fullylower)){
      age1a <- rlang::sym(paste0("valid_",fullylower[d],"_age1_to_analyze"))
      dat <- dat %>% mutate(fvb1_count = ifelse((!!age1a == 1) %in% TRUE, fvb1_count+1, fvb1_count))
    } #end of fullylower d loop

    dat <- dat %>% mutate(fully_vaccinated_by_age1 = ifelse((psweight != 0 & !is.na(psweight)) %in% TRUE,
                          ifelse((fvb1_count == length(RI_DOSES_TO_BE_FULLY_VACCINATED)) %in% TRUE,1 ,0), NA))

    dat$fvb1_count <- haven::labelled(dat$fvb1_count,
                                      label = "Count of valid vaccinations by age 1") %>% suppressWarnings()
    dat$fully_vaccinated_by_age1 <- haven::labelled(dat$fully_vaccinated_by_age1,
                                                 label = "Fully Vaccinated with Valid Doses by Age 1") %>% suppressWarnings()
    comment(dat$fully_vaccinated_by_age1) <-
      paste0("Received these doses by age 1: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))
  }

  # Are they fully-vaccinated for their age?

  # Doses required for their age
  dat <- dat %>% mutate(fv_doses_reqd_for_age = 0)

  # Doses received (from the required list)
  dat <- dat %>% mutate(fv_doses_recd_for_age_crude = 0, fv_doses_recd_for_age_valid = 0)

  fullylower <- str_to_lower(RI_DOSES_TO_BE_FULLY_VACCINATED)
  for (d in seq_along(fullylower)){
    min_age <- rlang::sym(paste0(fullylower[d],"_min_age_days"))
    gotcrude <- rlang::sym(paste0("got_crude_",fullylower[d],"_to_analyze"))
    gotvalid <- rlang::sym(paste0("got_valid_",fullylower[d],"_to_analyze"))
    dat <- dat %>% mutate(fv_doses_reqd_for_age = ifelse(((age_at_interview >= !!min_age) & !is.na(age_at_interview)) %in% TRUE,
                                                         fv_doses_reqd_for_age + 1, fv_doses_reqd_for_age))

    dat <- dat %>% mutate(fv_doses_recd_for_age_crude = ifelse((!!gotcrude == 1 & (age_at_interview >= !!min_age) & !is.na(age_at_interview)) %in% TRUE,
                                                               fv_doses_recd_for_age_crude + 1, fv_doses_recd_for_age_crude))

    dat <- dat %>% mutate(fv_doses_recd_for_age_valid = ifelse((!!gotvalid == 1 & (age_at_interview >= !!min_age) & !is.na(age_at_interview)) %in% TRUE,
                                                               fv_doses_recd_for_age_valid + 1, fv_doses_recd_for_age_valid))

  }#end of fullylower d loop

  dat <- dat %>% mutate(fully_vxd_for_age_crude = ifelse((fv_doses_reqd_for_age == fv_doses_recd_for_age_crude) & (fv_doses_reqd_for_age > 0) %in% TRUE,
                                                         1, 0),
                        fully_vxd_for_age_valid = ifelse((fv_doses_reqd_for_age == fv_doses_recd_for_age_valid) & (fv_doses_reqd_for_age > 0) %in% TRUE,
                                                         1, 0))

  # Set to missing if this child is not eligible for any doses, or we do not know their age at interview
  dat <- dat %>% mutate(fully_vxd_for_age_crude = ifelse((psweight == 0 | is.na(psweight) | is.na(age_at_interview) | fv_doses_reqd_for_age == 0 ) %in% TRUE,
                                                         NA, fully_vxd_for_age_crude),
                        fully_vxd_for_age_valid = ifelse((psweight == 0 | is.na(psweight) | is.na(age_at_interview) | fv_doses_reqd_for_age == 0 ) %in% TRUE,
                                                         NA, fully_vxd_for_age_valid))
  comment(dat$fully_vxd_for_age_crude) <-
    paste0("Received all crude doses from this list for which they are age-eligible: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))
  comment(dat$fully_vxd_for_age_valid) <-
    paste0("Received all valid doses from this list for which they are age-eligible: ",str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " "))

  dat$fv_doses_reqd_for_age <- haven::labelled(dat$fv_doses_reqd_for_age,
                                                 label = "Number of doses to be fully vaccinated (for their age)") %>% suppressWarnings()
  dat$fv_doses_recd_for_age_crude <- haven::labelled(dat$fv_doses_recd_for_age_crude,
                                                 label = "Number of crude doses received from list needed (for their age)") %>% suppressWarnings()
  dat$fv_doses_recd_for_age_valid <- haven::labelled(dat$fv_doses_recd_for_age_valid,
                                                     label = "Number of valid doses received from list needed (for their age)") %>% suppressWarnings()
  dat$fully_vxd_for_age_crude <- haven::labelled(dat$fully_vxd_for_age_crude,
                                                 label = "Fully Vaccinated with Crude Doses (for their age)") %>% suppressWarnings()
  dat$fully_vxd_for_age_valid <- haven::labelled(dat$fully_vxd_for_age_valid,
                                                 label = "Fully Vaccinated with Valid Doses (for their age)") %>% suppressWarnings()


  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_03_",ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
