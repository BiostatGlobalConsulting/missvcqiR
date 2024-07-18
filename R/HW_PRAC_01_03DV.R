#' Calculate derived variables for HW_PRAC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (HW_PRAC_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr
#' @import haven

# HW_PRAC_01_03DV R version 1.01 - Biostat Global Consulting - 2024-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# 2024-07-18  1.01      Caitlin Clary   Fix HW06AD issue
# *******************************************************************************

HW_PRAC_01_03DV <- function(VCP = "HW_PRAC_01_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,".rds"))

  for (i in 29:32){
    dat <- dat %>% mutate(tempvar = 0)
    dat$tempvar <- haven::labelled(dat$tempvar, label = paste0("Scores for variable HW",i)) %>% suppressWarnings()
    names(dat)[which(names(dat) == "tempvar")] <- paste0("coded_HW",i)
  } #end of i loop

  dat <- dat %>% mutate(coded_HW29 = ifelse(HW06AA %in% 5,1,0))
  dat <- dat %>% mutate(coded_HW30 = ifelse(HW06AB %in% 5,1,0))
  dat <- dat %>% mutate(coded_HW31 = ifelse(HW06AC %in% 4,1,0))

  for (i in 1:5){
    var <- rlang::sym(paste0("HW06AD_", i))
    dat <- dat %>%
      mutate(coded_HW32 = ifelse(!!var %in% 1, coded_HW32+1, coded_HW32))
  } #end of i loop

  dat <- dat %>% mutate(coded_HW32 = ifelse(HW06AD_6 %in% 1, 5, coded_HW32))

  dat <- dat %>% mutate(vacc_practices = 0)
  dat$vacc_practices <- haven::labelled(
    dat$vacc_practices,
    label = "Variable that shows the score for vaccination practices - part 1") %>% suppressWarnings()

  for (i in 29:32){
    var <- rlang::sym(paste0("coded_HW",i))
    dat <- dat %>% mutate(vacc_practices = vacc_practices + !!var)
  } #end of i loop

  dat <- dat %>% mutate(improper_practices_1 = ifelse((vacc_practices < 6) %in% TRUE, 1, 0))
  dat$improper_practices_1 <- haven::labelled(dat$improper_practices_1,
                                              label = "HW06AA-HW06AD_6 indicate poor practices") %>% suppressWarnings()

  dat <- dat %>% mutate(admin_vx = 0)
  dat$admin_vx <- haven::labelled(dat$admin_vx,
                                        label = "Respondent answered questions indicating they administer vaccines if greater than 0") %>% suppressWarnings()

  varlist <- c("HW06AE", "HW06AF", "HW06AG", "HW06AH", "HW06AI", "HW06AJ", "HW06AK_1", "HW06AK_2",
               "HW06AK_3", "HW06AK_4")
  for (v in seq_along(varlist)){
    var <- rlang::sym(varlist[v])
    dat <- dat %>% mutate(admin_vx = ifelse(!is.na(!!var), admin_vx+1, admin_vx))
  } #end of varlist v loop

  for (i in 33:39){
    dat <- dat %>% mutate(tempvar = ifelse((admin_vx != 0) %in% TRUE, 0, NA))
    dat$tempvar <- haven::labelled(dat$tempvar, label = paste0("Scores for variable HW",i)) %>% suppressWarnings()
    names(dat)[which(names(dat) == "tempvar")] <- paste0("coded_HW",i)
  } #end of i loop

  dat <- dat %>% mutate(coded_HW33 = ifelse(HW06AE %in% 4,1,0))
  dat <- dat %>% mutate(coded_HW34 = ifelse(HW06AF %in% 4,1,0))
  dat <- dat %>% mutate(coded_HW35 = ifelse(HW06AG %in% 4,1,0))
  dat <- dat %>% mutate(coded_HW36 = ifelse(HW06AH %in% 4,1,0))
  dat <- dat %>% mutate(coded_HW37 = ifelse(HW06AI %in% 3,1,0))
  dat <- dat %>% mutate(coded_HW38 = ifelse(HW06AJ %in% 7,1,0))

  dat <- dat %>% mutate(coded_HW39 = ifelse(HW06AK_1 %in% 1,1,0))
  dat <- dat %>% mutate(coded_HW39 = ifelse(HW06AK_2 %in% 2,coded_HW39 + 1,coded_HW39))
  dat <- dat %>% mutate(coded_HW39 = ifelse(HW06AK_3 %in% 1,coded_HW39 + 1,coded_HW39))
  dat <- dat %>% mutate(coded_HW39 = ifelse(HW06AK_4 %in% 3,coded_HW39 + 1,coded_HW39))

  dat <- dat %>% mutate(vacc_practices_2 = 0)
  dat$vacc_practices_2 <- haven::labelled(dat$vacc_practices_2,
                                        label = "Variable that shows the score for vaccination practices - part 2") %>% suppressWarnings()

  for (i in 33:39){
    var <- rlang::sym(paste0("coded_HW",i))
    dat <- dat %>% mutate(vacc_practices_2 = vacc_practices_2 + !!var)
  } #end of i loop

  dat <- dat %>% mutate(improper_practices_2 = ifelse((vacc_practices_2 < 8) %in% TRUE, 1, 0))
  dat$improper_practices_2 <- haven::labelled(dat$improper_practices_2,
                                              label = "HW06AE-39 indicate poor practices") %>% suppressWarnings()

  dat <- dat %>% mutate(improper_practices_none = ifelse(improper_practices_1 == 0 & improper_practices_2 == 0, 1, 0))
  dat$improper_practices_none <- haven::labelled(dat$improper_practices_none,
                                     label = "HW06AA-39 do not indicate improper practices") %>% suppressWarnings()

  dat <- dat %>% mutate(improper_practices_any = ifelse(improper_practices_1 == 1 | improper_practices_2 == 1, 1, 0))
  dat$improper_practices_any <- haven::labelled(dat$improper_practices_any,
                                     label = "HW06AA-32 or HW06AE-39 indicate poor practices") %>% suppressWarnings()

  dat <- dat %>% mutate(improper_practices_both = ifelse(improper_practices_1 == 1 & improper_practices_2 == 1, 1, 0))
  dat$improper_practices_both <- haven::labelled(dat$improper_practices_both,
                                     label = "HW06AA-32 and HW06AE-39 indicate poor practices") %>% suppressWarnings()

  dat <- dat %>% mutate(which_improper_practices = "No Improper Practices")
  dat <- dat %>% mutate(which_improper_practices = ifelse(improper_practices_1 == 1 & improper_practices_2 == 0,
                                                         "Improper Practices: Part 1", which_improper_practices))
  dat <- dat %>% mutate(which_improper_practices = ifelse(improper_practices_1 == 0 & improper_practices_2 == 1,
                                                         "Improper Practices: Part 2", which_improper_practices))
  dat <- dat %>% mutate(which_improper_practices = ifelse(improper_practices_1 == 1 & improper_practices_2 == 1,
                                                         "Improper Practices: Both 1 & 2", which_improper_practices))
  dat$which_improper_practices <- haven::labelled(dat$which_improper_practices,
                                                 label = "Improper vaccination practices") %>% suppressWarnings()

  dat <- dat %>% select(-c(all_of(starts_with("coded")),all_of(starts_with("vacc_practices"))))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/HW_PRAC_01_", ANALYSIS_COUNTER, ".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
