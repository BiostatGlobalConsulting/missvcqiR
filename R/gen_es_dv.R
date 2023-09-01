#' Reads in the Exit survey dataset and creates the derived variables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @importFrom sjlabelled set_labels
#' @rawNamespace import(tools, except = makevars_user)
#'
#' @return a dataset
#'
#' @export
#'
#' @examples
#' gen_es_dv()

# gen_es_dv R version 1.00 - Biostat Global Consulting - 2023-08-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-29  1.00      Mia Yu          Original R package version
# *******************************************************************************

gen_es_dv <- function(VCP = "gen_es_dv"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(VCQI_RI_DATASET),"_clean.rds"))

  # Create single variable to group Caregiver's age
  dat <- dat %>% mutate(caregiver_age = NA)
  dat <- dat %>% mutate(caregiver_age = case_when(ES04AA < 20 ~ 1, ES04AA >= 20 & ES04AA < 30 ~ 2,
                                                 ES04AA >= 30 & ES04AA < 40 ~ 3, ES04AA >= 40 & ES04AA < 50 ~ 4,
                                                 ES04AA >= 50 ~ 5))
  dat$caregiver_age <- haven::labelled(dat$caregiver_age, label = "Caregiver's Age grouped for tables")
  dat$caregiver_age <- sjlabelled::set_labels(dat$caregiver_age,
                                              labels = c("<20" = 1, "20-29" = 2, "30-39" = 3, "40-49" = 4, "50+" = 5),
                                              force.labels = TRUE)

  # Create variable to group childs age
  dat <- dat %>% mutate(childs_age = NA)
  dat <- dat %>% mutate(childs_age = case_when(ES03AA_1_1 %in% 0 ~ 1, ES03AA_1_1 %in% 1 ~ 2, ES03AA_1_1 >=2 ~ 3))
  dat$childs_age <- haven::labelled(dat$childs_age, label = "Child's age grouped by month for table shells")
  dat$childs_age <- sjlabelled::set_labels(dat$childs_age,
                                              labels = c("<1y" = 1, "1y" = 2, "2-5y" = 3),force.labels = TRUE)

  # Clean up variable ES08AA (child received vaccination today)
  # to set to no if no doses received that day and yes if
  # any vaccination dates equal visit date
  dat <- dat %>% mutate(got_dose_today = 0)
  for (v in seq_along(RI_DOSE_LIST_MINUS_VISIT)){
    cardm <- rlang::sym(paste0(RI_DOSE_LIST_MINUS_VISIT[v],"_date_card_m"))
    cardd <- rlang::sym(paste0(RI_DOSE_LIST_MINUS_VISIT[v],"_date_card_d"))
    cardy <- rlang::sym(paste0(RI_DOSE_LIST_MINUS_VISIT[v],"_date_card_y"))

    dat <- dat %>% mutate(got_dose_today = ifelse((!!cardm == visit_date_card_m & !!cardd == visit_date_card_d & !!cardy == visit_date_card_y) %in% TRUE,
                                                  got_dose_today + 1, got_dose_today))
  } #end of RI_DOSE_LIST_MINUS_VISIT v loop

  dat <- dat %>% mutate(ES08AA = ifelse(got_dose_today %in% 0,2,ES08AA)) %>%
    mutate(ES08AA = ifelse(got_dose_today >= 1,1,ES08AA))

  #NOTE: this program currently does not include the part dealing with level2id and level2names

  saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(VCQI_RI_DATASET),"_clean.rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
