#' This program runs coverage analysis to create variables for Proportion of Coverage
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#'
#' @return a dataset
#'
#' @export
#'
#' @examples
#' assign_covg_category()

# assign_covg_category R version 1.00 - Biostat Global Consulting - 2023-09-07
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-07  1.00      Mia Yu          Original R package version
# *******************************************************************************

assign_covg_category <- function(VCP = "assign_covg_category"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print("Running Coverage analysis to create variables for Proportion of Coverage")
  # Clear out globals so that they do not create any output other than datasets

  export <- EXPORT_TO_EXCEL
  plots <- MAKE_PLOTS
  database <- VCQI_GENERATE_DATABASES

  vcqi_global(VCQI_GENERATE_DATABASES, 0)
  vcqi_global(EXPORT_TO_EXCEL, 0)
  vcqi_global(MAKE_PLOTS, 0)

  # Reset RI DOSE List and RI_SINGLE_DOSE_LIST to exclude visit
  # vcqi_global RI_DOSE_LIST $RI_DOSE_LIST_MINUS_VISIT
  # vcqi_global RI_SINGLE_DOSE_LIST  = subinstr("$RI_SINGLE_DOSE_LIST","VISIT","",1)

  # Estimate crude dose coverage for all the doses in the RI_DOSE_LIST
  # Estimate valid dose coverage for all the doses in the RI_DOSE_LIST

  # Estimate proportion of respondents fully vaccinated
  # Estimate proportion of respondents not fully vaccinated

  if (!vcqi_object_exists("RI_DOSES_TO_BE_FULLY_VACCINATED")){
    vcqi_global(RI_DOSES_TO_BE_FULLY_VACCINATED, MOV_OUTPUT_DOSE_LIST)
  } else {
    vcqi_global(RI_DOSES_TO_BE_FULLY_VACCINATED, str_to_lower(RI_DOSES_TO_BE_FULLY_VACCINATED))

    # Check to see that each dose listed in RI_DOSES_TO_BE_FULLY_VACCINATED is included in the analysis
    if (vcqi_object_exists("RI_DOSES_TO_BE_FULLY_VACCINATED")){
      exitflag <- 0
      errormsgs <- NULL

      dlist <- str_to_upper(RI_DOSES_TO_BE_FULLY_VACCINATED)
      movlist <- str_to_upper(MOV_OUTPUT_DOSE_LIST)

      for (d in seq_along(dlist)){
        if (!dlist[d] %in% movlist){
          errormsgs <- c(errormsgs,
                         paste0("RI_DOSES_TO_BE_FULLY_VACCINATED includes ",
                                dlist[d], ", but ",dlist[d]," does not appear in the MOV_OUTPUT_DOSE_LIST. Either add ",
                                dlist[d]," to that macro or remove it from RI_DOSES_TO_BE_FULLY_VACCINATED."))
          exitflag <- 1
          vcqi_log_comment(VCP, 1, "Error",
                           paste0("RI_DOSES_TO_BE_FULLY_VACCINATED includes ",
                                  dlist[d], ", but ",dlist[d]," does not appear in the MOV_OUTPUT_DOSE_LIST. Either add ",
                                  dlist[d]," to that macro or remove it from RI_DOSES_TO_BE_FULLY_VACCINATED."))
        }
      } #end of dlist d loop

      if(exitflag == 1){
        vcqi_global(VCQI_ERROR, 1)
        miss_vcqi_halt_immediately(
          halt_message = errormsgs
        )
      }
    }
  }

  RI_COVG_01_02_03_04()

  # Create variable that shows if they are fully vaccinated, unvaccinated,
  # or undervaccinated based on the above Indicators
  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_04_",ANALYSIS_COUNTER,".rds")) %>%
    select(c(respid,all_of(starts_with("not"))))

  dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_03_",ANALYSIS_COUNTER,".rds")) %>%
    select(c(respid,fully_vxd_for_age_crude, fully_vxd_for_age_valid))
  dat <- full_join(dat,dat2,by = "respid")

  stype <- c("crude", "valid")
  for (vc in seq_along(stype)){
    vx_age <- rlang::sym(paste0("fully_vxd_for_age_",stype[vc]))
    not_vx <- rlang::sym(paste0("not_vaccinated_",stype[vc]))

    dat <- dat %>% mutate(tempvar = ifelse((!(!!vx_age) %in% 1) & (!(!!not_vx) %in% 1), 1, 0))

    dat$tempvar <- haven::labelled(dat$tempvar, label = paste0("Undervaccinated - ",stype[vc]))
    names(dat)[which(names(dat) == "tempvar")] <- paste0("under_vaccinated_",stype[vc])
  } #end of stype vc loop

  dat3 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/ES_with_ids.rds"))
  dat <- full_join(dat,dat3,by = "respid")

  # Create new single variable that will show the proportion of coverage
  #label define coverage 1 "Fully Vaccinated" 2 "Unvaccinated" 3 "Undervaccinated"
  label1 <- language_string(language_use = language_use, str = "OS_217", replaceq = TRUE)
  label2 <- language_string(language_use = language_use, str = "OS_218", replaceq = TRUE)
  label3 <- language_string(language_use = language_use, str = "OS_219", replaceq = TRUE)

  for (vc in seq_along(stype)){

    vx_age <- rlang::sym(paste0("fully_vxd_for_age_",stype[vc]))
    not_vx <- rlang::sym(paste0("not_vaccinated_",stype[vc]))
    under_vx <- rlang::sym(paste0("under_vaccinated_",stype[vc]))

    dat <- dat %>% mutate(tempvar = ifelse(!!vx_age %in% 1, 1, NA)) %>%
      mutate(tempvar = ifelse(!!not_vx %in% 1, 2, tempvar)) %>%
      mutate(tempvar = ifelse(!!under_vx %in% 1, 3, tempvar))

    f <- paste0("dat$tempvar <- haven::labelled(dat$tempvar, label = 'Variable that shows fully vacc, not vacc, and under vacc - ",
                stype[vc],"', labels = c('",label1,
                "' = 1, '",label2,"' = 2, '",label3,"' = 3))")
    eval(rlang::parse_expr(f))

    names(dat)[which(names(dat) == "tempvar")] <- paste0("covg_category_",stype[vc])
  } #end of stype vc loop

  saveRDS(dat,file = paste0(VCQI_OUTPUT_FOLDER,"/ES_with_ids.rds"))

  vcqi_global(VCQI_GENERATE_DATABASES, database)
  vcqi_global(EXPORT_TO_EXCEL, export)
  vcqi_global(MAKE_PLOTS, plots)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
