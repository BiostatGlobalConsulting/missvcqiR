#' Calculate derived variables for HW_BAR_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (HW_BAR_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr
#' @import haven

# HW_BAR_01_03DV R version 1.01 - Biostat Global Consulting - 2024-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# 2024-07-18  1.01      Caitlin Clary   Fix typo re: HW04AC_5
# *******************************************************************************

HW_BAR_01_03DV <- function(VCP = "HW_BAR_01_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/HW_BAR_01_",ANALYSIS_COUNTER,".rds"))

  for (i in 10:28){
    dat <- dat %>% mutate(tempvar = 0)
    dat$tempvar <- haven::labelled(dat$tempvar, label = paste0("Scores for variable HW",i)) %>% suppressWarnings()
    names(dat)[which(names(dat) == "tempvar")] <- paste0("coded_HW",i)
  } #end of i loop

  # HW04AA only gets credit if options 1(BCG)  and 3(Hepb) are selected.
  # Coding per the document all or nothing
  dat <- dat %>% mutate(coded_HW10 = ifelse((HW04AA_1==1 & HW04AA_3==1) %in% TRUE,1,0))

  # Reset to 0 if any other values are selected
  dat <- dat %>% mutate(coded_HW10 = ifelse(HW04AA_2 %in% 1, 0, coded_HW10))
  dat <- dat %>% mutate(coded_HW10 = ifelse(HW04AA_4 %in% 1, 0, coded_HW10))
  dat <- dat %>% mutate(coded_HW10 = ifelse(HW04AA_5 %in% 1, 0, coded_HW10))

  # HW04AB only gets credit if option 1
  # This can only get credit if all other responses were set to 0
  dat <- dat %>% mutate(coded_HW11 = ifelse(HW04AB_1 %in% 1,1,0))

  # Reset to 0 if any other values are populated
  dat <- dat %>% mutate(coded_HW11 = ifelse(HW04AB_2 %in% 1, 0, coded_HW11))
  dat <- dat %>% mutate(coded_HW11 = ifelse(HW04AB_3 %in% 1, 0, coded_HW11))
  dat <- dat %>% mutate(coded_HW11 = ifelse(HW04AB_4 %in% 1, 0, coded_HW11))
  dat <- dat %>% mutate(coded_HW11 = ifelse(HW04AB_5 %in% 1, 0, coded_HW11))

  # HW04AC should be 5 (None)
  dat <- dat %>% mutate(coded_HW12 = ifelse(HW04AC_5 %in% 1, 1, 0))
  # Reset to 0 if any other values are populated
  dat <- dat %>% mutate(coded_HW12 = ifelse(HW04AC_1 %in% 1, 0, coded_HW12))
  dat <- dat %>% mutate(coded_HW12 = ifelse(HW04AC_2 %in% 1, 0, coded_HW12))
  dat <- dat %>% mutate(coded_HW12 = ifelse(HW04AC_3 %in% 1, 0, coded_HW12))
  dat <- dat %>% mutate(coded_HW12 = ifelse(HW04AC_4 %in% 1, 0, coded_HW12))

  # HW04AD should be 4 (DPT at age 4)
  dat <- dat %>% mutate(coded_HW13 = ifelse(HW04AD_4 %in% 1,1,0))
  # Reset to 0 if any other values are populated
  dat <- dat %>% mutate(coded_HW13 = ifelse(HW04AD_1 %in% 1, 0, coded_HW13))
  dat <- dat %>% mutate(coded_HW13 = ifelse(HW04AD_2 %in% 1, 0, coded_HW13))
  dat <- dat %>% mutate(coded_HW13 = ifelse(HW04AD_3 %in% 1, 0, coded_HW13))
  dat <- dat %>% mutate(coded_HW13 = ifelse(HW04AD_5 %in% 1, 0, coded_HW13))

  dat <- dat %>% mutate(coded_HW14 = ifelse(HW04AE_1 %in% 2,1,0))
	dat <- dat %>% mutate(coded_HW14 = ifelse(HW04AE_2 %in% 4,coded_HW14 + 1,coded_HW14))
	dat <- dat %>% mutate(coded_HW14 = ifelse(HW04AE_3 %in% 1,coded_HW14 + 1,coded_HW14))
	dat <- dat %>% mutate(coded_HW14 = ifelse(HW04AE_4 %in% 5,coded_HW14 + 1,coded_HW14))
	dat <- dat %>% mutate(coded_HW14 = ifelse(HW04AE_5 %in% 3,coded_HW14 + 1,coded_HW14))

	# Below are single response
	dat <- dat %>% mutate(coded_HW15 = ifelse(HW04AF %in% 4,1,0))
	dat <- dat %>% mutate(coded_HW16 = ifelse(HW04AG %in% 5,1,0))
	dat <- dat %>% mutate(coded_HW17 = ifelse(HW04AH %in% 4,1,0))
	dat <- dat %>% mutate(coded_HW18 = ifelse(HW04AI_1 %in% 1,1,0))

	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_1 %in% 1,1,0))
	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_2 %in% 2,coded_HW19 + 1,coded_HW19))
	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_3 %in% 1,coded_HW19 + 1,coded_HW19))
	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_4 %in% 1,coded_HW19 + 1,coded_HW19))
	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_5 %in% 1,coded_HW19 + 1,coded_HW19))
	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_6 %in% 2,coded_HW19 + 1,coded_HW19))
	dat <- dat %>% mutate(coded_HW19 = ifelse(HW04AJ_7 %in% 2,coded_HW19 + 1,coded_HW19))

	dat <- dat %>% mutate(knowledge = 0)
	dat$knowledge <- haven::labelled(dat$knowledge,
	                                 label = "Variable used to show score on knowledge questions") %>% suppressWarnings()

	for (i in 10:19){
	  var <- rlang::sym(paste0("coded_HW",i))
	  dat <- dat %>% mutate(knowledge = knowledge + !!var)
	} #end of i loop

	dat <- dat %>% mutate(knowledge_barriers = ifelse((knowledge < 16) %in% TRUE, 1, 0))
	dat$knowledge_barriers <- haven::labelled(dat$knowledge_barriers,
	                                          label = "Questions HW04AA-HW04AJ indicate knowledge barrier") %>% suppressWarnings()

	# Single response questions
	dat <- dat %>% mutate(coded_HW20 = ifelse(HW05AA %in% 4,1,0))
	dat <- dat %>% mutate(coded_HW21 = ifelse(HW05AB %in% 5,1,0))
	dat <- dat %>% mutate(coded_HW22 = ifelse(HW05AC %in% 7,1,0))
	dat <- dat %>% mutate(coded_HW23 = ifelse(HW05AD %in% 2,1,0))

	dat <- dat %>% select(-c(coded_HW24,coded_HW25,coded_HW26))

	dat <- dat %>% mutate(coded_HW27 = ifelse(HW05AH %in% 2,1,0))
	dat <- dat %>% mutate(coded_HW28 = ifelse(HW05AI %in% 2,1,0))

	dat <- dat %>% mutate(attitudes = 0)
	dat$attitudes <- haven::labelled(dat$attitudes,
	                                 label = "Variable used to show score on attitude questions") %>% suppressWarnings()

	for (i in 20:23){
	  var <- rlang::sym(paste0("coded_HW",i))
	  dat <- dat %>% mutate(attitudes = attitudes + !!var)
	} #end of i loop
	for (i in 27:28){
	  var <- rlang::sym(paste0("coded_HW",i))
	  dat <- dat %>% mutate(attitudes = attitudes + !!var)
	} #end of i loop

	dat <- dat %>% mutate(attitude_barriers = ifelse((attitudes < 4) %in% TRUE, 1, 0))
	dat$attitude_barriers <- haven::labelled(dat$attitude_barriers,
	                                         label = "Questions HW05AA-HW05AD and HW05AH-HW05AI indicate attitude barrier") %>% suppressWarnings()

	dat <- dat %>% mutate(no_barriers = ifelse(knowledge_barriers == 0 & attitude_barriers == 0, 1, 0))
	dat$no_barriers <- haven::labelled(dat$no_barriers,
	                                   label = "Worker did not have knowledge or attitude barriers") %>% suppressWarnings()

	dat <- dat %>% mutate(any_barriers = ifelse(knowledge_barriers == 1 | attitude_barriers == 1 , 1, 0))
	dat$any_barriers <- haven::labelled(dat$any_barriers,
	                                   label = "Worker had knowledge or attitude barriers") %>% suppressWarnings()

	dat <- dat %>% mutate(both_barriers = ifelse(knowledge_barriers == 1 & attitude_barriers == 1, 1, 0))
	dat$both_barriers <- haven::labelled(dat$both_barriers,
	                                   label = "Worker had knowledge and attitude barriers") %>% suppressWarnings()

	dat <- dat %>% select(-c(all_of(starts_with("coded")),knowledge,attitudes))

	saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/HW_BAR_01_", ANALYSIS_COUNTER, ".rds"))

	vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
