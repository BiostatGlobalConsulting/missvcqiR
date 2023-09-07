#' Reads in the Health Worker survey and creates the derived variables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @import haven
#' @rawNamespace import(tools, except = makevars_user)
#'
#' @return a dataset
#'
#' @export
#'
#' @examples
#' gen_hw_dv()

# gen_hw_dv R version 1.00 - Biostat Global Consulting - 2023-08-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-11  1.00      Mia Yu          Original R package version
# *******************************************************************************

gen_hw_dv <- function(VCP = "gen_hw_dv"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET))

  # HW age group
  dat <- dat %>% mutate(age_group = NA)
  label1 <- paste0(language_string(language_use = language_use, str = "OS_240", replaceq = TRUE), " 20") # "Under 20"
  dat <- dat %>% mutate(age_group = case_when(HW03AB <  20 ~ 1, HW03AB >= 20 & HW03AB < 35 ~ 2,
                                              HW03AB >= 35 & HW03AB < 45 ~ 3, HW03AB >= 45 & HW03AB < 55 ~ 4,
                                              HW03AB >= 55 & !is.na(HW03AB) ~ 5))
  f <- paste0("dat$age_group <- haven::labelled(dat$age_group, label = 'Age group', labels = c('",
              label1,"' = 1, '20-34' = 2, '35-44' = 3,'45-54' = 4,'55+' = 5))")
  eval(rlang::parse_expr(f))

  # HW years of experience
  dat <- dat %>% mutate(experience_category = NA)
  label0 <- paste0("<1 ", language_string(language_use = language_use, str = "OS_241", replaceq = TRUE)) # "<1 year"
  label1 <- paste0("1 ", language_string(language_use = language_use, str = "OS_241", replaceq = TRUE)) # "1 year"
  label2 <- paste0("2-4 ", language_string(language_use = language_use, str = "OS_242", replaceq = TRUE)) # "2-4 years"
  label3 <- paste0("5+ ", language_string(language_use = language_use, str = "OS_242", replaceq = TRUE)) # "5+ years"

  dat <- dat %>% mutate(experience_category = case_when(HW03AE_1 == 0 & HW03AE_2 >= 1 & HW03AE_2 <=12 ~ 0,
                                                        is.na(HW03AE_1) & HW03AE_2 >= 1 & HW03AE_2 <=12 ~ 0,
                                                        HW03AE_1 == 1 ~ 1,
                                                        HW03AE_1 > 1 & HW03AE_1 < 5 ~ 2,
                                                        HW03AE_1 >= 5 & !is.na(HW03AE_1) ~ 3))

  f <- paste0("dat$experience_category <- haven::labelled(dat$experience_category, label = 'Time in post', labels = c('",label0,
              "' = 0, '",label1,"' = 1, '",label2,"' = 2, '",label3,"' = 3))")
  eval(rlang::parse_expr(f))

  # Create variable to combine questions HW03AH and HW03AI for requested table

  varlabel <- language_string(language_use = language_use, str = "OS_243", replaceq = TRUE)
  label1 <- language_string(language_use = language_use, str = "OS_244", replaceq = TRUE) # "Vaccination or VPD classes offered in last 12 months"
  label2 <- language_string(language_use = language_use, str = "OS_245", replaceq = TRUE) # "Non vaccination or VPD class offered in last 12 months"
  label3 <- language_string(language_use = language_use, str = "OS_246", replaceq = TRUE) # "No classes offered"

  dat <- dat %>% mutate(classes = ifelse(HW03AH %in% 1,1,NA))
  dat <- dat %>% mutate(classes = ifelse((classes == 1) & (HW03AI %in% 2),2,classes))
  dat <- dat %>% mutate(classes = ifelse(is.na(classes),3,classes))

  f <- paste0("dat$classes <- haven::labelled(dat$classes,label = '",varlabel,"', labels = c('",label1,
              "' = 1, '",label2,"' = 2, '",label3,"' = 3))")
  eval(rlang::parse_expr(f))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(HW_SURVEY_DATASET),"_dv.rds"))

  # Reset global to reflect new datasetname
  vcqi_global(HW_SURVEY_DATASET, paste0(tools::file_path_sans_ext(HW_SURVEY_DATASET),"_dv.rds"))

  # Comment following codes out as we don't use level2 for missvcqiR
  # if (vcqi_object_exists("LEVEL_2_ID")){
  #   # Create level2 variables so they are all inclusive of each ID
  #   # Make sure each has 3 characters
  #
  #   var <- c("B", "D", "F", "H")
  #   for(v in seq_along(var)){
  #     varchange <- rlang::sym(paste0("ID02A",var[v]))
  #     dat <- dat %>% mutate(tempvar = !!varchange*100)
  #     dat$tempvar <- haven::labelled(dat$tempvar,
  #                                    label = paste0("Variable ID02A",var[v]," multiplied by 100 to get a three digit code"))
  #     names(dat)[which(names(dat) == "tempvar")] <- paste0("l2_ID02A",var[v])
  #   } # end of var v loop
  #
  #   dat <- dat %>% mutate(level2_ID02AB = as.character(l2_ID02AB) + "000000000") %>%
  #     mutate(level2_ID02AD = as.character(l2_ID02AB) + as.character(l2_ID02AD) + "000000") %>%
  #     mutate(level2_ID02AF = as.character(l2_ID02AB) + as.character(l2_ID02AD) + as.character(l2_ID02AF) + "000") %>%
  #     mutate(level2_ID02AH = as.character(l2_ID02AB) + as.character(l2_ID02AD) + as.character(l2_ID02AF) + as.character(l2_ID02AH))
  #
  #   for(v in seq_along(var)){
  #     varchange <- rlang::sym(paste0("level2_ID02A",var[v]))
  #     dat <- dat %>% mutate(!!varchange := as.numeric(!!varchange))
  #   }
  #
  #   dat$level2_ID02AD <- haven::labelled(dat$level2_ID02AD, label ="State and Municipality code")
  #   dat$level2_ID02AF <- haven::labelled(dat$level2_ID02AF, label ="State, Municipality, and District code")
  #   dat$level2_ID02AH <- haven::labelled(dat$level2_ID02AH, label ="State, Municipality, District and Region code")
  #
  #   # Create level2name variables
  #   dat <- dat %>% mutate(level2_ID02ABname = ID02AA) %>%
  #     mutate(level2_ID02ADname = paste0(level2_ID02ABname , " " , ID02AC)) %>%
  #     mutate(level2_ID02AFname = paste0(level2_ID02ADname , " " , ID02AE)) %>%
  #     mutate(level2_ID02AHname = paste0(level2_ID02AFname + " " + ID02AG))
  #
  #   #TODO update the labels once confirmed with Dale and MK
  #   dat$level2_ID02ABname <- haven::labelled(dat$level2_ID02ABname, label = "Level 2 name variable equal to ID02AA")
  #   dat$level2_ID02ADname <- haven::labelled(dat$level2_ID02ADname, label = "Level 2 name variable equal to  and ID02AC")
  #   dat$level2_ID02AFname <- haven::labelled(dat$level2_ID02AFname, label = "Level 2 name variable equal to ID02AC and ID02AE")
  #   dat$level2_ID02AHname <- haven::labelled(dat$level2_ID02AHname, label = "Level 2 name variable equal to ID02AC ID02AE and ID02AG")
  #
  #   dat <- dat %>% arrange(ID02AB) %>%
  #     group_by(ID02AB) %>%
  #     mutate(level2_ID02ABsmall = cur_group_id()) %>%
  #     ungroup()
  #
  #   dat <- dat %>% arrange(ID02AB,ID02AD) %>%
  #     group_by(ID02AB,ID02AD) %>%
  #     mutate(level2_ID02ADsmall = cur_group_id()) %>%
  #     ungroup()
  #
  #   dat <- dat %>% arrange(ID02AB,ID02AD,ID02AF) %>%
  #     group_by(ID02AB,ID02AD,ID02AF) %>%
  #     mutate(level2_ID02AFsmall = cur_group_id()) %>%
  #     ungroup()
  #
  #   dat <- dat %>% arrange(ID02AB,ID02AD,ID02AF,ID02AH) %>%
  #     group_by(ID02AB,ID02AD,ID02AF,ID02AH) %>%
  #     mutate(level2_ID02AHsmall = cur_group_id()) %>%
  #     ungroup()
  #
  #   #TODO update the labels once confirmed with Dale and MK
  #   dat$level2_ID02ABsmall <- haven::labelled(dat$level2_ID02ABsmall, label = "Level 2 variable equal to ID02AB")
  #   dat$level2_ID02ADsmall <- haven::labelled(dat$level2_ID02ADsmall, label = "Level 2 variable made by grouping  and ID02AD")
  #   dat$level2_ID02AFsmall <- haven::labelled(dat$level2_ID02AFsmall, label = "Level 2 variable made by grouping ID02AD and ID02AF")
  #   dat$level2_ID02AHsmall <- haven::labelled(dat$level2_ID02AHsmall, label = "Level 2 variable made by grouping ID02AD ID02AF and ID02AH")
  #
  #   saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/level2file.rds"))
  #
  #   if (LEVEL_2_ID %in% c("ID02AB","ID02AD","ID02AF","ID02AH")){
  #     dat <- dat %>% select(all_of(contains(LEVEL_2_ID)))
  #     names(dat)[which(names(dat) == paste0("level2_",LEVEL_2_ID,"small"))] <- "level2id"
  #     names(dat)[which(names(dat) == paste0("level2_",LEVEL_2_ID,"name"))] <- "level2name"
  #     dat <- dat %>% select(c(level2id,level2name)) %>% distinct()
  #     dat <- dat %>% arrange(level2id)
  #     saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/level2names.rds"))
  #
  #     dat <- dat %>% select(-c(level2name)) %>% arrange(level2id) %>% mutate(level2order = 1:n())
  #     saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/level2order.rds"))
  #
  #     vcqi_global(LEVEL2_ORDER_DATASET,paste0(VCQI_OUTPUT_FOLDER,"/level2order.rds"))
  #     vcqi_global(LEVEL2_NAME_DATASET,paste0(VCQI_OUTPUT_FOLDER,"/level2names.rds"))
  #   }
  #
  #   # Pulled from VCQI Code
  #   dat <- vcqi_read(file = paste0(VCQI_OUTPUT_FOLDER,"/level2file.rds"))
  #   dat <- dat %>% select(c(all_of(LEVEL_3_ID),all_of(paste0("level2_",LEVEL_2_ID,"small")))) %>% distinct()
  #
  #   level2name <- vcqi_read(LEVEL2_NAME_DATASET)
  #
  #   dat <- left_join(dat, level2name, by = "level2id") %>%
  #     rename(level2nameforlevel3 = level2name)
  #
  #   rm(level2name)
  #
  #   saveRDS(dat,file = paste0(VCQI_OUTPUT_FOLDER,"/level2namesforlevel3.rds"))
  #   if (!vcqi_object_exists("RI_TEMP_DATASETS")){
  #     RI_TEMP_DATASETS <- NULL
  #   }
  #   vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "level2namesforlevel3.rds"))
  #
  #   # If the user has specified a level2id listed below, then update CM
  #
  #   if (LEVEL_2_ID %in% c("ID02AB","ID02AD","ID02AF","ID02AH")){
  #     dat <- dat %>% rename(ID02AIid = level3id, province_id = level2id) %>%
  #       select(c(ID02AIid,province_id))
  #
  #     dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))
  #     if ("province_id" %in% names(dat2)){
  #       dat2 <- dat2 %>% select(-c(province_id))
  #     }
  #
  #     dat2 <- left_join(dat2, dat, by = "ID02AIid")
  #     saveRDS(dat2, file = paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))
  #
  #   }
  # }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
