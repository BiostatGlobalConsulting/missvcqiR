#' Reads in the Exit survey dataset and creates the derived variables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @rawNamespace import(tools, except = makevars_user)
#' @import haven
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
  dat$caregiver_age <- haven::labelled(dat$caregiver_age, label = "Caregiver's Age grouped for tables",
                                       labels = c("<20" = 1, "20-29" = 2, "30-39" = 3, "40-49" = 4, "50+" = 5))

  # Create variable to group childs age
  dat <- dat %>% mutate(childs_age = NA)
  dat <- dat %>% mutate(childs_age = case_when(ES03AA_1_1 %in% 0 ~ 1, ES03AA_1_1 %in% 1 ~ 2, ES03AA_1_1 >=2 ~ 3))
  dat$childs_age <- haven::labelled(dat$childs_age, label = "Child's age grouped by month for table shells",
                                    labels = c("<1y" = 1, "1y" = 2, "2-5y" = 3))

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

  saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(VCQI_RI_DATASET),"_clean.rds"))


  if (vcqi_object_exists("LEVEL_2_ID")){
    # Create level2 variables so they are all inclusive of each ID
    # Make sure each has 3 characters

    var <- c("B", "D", "F", "H")
    for(v in seq_along(var)){
      varchange <- rlang::sym(paste0("ID02A",var[v]))
      dat <- dat %>% mutate(tempvar = !!varchange*100)
      dat$tempvar <- haven::labelled(dat$tempvar,
                                     label = paste0("Variable ID02A",var[v]," multiplied by 100 to get a three digit code"))
      names(dat)[which(names(dat) == "tempvar")] <- paste0("l2_ID02A",var[v])
    } # end of var v loop

    dat <- dat %>% mutate(level2_ID02AB = paste0(as.character(l2_ID02AB),"000000000")) %>%
      mutate(level2_ID02AD = paste0(as.character(l2_ID02AB),as.character(l2_ID02AD),"000000")) %>%
      mutate(level2_ID02AF = paste0(as.character(l2_ID02AB),as.character(l2_ID02AD),as.character(l2_ID02AF),"000")) %>%
      mutate(level2_ID02AH = paste0(as.character(l2_ID02AB),as.character(l2_ID02AD),as.character(l2_ID02AF),as.character(l2_ID02AH)))

    for(v in seq_along(var)){
      varchange <- rlang::sym(paste0("level2_ID02A",var[v]))
      dat <- dat %>% mutate(!!varchange := as.numeric(!!varchange))
    }

    dat$level2_ID02AB <- haven::labelled(dat$level2_ID02AD, label ="State code only")
    dat$level2_ID02AD <- haven::labelled(dat$level2_ID02AD, label ="State and Municipality code")
    dat$level2_ID02AF <- haven::labelled(dat$level2_ID02AF, label ="State, Municipality, and District code")
    dat$level2_ID02AH <- haven::labelled(dat$level2_ID02AH, label ="State, Municipality, District and Region code")

    # Create level2name variables
    dat <- dat %>% mutate(level2_ID02ABname = ID02AA) %>%
      mutate(level2_ID02ADname = paste0(level2_ID02ABname , " " , ID02AC)) %>%
      mutate(level2_ID02AFname = paste0(level2_ID02ADname , " " , ID02AE)) %>%
      mutate(level2_ID02AHname = paste0(level2_ID02AFname , " " , ID02AG))

    dat$level2_ID02ABname <- haven::labelled(dat$level2_ID02ABname, label = "Level 2 name variable equal to ID02AA")
    dat$level2_ID02ADname <- haven::labelled(dat$level2_ID02ADname, label = "Level 2 name variable equal to ID02AA and ID02AC")
    dat$level2_ID02AFname <- haven::labelled(dat$level2_ID02AFname, label = "Level 2 name variable equal to ID02AA ID02AC and ID02AE")
    dat$level2_ID02AHname <- haven::labelled(dat$level2_ID02AHname, label = "Level 2 name variable equal to ID02AA ID02AC ID02AE and ID02AG")

    dat <- dat %>% arrange(ID02AB) %>%
      group_by(ID02AB) %>%
      mutate(level2_ID02ABsmall = cur_group_id()) %>%
      ungroup()

    dat <- dat %>% arrange(ID02AB,ID02AD) %>%
      group_by(ID02AB,ID02AD) %>%
      mutate(level2_ID02ADsmall = cur_group_id()) %>%
      ungroup()

    dat <- dat %>% arrange(ID02AB,ID02AD,ID02AF) %>%
      group_by(ID02AB,ID02AD,ID02AF) %>%
      mutate(level2_ID02AFsmall = cur_group_id()) %>%
      ungroup()

    dat <- dat %>% arrange(ID02AB,ID02AD,ID02AF,ID02AH) %>%
      group_by(ID02AB,ID02AD,ID02AF,ID02AH) %>%
      mutate(level2_ID02AHsmall = cur_group_id()) %>%
      ungroup()

    dat$level2_ID02ABsmall <- haven::labelled(dat$level2_ID02ABsmall, label = "Level 2 variable equal to ID02AB")
    dat$level2_ID02ADsmall <- haven::labelled(dat$level2_ID02ADsmall, label = "Level 2 variable made by grouping ID02AB and ID02AD")
    dat$level2_ID02AFsmall <- haven::labelled(dat$level2_ID02AFsmall, label = "Level 2 variable made by grouping ID02AB ID02AD and ID02AF")
    dat$level2_ID02AHsmall <- haven::labelled(dat$level2_ID02AHsmall, label = "Level 2 variable made by grouping ID02AB ID02AD ID02AF and ID02AH")

    saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/",tools::file_path_sans_ext(VCQI_RI_DATASET),"_clean.rds"))

    if (LEVEL_2_ID %in% c("ID02AB","ID02AD","ID02AF","ID02AH")){
      dat1 <- dat %>% select(all_of(contains(LEVEL_2_ID)))
      names(dat1)[which(names(dat1) == paste0("level2_",LEVEL_2_ID,"small"))] <- "level2id"
      names(dat1)[which(names(dat1) == paste0("level2_",LEVEL_2_ID,"name"))] <- "level2name"
      dat1 <- dat1 %>% select(c(level2id,level2name)) %>% distinct()
      dat1 <- dat1 %>% arrange(level2id)
      saveRDS(dat1, file = paste0(VCQI_OUTPUT_FOLDER,"/level2names.rds"))

      dat2 <- dat1 %>% select(-c(level2name)) %>% arrange(level2id) %>% mutate(level2order = 1:n())
      saveRDS(dat2, file = paste0(VCQI_OUTPUT_FOLDER,"/level2order.rds"))

      vcqi_global(LEVEL2_ORDER_DATASET,paste0(VCQI_OUTPUT_FOLDER,"/level2order.rds"))
      vcqi_global(LEVEL2_NAME_DATASET,paste0(VCQI_OUTPUT_FOLDER,"/level2names.rds"))
    }

    # Pulled from VCQI Code
    dat <- dat %>% select(c(all_of(LEVEL_3_ID),all_of(paste0("level2_",LEVEL_2_ID)))) %>% distinct()

    names(dat)[which(names(dat) == LEVEL_3_ID)] <- "level3id"
    names(dat)[which(names(dat) == paste0("level2_",LEVEL_2_ID))] <- "level2id"

    level2name <- vcqi_read(LEVEL2_NAME_DATASET)

    dat <- left_join(dat, level2name, by = "level2id") %>%
      rename(level2nameforlevel3 = level2name)

    rm(level2name)

    saveRDS(dat,file = paste0(VCQI_OUTPUT_FOLDER,"/level2namesforlevel3.rds"))
    if (!vcqi_object_exists("RI_TEMP_DATASETS")){
      RI_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "level2namesforlevel3.rds"))

    # If the user has specified a level2id listed below, then update CM

    if (LEVEL_2_ID %in% c("ID02AB","ID02AD","ID02AF","ID02AH")){
      dat <- dat %>% rename(ID02AIid = level3id, province_id = level2id) %>%
        select(c(ID02AIid,province_id))

      dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))
      if ("province_id" %in% names(dat2)){
        dat2 <- dat2 %>% select(-c(province_id))
      }

      dat2 <- left_join(dat2, dat, by = "ID02AIid")
      haven::write_dta(dat2, path = paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))

    }
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
