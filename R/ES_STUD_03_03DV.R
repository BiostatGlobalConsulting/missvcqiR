#' Calculate derived variables for ES_STUD_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (ES_STUD_03_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr
#' @import haven

# ES_STUD_03_03DV R version 1.00 - Biostat Global Consulting - 2023-09-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-11  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_03_03DV <- function(VCP = "ES_STUD_03_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",ANALYSIS_COUNTER,".rds"))
  vc <- str_to_lower(ES_STUD_03_VALID_OR_CRUDE)

  # Create antigen list (only for antigens that appear in MOV_OUTPUT_DOSE_LIST)
  alist <- NULL
  vloop1 <- str_to_lower(RI_SINGLE_DOSE_LIST)
  vloop1 <- vloop1[-which(vloop1 == "visit")]

  for (v in seq_along(vloop1)){
    if (str_to_upper(vloop1[v]) %in% str_to_upper(MOV_OUTPUT_DOSE_LIST)){
      alist <- c(alist, vloop1[v])
    }
  } #end of vloop1 v loop

  vloop2 <- NULL
  for (i in 2:9) {
    if (vcqi_object_exists(paste0("RI_MULTI_", i, "_DOSE_LIST"))) {
      dl <- get(paste0("RI_MULTI_", i, "_DOSE_LIST"))
      if (!is.null(dl) & length(dl > 0)) {
        vloop2 <- c(vloop2, dl)
      }
    }
  } #end of i loop
  vloop2 <- str_to_lower(vloop2)

  alist2 <- NULL
  for (v in seq_along(vloop2)){
    v2 <- paste0(str_to_upper(vloop2[v]),2)
    if (v2 %in% str_to_upper(MOV_OUTPUT_DOSE_LIST)){
      alist2 <- c(alist2, vloop2[v])
    }
  }#end of vloop2 v loop

  alist <-c(alist,alist2)

  # Rename visit date so it is not part of the reshape
  dat <- dat %>% rename(study_date = visit_card_date)

  # Drop lines not included in summary
  dat <- dat %>% filter(!is.na(visitdate)) %>% filter(has_card_with_dob_and_dose %in% 1) #TODO: double check this logic

  # Create variable to show if the date is before or on study_day
  dat <- dat %>% mutate(study_day = ifelse((visitdate == study_date) %in% TRUE, 1, 0))
  dat$study_day <- haven::labelled(dat$study_day, label = "Date is equal to study date")

  # drop has_card and study_date vars
  dat <- dat %>% select(-c(has_card_with_dob_and_dose,study_date))

  # Generate total variables that will be populated later
  dat <- dat %>% mutate(tempvar1 = 0,
                        tempvar2 = 0,
                        tempvar3 = 0,
                        tempvar4 = 0,
                        tempvar5 = 0,
                        tempvar6 = 0)

  dat$tempvar1 <- haven::labelled(dat$tempvar1, label = "Total number of doses for which children were not eligible")
  dat$tempvar2 <- haven::labelled(dat$tempvar2, label = "Total number of doses for which children were eligible")
  dat$tempvar3 <- haven::labelled(dat$tempvar3, label = "Total number doses not received if not eligible - Correct")
  dat$tempvar4 <- haven::labelled(dat$tempvar4, label = "Total number doses received if not eligible - Invalid")
  dat$tempvar5 <- haven::labelled(dat$tempvar5, label = "Total number doses not received if eligible - MOV")
  dat$tempvar6 <- haven::labelled(dat$tempvar6, label = "Total number doses received if eligible - Valid")

  names(dat)[which(names(dat) == "tempvar1")] <- paste0("total_non_elig_doses_",vc)
  names(dat)[which(names(dat) == "tempvar2")] <- paste0("total_elig_doses_",vc)
  names(dat)[which(names(dat) == "tempvar3")] <- paste0("total_correct_nodose_",vc)
  names(dat)[which(names(dat) == "tempvar4")] <- paste0("total_invalid_",vc)
  names(dat)[which(names(dat) == "tempvar5")] <- paste0("total_mov_",vc)
  names(dat)[which(names(dat) == "tempvar6")] <- paste0("total_correct_validdose_",vc)

  # Create dose specific total variables that will be used for collapsing
  for (v in seq_along(alist)){
    dat <- dat %>% mutate(tempvar1 = 0, tempvar2 = 0)

    dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                    label = paste0(str_to_upper(alist[v]),
                                                   " - Number of doses for which children were not eligible"))
    dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                    label = paste0(str_to_upper(alist[v]),
                                                   " - Number of doses for which children were eligible"))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0(alist[v],"_non_elig_doses_",vc)
    names(dat)[which(names(dat) == "tempvar2")] <- paste0(alist[v],"_elig_doses_",vc)
  } #end of alist v loop

  # Create a single variable to show invalid, valid, mov for each antigen
  for (v in seq_along(alist2)){
    dat <- dat %>% mutate(tempvar1 = 0, tempvar2 = 0, tempvar3 = 0, tempvar4 = 0)

    dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                    label = paste0("Number ", alist2[v],
                                                   " not received if not eligible - Correct"))
    dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                    label = paste0("Number ", alist2[v],
                                                   " received if not eligible - Invalid"))
    dat$tempvar3 <- haven::labelled(dat$tempvar3,
                                    label = paste0("Number ", alist2[v],
                                                   " doses not received if eligible - MOV"))
    dat$tempvar4 <- haven::labelled(dat$tempvar4,
                                    label = paste0("Number ", alist2[v],
                                                   " doses received if eligible - Valid"))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("correct_nodose_",alist2[v],"_",vc)
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("invalid_",alist2[v],"_",vc)
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("mov_",alist2[v],"_",vc)
    names(dat)[which(names(dat) == "tempvar4")] <- paste0("correct_validdose_",alist2[v],"_",vc)

  } #end of alist2 v loop

  # Create local to pull the multi dose variables to be dropped later on
  mlist <- NULL

  for(i in seq_along(vloop2)){ #

    if (paste0(vloop2[i],2) %in% str_to_lower(MOV_OUTPUT_DOSE_LIST)){

      elig <- NULL
      correct <- NULL
      invalid <- NULL
      mov <- NULL
      valid <- NULL
      for (v in seq_along(vloop1)){
        if (grepl(vloop2[i], vloop1[v]) %in% TRUE){
          elig <- c(elig,paste0("elig_",vloop1[v], "_", vc))
          correct <- c(correct,paste0("correct_nodose_", vloop1[v], "_", vc))
          invalid <- c(invalid,paste0("invalid_", vloop1[v], "_", vc))
          mov <- c(mov,paste0("mov_", vloop1[v], "_", vc))
          valid <- c(valid,paste0("correct_validdose_", vloop1[v], "_", vc))
        }
      } #end of vloop1 v loop

      elig <- elig[which(elig %in% names(dat))]
      correct <- correct[which(correct %in% names(dat))]
      invalid <- invalid[which(invalid %in% names(dat))]
      mov <- mov[which(mov %in% names(dat))]
      valid <- valid[which(valid %in% names(dat))]

      sym_elig_dose <- rlang::sym(paste0(vloop2[i], "_elig_", vc))
      sym_correct_nodose <- rlang::sym(paste0("correct_nodose_", vloop2[i], "_", vc))
      sym_invalid_dose <- rlang::sym(paste0("invalid_", vloop2[i], "_", vc))
      sym_mov_dose <- rlang::sym(paste0("mov_", vloop2[i], "_", vc))
      sym_correct_validdose <- rlang::sym(paste0("correct_validdose_", vloop2[i], "_", vc))

      m_dose_elig <- dat %>%
        select(starts_with(paste0("elig_", vloop2[i]))) %>%
        select(ends_with(vc)) %>%
        select(-c(all_of(elig))) %>%
        mutate(tempx = rowSums(., na.rm = TRUE),
               temp = ifelse(tempx > 0 & !is.na(tempx), 1, 0)) %>%
        select(-tempx)

      m_correct_nodose <- dat %>%
        select(starts_with(paste0("correct_nodose_", vloop2[i]))) %>%
        select(ends_with(vc)) %>%
        select(-c(all_of(correct))) %>%
        mutate(
          tempx = rowSums(select(., -!!sym_correct_nodose), na.rm = TRUE),
          tempelig = m_dose_elig$temp,
          temp = ifelse(
            tempx > 0 & tempelig %in% 0, 1, !!sym_correct_nodose)
          ) %>% select(-tempx, -tempelig, -!!sym_correct_nodose)

      m_invalid_dose <- dat %>%
        select(starts_with(paste0("invalid_", vloop2[i]))) %>%
        select(ends_with(vc)) %>%
        select(-c(all_of(invalid))) %>%
        mutate(
          tempx = rowSums(select(., -!!sym_invalid_dose), na.rm = TRUE),
          tempelig = m_dose_elig$temp,
          temp = ifelse(
            tempx > 0 & tempelig %in% 0, 1, !!sym_invalid_dose)
        ) %>% select(-tempx, -tempelig, -!!sym_invalid_dose)

      m_mov_dose <- dat %>%
        select(starts_with(paste0("mov_", vloop2[i]))) %>%
        select(ends_with(vc)) %>%
        select(-c(all_of(mov))) %>%
        mutate(
          tempx = rowSums(select(., -!!sym_mov_dose), na.rm = TRUE),
          tempelig = m_dose_elig$temp,
          temp = ifelse(
            tempx > 0 & tempelig %in% 1, 1, !!sym_mov_dose)
        ) %>% select(-tempx, -tempelig, -!!sym_mov_dose)

      m_correct_validdose <- dat %>%
        select(starts_with(paste0("correct_validdose_", vloop2[i]))) %>%
        select(ends_with(vc)) %>%
        select(-c(all_of(valid))) %>%
        mutate(
          tempx = rowSums(select(., -!!sym_correct_validdose), na.rm = TRUE),
          tempelig = m_dose_elig$temp,
          temp = ifelse(
            tempx > 0 & tempelig %in% 1, 1, !!sym_correct_validdose)
        ) %>% select(-tempx, -tempelig, -!!sym_correct_validdose)

      dat <- dat %>%
        mutate(
          !!sym_elig_dose := m_dose_elig$temp,
          !!sym_correct_nodose := m_correct_nodose$temp,
          !!sym_invalid_dose := m_invalid_dose$temp,
          !!sym_mov_dose := m_mov_dose$temp,
          !!sym_correct_validdose := m_correct_validdose$temp,
        )

      mlist <- c(
        mlist,
        names(select(m_dose_elig, -temp)),
        names(select(m_correct_nodose,-temp)),
        names(select(m_invalid_dose, -temp)),
        names(select(m_mov_dose, -temp)),
        names(select(m_correct_validdose, -temp)))

    } # end if
  } # end i loop

  # rename single dose elig variable to align with the multi dose antigen name
  for (v in seq_along(vloop1)){
    names(dat)[which(names(dat) == paste0("elig_",vloop1[v],"_",vc))] <- paste0(vloop1[v],"_elig_",vc)
  } #end of vloop1 v loop

  for (v in seq_along(alist)){
    total_correct_no <- rlang::sym(paste0("total_correct_nodose_",vc))
    correct_nodose <- rlang::sym(paste0("correct_nodose_",alist[v],"_",vc))
    dat <- dat %>% mutate(!!total_correct_no := ifelse(!!correct_nodose %in% 1, !!total_correct_no + 1, !!total_correct_no))

    total_invalid <- rlang::sym(paste0("total_invalid_",vc))
    invalid <- rlang::sym(paste0("invalid_",alist[v],"_",vc))
    dat <- dat %>% mutate(!!total_invalid := ifelse(!!invalid %in% 1, !!total_invalid + 1, !!total_invalid))

    elig <- rlang::sym(paste0(alist[v],"_elig_",vc))
    non_elig <- rlang::sym(paste0(alist[v],"_non_elig_doses_",vc))
    dat <- dat %>% mutate(!!non_elig := ifelse(!!elig %in% 0, 1, !!non_elig))
    total_non_elig <- rlang::sym(paste0("total_non_elig_doses_",vc))
    dat <- dat %>% mutate(!!total_non_elig := ifelse(!!elig %in% 0, !!total_non_elig + 1, !!total_non_elig))

    total_mov <- rlang::sym(paste0("total_mov_",vc))
    mov <- rlang::sym(paste0("mov_",alist[v],"_",vc))
    dat <- dat %>% mutate(!!total_mov := ifelse(!!mov %in% 1, !!total_mov + 1, !!total_mov))

    total_correct_v <- rlang::sym(paste0("total_correct_validdose_",vc))
    correct_valid <- rlang::sym(paste0("correct_validdose_",alist[v],"_",vc))
    dat <- dat %>% mutate(!!total_correct_v := ifelse(!!correct_valid %in% 1, !!total_correct_v + 1, !!total_correct_v))

    elig_dose <- rlang::sym(paste0(alist[v],"_elig_doses_",vc))
    dat <- dat %>% mutate(!!elig_dose := ifelse(!!elig %in% 1, 1, !!elig_dose))
    total_elig <- rlang::sym(paste0("total_elig_doses_",vc))
    dat <- dat %>% mutate(!!total_elig := ifelse(!!elig %in% 1, !!total_elig + 1, !!total_elig))

  } #end of alist v loop

  # drop the individual dose variables for multi doses
  # And eligibility variables
  tempdat <- dat %>% select(all_of(ends_with(paste0("_elig_",vc))))
  droplist <- names(tempdat)
  droplist <- c(mlist,droplist)
  droplist <- unique(droplist)
  dat <- dat %>% select(-c(all_of(droplist)))

  # Create local to contain all the vars that we want to collapse by
  clist <- NULL
  for (v in seq_along(alist)){
    clist <- c(clist, paste0("correct_nodose_",alist[v],"_",vc),
               paste0("invalid_",alist[v],"_",vc),
               paste0("mov_",alist[v],"_",vc),
               paste0("correct_validdose_",alist[v],"_",vc),
               paste0(alist[v],"_elig_doses_",vc),
               paste0(alist[v],"_non_elig_doses_",vc))
  } #end of alist v loop
  clist <- c(clist, paste0("total_correct_nodose_",vc),paste0("total_invalid_",vc),
             paste0("total_mov_",vc),paste0("total_correct_validdose_",vc),
             paste0("total_elig_doses_",vc),paste0("total_non_elig_doses_",vc))

  dat$level1id <- haven::labelled(dat$level1id, label = "level1id")
  dat$level2id <- haven::labelled(dat$level2id, label = "level2id")
  dat$level3id <- haven::labelled(dat$level3id, label = "level3id")

  saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",ANALYSIS_COUNTER,"_pre_collapsed.rds"))

  #  Collapse so that it looks at each facility
  groupvar <- c("level1id","level2id","level3id","visitdate","study_day")
  for (v in seq_along(VCQI_LEVEL4_SET_VARLIST)){
    if (VCQI_LEVEL4_SET_VARLIST[v] %in% groupvar){
      groupvar <- groupvar[-which(groupvar == VCQI_LEVEL4_SET_VARLIST[v])]
    }
  }
  collapsedat <- dat %>% select(all_of(VCQI_LEVEL4_SET_VARLIST),level1id,level2id,level3id,visitdate,study_day,all_of(clist)) %>%
    group_by(across(c(all_of(VCQI_LEVEL4_SET_VARLIST),all_of(groupvar)))) %>%
    summarise(across(all_of(clist), sum))
  # save this file
  saveRDS(collapsedat, file = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",ANALYSIS_COUNTER,".rds"))
  # Add the temp dataset names
  if (!vcqi_object_exists("RI_TEMP_DATASETS")){
    RI_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_TEMP_DATASETS,
              c(RI_TEMP_DATASETS, paste0("ES_STUD_03_",ANALYSIS_COUNTER,".rds")))
  # Create study day dataset
  dat2 <- collapsedat %>% filter(study_day %in% 1)
  saveRDS(dat2, file = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_a_",ANALYSIS_COUNTER,".rds"))
  # Add the temp dataset names
  vcqi_global(RI_TEMP_DATASETS,
              c(RI_TEMP_DATASETS, paste0("ES_STUD_03_a_",ANALYSIS_COUNTER,".rds")))
  # Create before study day dataset
  dat3 <- collapsedat %>% filter(study_day %in% 0)
  saveRDS(dat3, file = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_b_",ANALYSIS_COUNTER,".rds"))
  # Add the temp dataset names
  vcqi_global(RI_TEMP_DATASETS,
              c(RI_TEMP_DATASETS, paste0("ES_STUD_03_b_",ANALYSIS_COUNTER,".rds")))

  # Now break apart the two datasets above so that it can look across all antigens and individual antigens
  string1 <- c("a","b")

  for(v in seq_along(string1)){
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",string1[v],"_",ANALYSIS_COUNTER,".rds"))
    dat4 <- dat %>% select(c(level1id,level2id,level3id,visitdate,
                             all_of(paste0("total_correct_nodose_",vc)),all_of(paste0("total_invalid_",vc)),
                             all_of(paste0("total_mov_",vc)),all_of(paste0("total_correct_validdose_",vc)),
                             all_of(paste0("total_elig_doses_",vc)),all_of(paste0("total_non_elig_doses_",vc)),
                             all_of(VCQI_LEVEL4_SET_VARLIST)))
    saveRDS(dat4,paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",string1[v],"_t_",ANALYSIS_COUNTER,".rds"))
    # Add the temp dataset names
    vcqi_global(RI_TEMP_DATASETS,
                c(RI_TEMP_DATASETS, paste0("ES_STUD_03_",string1[v],"_t_",ANALYSIS_COUNTER,".rds")))

    dat5 <- dat %>% select(-c(all_of(paste0("total_correct_nodose_",vc)),all_of(paste0("total_invalid_",vc)),
                              all_of(paste0("total_mov_",vc)),all_of(paste0("total_correct_validdose_",vc)),
                              all_of(paste0("total_elig_doses_",vc)),all_of(paste0("total_non_elig_doses_",vc))))
    saveRDS(dat5,paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_03_",string1[v],"_a_",ANALYSIS_COUNTER,".rds"))
    # Add the temp dataset names
    vcqi_global(RI_TEMP_DATASETS,
                c(RI_TEMP_DATASETS, paste0("ES_STUD_03_",string1[v],"_a_",ANALYSIS_COUNTER,".rds")))

  } #end of string1 v loop

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
