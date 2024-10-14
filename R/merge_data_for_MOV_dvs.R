#' This program creates a dataset that will be used to create derived indicator variables for study date MOV tables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import tidyselect
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import haven

#' @return a dataset
#'
#' @export
#'
#' @examples
#' merge_data_for_MOV_dvs()

# merge_data_for_MOV_dvs R version 1.00 - Biostat Global Consulting - 2023-08-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-29  1.00      Mia Yu          Original R package version
# 2024-10-11  1.01      Caitlin Clary   If dose date count > 0, set ES06AA to 1.
#                                       (Moving forward, consider dropping req
#                                       that the card show a date on it.)
# *******************************************************************************

merge_data_for_MOV_dvs <- function(VCP = "merge_data_for_MOV_dvs"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dlist <- NULL
  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    dlist <- c(
      dlist,
      paste0("elig_", MOV_OUTPUT_DOSE_LIST[d], "_crude"),
      paste0("elig_", MOV_OUTPUT_DOSE_LIST[d] ,"_valid"),
      paste0("got_", MOV_OUTPUT_DOSE_LIST[d], "_crude"),
      paste0("got_", MOV_OUTPUT_DOSE_LIST[d], "_valid"),
      paste0("got_", MOV_OUTPUT_DOSE_LIST[d], "_tick"))
  } # end of MOV_OUTPUT_DOSE_LIST d loop

  # Merge RI_with_IDs & RI_MOV_step07
  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))
  dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_step07.rds")) %>%
    select(respid, got_visit, visitdate, all_of(dlist))

  dat <- left_join(dat, dat2, by = "respid", multiple = "all")

  # Generate variable "showed_card_with_dates"
  if ("card_date_count" %in% names(dat)){
    dat <- dat %>% select(-card_date_count)
  }

  dat <- dat %>% mutate(card_date_count = 0)
  dat$card_date_count <- haven::labelled(dat$card_date_count, label = "Number of Dates on Card")

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    carddate <- rlang::sym(paste0(MOV_OUTPUT_DOSE_LIST[d], "_card_date"))
    dat <- dat %>%
      mutate(
        card_date_count = ifelse(!is.na(!!carddate),
                                 card_date_count + 1, card_date_count)
      )
  } # end of MOV_OUTPUT_DOSE_LIST d loop

  dat <- dat %>%
    mutate(
      showed_card_with_dates = ifelse((card_date_count > 0) %in% TRUE, 1, 0),
      # 2024-10-11 update
      ES06AA = ifelse(card_date_count > 0, 1, ES06AA)
    )

  dat$showed_card_with_dates <- haven::labelled(
    dat$showed_card_with_dates,
    label = "Card Seen- Dates listed on Card")

  # Generate variable "has_card_with_dob_and_dose"
  dat <- dat %>%
    mutate(has_card_with_dob_and_dose = ifelse(
      !is.na(dob_for_valid_dose_calculations) & showed_card_with_dates %in% 1, 1, 0))

  dat$has_card_with_dob_and_dose <- haven::labelled(
    dat$has_card_with_dob_and_dose,
    label = "Child had card (valid dob & at least one dose date)")

  # Generate correct_nodose, invalid, mov, & correct_validdose variables by dose
  type <- c("valid", "crude")
  for (vc in seq_along(type)){
    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      elig <- rlang::sym(paste0("elig_", MOV_OUTPUT_DOSE_LIST[d], "_", type[vc]))
      got <- rlang::sym(paste0("got_", MOV_OUTPUT_DOSE_LIST[d], "_", type[vc]))
      tick <- rlang::sym(paste0("got_", MOV_OUTPUT_DOSE_LIST[d], "_tick"))

      # dat <- dat %>% mutate(tempvar1 = NA) %>%
      #   mutate(tempvar1 = ifelse(has_card_with_dob_and_dose %in% 1, 0, tempvar1)) %>%
      #   mutate(tempvar1 = ifelse((has_card_with_dob_and_dose == 1 & !!elig == 0 & !!got == 0) %in% TRUE, 1, tempvar1))

      # dat <- dat %>% mutate(tempvar2 = NA) %>%
      #   mutate(tempvar2 = ifelse(has_card_with_dob_and_dose %in% 1, 0, tempvar2)) %>%
      #   mutate(tempvar2 = ifelse((has_card_with_dob_and_dose == 1 & !!elig == 0 & !!got == 1) %in% TRUE, 1, tempvar2))

      # dat <- dat %>% mutate(tempvar3 = NA) %>%
      #   mutate(tempvar3 = ifelse(has_card_with_dob_and_dose %in% 1, 0, tempvar3)) %>%
      #   mutate(tempvar3 = ifelse((has_card_with_dob_and_dose == 1 & !!elig == 1 & !!got == 0 & !!tick == 0) %in% TRUE, 1, tempvar3))

      # dat <- dat %>% mutate(tempvar4 = NA) %>%
      #   mutate(tempvar4 = ifelse(has_card_with_dob_and_dose %in% 1, 0, tempvar4)) %>%
      #   mutate(tempvar4 = ifelse((has_card_with_dob_and_dose == 1 & !!elig == 1 & !!got == 1) %in% TRUE, 1, tempvar4))

      dat <- dat %>%
        mutate(
          tempvar_init = ifelse(has_card_with_dob_and_dose %in% 1, 0, NA),
          tempvar1 = ifelse(
            (has_card_with_dob_and_dose == 1 & !!elig == 0 & !!got == 0) %in% TRUE,
            1, tempvar_init),
          tempvar2 = ifelse(
            (has_card_with_dob_and_dose == 1 & !!elig == 0 & !!got == 1) %in% TRUE,
            1, tempvar_init),
          tempvar3 = ifelse(
            (has_card_with_dob_and_dose == 1 & !!elig == 1 & !!got == 0 & !!tick == 0) %in% TRUE,
            1, tempvar_init),
         tempvar4 = ifelse(
            (has_card_with_dob_and_dose == 1 & !!elig == 1 & !!got == 1) %in% TRUE,
            1, tempvar_init)
        ) %>%
        select(-tempvar_init)

      dat$tempvar1 <- haven::labelled(
        dat$tempvar1, label = "Correct no dose: Not eligible & did not receive it")
      dat$tempvar2 <- haven::labelled(
        dat$tempvar2, label = "Invalid dose: Not eligible & rec'd it")
      dat$tempvar3 <- haven::labelled(
        dat$tempvar3, label = "MOV dose: Eligible & did not receive it")
      dat$tempvar4 <- haven::labelled(
        dat$tempvar4, label = "Correct valid dose: Eligible & rec'd it")

      names(dat)[which(names(dat) == "tempvar1")] <- paste0(
        "correct_nodose_", MOV_OUTPUT_DOSE_LIST[d], "_", type[vc])
      names(dat)[which(names(dat) == "tempvar2")] <- paste0(
        "invalid_", MOV_OUTPUT_DOSE_LIST[d] ,"_", type[vc])
      names(dat)[which(names(dat) == "tempvar3")] <- paste0(
        "mov_", MOV_OUTPUT_DOSE_LIST[d], "_", type[vc])
      names(dat)[which(names(dat) == "tempvar4")] <- paste0(
        "correct_validdose_", MOV_OUTPUT_DOSE_LIST[d], "_", type[vc])

    } # end of MOV_OUTPUT_DOSE_LIST d loop
  } # end of type vc loop

  keep_dose_card_date_list <- NULL

  doselist <- c(MOV_OUTPUT_DOSE_LIST,"visit")
  for (d in seq_along(doselist)){
    keep_dose_card_date_list <- c(keep_dose_card_date_list, paste0(doselist[d],"_card_date"))
  }

  dat <- dat %>%
    select(
      ES01AA, ID02AB, ID02AD, ID02AF, ID02AH, ID02AIid,
      all_of(VCQI_LEVEL4_SET_VARLIST), all_of(dlist),
      dob_for_valid_dose_calculations, ES06AA, all_of(keep_dose_card_date_list),
      all_of(starts_with("ES08AA")),
      level1id, level2id, level3id, stratumid, clusterid, respid,
      HH02, HH04, psweight, got_visit, visitdate,
      card_date_count, showed_card_with_dates, has_card_with_dob_and_dose,
      all_of(starts_with("correct_nodose_")),
      all_of(starts_with("invalid_")),
      all_of(starts_with("mov_")),
      all_of(starts_with("correct_validdose_")))

  saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_by_visit_dose_category.rds"))
  if (!vcqi_object_exists("RI_TEMP_DATASETS")){
    RI_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_by_visit_dose_category"))

  # Subset data to only the visit dates
  dat <- dat %>% filter(got_visit %in% 1) %>% select(-got_visit)
  saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_merged_vars.rds"))
  vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_merged_vars"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
