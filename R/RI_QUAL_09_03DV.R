#' Calculate derived variables for RI_QUAL_09
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_09_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr
#' @import haven

# RI_QUAL_09_03DV R version 1.03 - Biostat Global Consulting - 2023-07-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-29  1.00      Mia Yu          Original R version
# 2022-10-13  1.01      Mia Yu          Package version
# 2022-10-18  1.02      Mia Yu          Add variable labels
# 2023-07-21  1.03      Caitlin Clary   Update labels (MOV -> MOSV)
# *******************************************************************************

RI_QUAL_09_03DV <- function(VCP = "RI_QUAL_09_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(RI_QUAL_09_VALID_OR_CRUDE)

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,".rds"))

  dat <- dat %>% mutate(tempvar1 = 0, tempvar2 = 0, tempvar3 = 0)

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)) {
    print(MOV_OUTPUT_DOSE_LIST[d])

    flaghad <- rlang::sym(paste0("flag_had_mov_", MOV_OUTPUT_DOSE_LIST[d],"_",vc))
    totalelig <- rlang::sym(paste0("total_elig_",MOV_OUTPUT_DOSE_LIST[d],"_",vc))

    flaguncor <- rlang::sym(paste0("flag_uncor_mov_", MOV_OUTPUT_DOSE_LIST[d],"_",vc))
    flagcor <- rlang::sym(paste0("flag_cor_mov_", MOV_OUTPUT_DOSE_LIST[d],"_",vc))
    childhad <- rlang::sym(paste0("child_had_mov_", MOV_OUTPUT_DOSE_LIST[d],"_",vc))

    dat <- dat %>% mutate(tempvar4 = ifelse((!is.na(!!totalelig) & !!totalelig > 0) %in% TRUE,
                                            !!flaghad, NA_real_)) %>%
      mutate(tempvar5 = ifelse(tempvar4 %in% 1, !!flaguncor, NA_real_),
             tempvar6 = ifelse(tempvar4 %in% 1, !!flagcor, NA_real_))

    dat <- dat %>% mutate(tempvar1 = ifelse(tempvar4 %in% 1, tempvar1 + 1, tempvar1),
                          tempvar2 = ifelse(tempvar5 %in% 1, tempvar2 + 1, tempvar2),
                          tempvar3 = ifelse(tempvar6 %in% 1, tempvar3 + 1, tempvar3))

    dat$tempvar4 <- haven::labelled(dat$tempvar4, label = paste0("Child had a MOSV on dose ", MOV_OUTPUT_DOSE_LIST[d])) %>% suppressWarnings()
    dat$tempvar5 <- haven::labelled(dat$tempvar5, label = paste0("Child had an uncorrected MOSV on dose ", MOV_OUTPUT_DOSE_LIST[d])) %>% suppressWarnings()
    dat$tempvar6 <- haven::labelled(dat$tempvar6, label = paste0("Child had a corrected MOSV on dose ", MOV_OUTPUT_DOSE_LIST[d])) %>% suppressWarnings()

    names(dat)[which(names(dat) == "tempvar4")] <- paste0("child_had_mov_", MOV_OUTPUT_DOSE_LIST[d], "_", vc)
    names(dat)[which(names(dat) == "tempvar5")] <- paste0("child_had_uncor_mov_", MOV_OUTPUT_DOSE_LIST[d], "_", vc)
    names(dat)[which(names(dat) == "tempvar6")] <- paste0("child_had_cor_mov_", MOV_OUTPUT_DOSE_LIST[d], "_", vc)

  } #end of d loop

  print("Totals...")

  totalelig <- rlang::sym(paste0("total_elig_visits_", vc))

  dat <- dat %>% mutate(tempvar7 = ifelse((tempvar1 > 0) %in% TRUE,1,0),
                        tempvar8 = ifelse((tempvar1 > 0 & tempvar3 %in% 0) %in% TRUE,1,0),
                        tempvar9 = ifelse((tempvar1 > 0 & tempvar2 %in% 0) %in% TRUE,1,0),
                        tempvar10 = ifelse((tempvar2 > 0 & tempvar3 > 0) %in% TRUE,1,0))

  dat <- dat %>% mutate(tempvar7 = ifelse((is.na(!!totalelig) | !!totalelig %in% 0), NA, tempvar7),
                        tempvar8 = ifelse((is.na(!!totalelig) | !!totalelig %in% 0), NA, tempvar8),
                        tempvar9 = ifelse((is.na(!!totalelig) | !!totalelig %in% 0), NA, tempvar9),
                        tempvar10 = ifelse((is.na(!!totalelig) | !!totalelig %in% 0), NA, tempvar10))

  dat$tempvar1 <- haven::labelled(dat$tempvar1, label = "Number of doses with MOSVs") %>% suppressWarnings()
  dat$tempvar2 <- haven::labelled(dat$tempvar2, label = "Number of uncorrected MOSVs") %>% suppressWarnings()
  dat$tempvar3 <- haven::labelled(dat$tempvar3, label = "Number of corrected MOSVs") %>% suppressWarnings()
  # had 1+ MOSVs over all doses
  dat$tempvar7 <- haven::labelled(dat$tempvar7, label = "Child had 1+ MOSVs over all doses") %>% suppressWarnings()
  # had only uncorrected MOSVs
  dat$tempvar8 <- haven::labelled(dat$tempvar8, label = "Child only had uncorrected MOSVs") %>% suppressWarnings()
  # had only corrected MOSVs
  dat$tempvar9 <- haven::labelled(dat$tempvar9, label = "Child only had corrected MOSVs") %>% suppressWarnings()
  # had both uncorrected and corrected MOSVs
  dat$tempvar10 <- haven::labelled(dat$tempvar10, label = "Child had both corrected and uncorrected MOSVs") %>% suppressWarnings()

  names(dat)[which(names(dat) == "tempvar1")] <- paste0("doses_with_mov_",vc)
  names(dat)[which(names(dat) == "tempvar2")] <- paste0("doses_with_uncor_mov_",vc)
  names(dat)[which(names(dat) == "tempvar3")] <- paste0("doses_with_cor_mov_",vc)
  names(dat)[which(names(dat) == "tempvar7")] <- paste0("child_had_mov_",vc)
  names(dat)[which(names(dat) == "tempvar8")] <- paste0("child_had_only_uncor_mov_",vc)
  names(dat)[which(names(dat) == "tempvar9")] <- paste0("child_had_only_cor_mov_",vc)
  names(dat)[which(names(dat) == "tempvar10")] <- paste0("child_had_cor_n_uncor_mov_",vc)

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_09_", ANALYSIS_COUNTER, ".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

