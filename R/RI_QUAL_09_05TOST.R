#'Export datasets to Excel for RI_QUAL_09
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr
#' @import dplyr

# RI_QUAL_09_05TOST R version 1.00 - Biostat Global Consulting - 2023-07-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-24  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_09_05TOST <- function(VCP = "RI_QUAL_09_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_QUAL_09", "TO_RI_QUAL_09_columnlabel", "TO_RI_QUAL_09_formatnum", "TO_RI_QUAL_09_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()

  vc <- str_to_lower(RI_QUAL_09_VALID_OR_CRUDE)

  suff <- c(MOV_OUTPUT_DOSE_LIST,"anydose")

  for (d in seq_along(suff)){

    print(suff[d])

    dose <- suff[d]

    # Do some cleanup work for each dose

    # establish the local macros ldose and udose to use in situations that
    # require either lower case or upper case in this program

    ldose <- str_to_lower(dose)
    udose <- str_to_upper(dose)

    if (dose == "anydose") {
      ldose <- "anydose"
      udose <- "Any Dose"
    }

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_database.rds"))

    # calculate the three percent figures

    dat <- dat %>% mutate(pct_mov = ifelse((n_eligible > 0) %in% TRUE, (n_mov/n_eligible) * 100, NA),
                          pct_uncor = ifelse((n_mov > 0) %in% TRUE, (n_uncor_mov/n_mov) * 100, NA),
                          pct_cor = ifelse((n_mov > 0) %in% TRUE, (n_cor_mov/n_mov) * 100, NA))

    dat <- dat %>% mutate(pct_mov = ifelse(is.na(pct_mov) & !is.na(n_eligible), 0 , pct_mov),
                          pct_uncor = ifelse(is.na(pct_uncor) & !is.na(n_eligible), 0 , pct_uncor),
                          pct_cor = ifelse(is.na(pct_cor) & !is.na(n_eligible), 0 , pct_cor))

    # calculate the number and pct who had some but not all MOVs
    # corrected if we're building a table for all doses

    if (dose == "anydose"){
      dat <- dat %>% mutate(n_partial = n_mov - n_uncor_mov - n_cor_mov,
                            pct_partial = ifelse((n_mov > 0) %in% TRUE, (n_partial/n_mov) * 100, NA))
      dat <- dat %>% mutate(pct_partial = ifelse(is.na(pct_partial), 0, pct_partial))
    }

    # generate a new 0/1 flag that indicates which rows in the output
    # are showing results for sub-strata defined by level 4
    dat <- dat %>% mutate(substratum = ifelse(!is.na(level4id),1,0))

    names(dat)[which(names(dat) == "n_eligible")] <- "n"

    saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"))

    if (!vcqi_object_exists("RI_QUAL_09_TEMP_DATASETS")){
      RI_QUAL_09_TEMP_DATASETS <- NULL
    }

    vcqi_global(RI_QUAL_09_TEMP_DATASETS,c(RI_QUAL_09_TEMP_DATASETS,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds")))

    if (dose != "anydose") {
      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_mov", replacevar = NA, noannotate = TRUE,
        label = paste0("Had MOV for ", udose, " (N)"), varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_mov", replacevar = NA, noannotate = TRUE,
        label = paste0("Had MOV for ", udose, " (%)"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_uncor_mov", replacevar = NA, noannotate = TRUE,
        label = paste0("MOV uncorrected for ", udose, " (N)"), varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_uncor", replacevar = NA, noannotate = TRUE,
        label = paste0("MOV uncorrected for ", udose, " (%)"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_cor_mov", replacevar = NA, noannotate = TRUE,
        label = paste0("MOV corrected for ", udose, " (N)"), varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_cor", replacevar = NA, noannotate = TRUE,
        label = paste0("MOV corrected for ", udose, " (%)"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n", replacevar = NA, noannotate = TRUE,
        label = paste0("Had visits eligible ", udose, " (N)"), varformat = list("#,##0"))
    }

    if (dose == "anydose") {
      print("Totals...")

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_mov", replacevar = NA, noannotate = TRUE,
        label = "Had MOV for any dose (N)", varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_mov", replacevar = NA, noannotate = TRUE,
        label = "Had MOV for any dose (%)")

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_uncor_mov", replacevar = NA, noannotate = TRUE,
        label = "All MOVs were uncorrected (N)", varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_uncor", replacevar = NA, noannotate = TRUE,
        label = "All MOVs were uncorrected (%)")

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_cor_mov", replacevar = NA, noannotate = TRUE,
        label = "All MOVs were corrected (N)", varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_cor", replacevar = NA, noannotate = TRUE,
        label = "All MOVs were corrected (%)")

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n_partial", replacevar = NA, noannotate = TRUE,
        label = "Some (not all) MOVs were corrected (N)", varformat = list("#,##0"))

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "pct_partial", replacevar = NA, noannotate = TRUE,
        label = "Some (not all) MOVs were corrected (%)")

      make_table_column(
        tablename = "TO_RI_QUAL_09",
        dbfilename = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",ldose,"_TO.rds"),
        variable = "n", replacevar = NA, noannotate = TRUE,
        label = "Had visits eligible for any dose (N)", varformat = list("#,##0"))
    }

  } #end of d loop

  export_table_to_excel(indicator = "RI_QUAL_09",brief = FALSE)

  rm(list = c("TO_RI_QUAL_09", "TO_RI_QUAL_09_columnlabel", "TO_RI_QUAL_09_formatnum", "TO_RI_QUAL_09_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_RI_QUAL_09_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

