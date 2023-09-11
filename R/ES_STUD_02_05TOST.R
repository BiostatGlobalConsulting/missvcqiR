#' Export datasets to Excel for ES_STUD_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# ES_STUD_02_05TOST R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_02_05TOST <- function(VCP = "ES_STUD_02_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_ES_STUD_02", "TO_ES_STUD_02_columnlabel", "TO_ES_STUD_02_formatnum","TO_ES_STUD_02_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()

  vc <- str_to_lower(ES_STUD_02_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)
  vcf <- substr(vc,1,1)

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])

    make_table_column(
      tablename = "TO_ES_STUD_02",
      dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_valid_",vcf,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_61"),
                     " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," ",
                     language_string(language_use = language_use, str = "OS_1"))) #Received valid dose for `=upper("`d'")' (%)

    make_table_column(
      tablename = "TO_ES_STUD_02",
      dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_mov_",vcf,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_23"),
                     " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," ",
                     language_string(language_use = language_use, str = "OS_1"))) #Experienced MOV for `=upper("`d'")' (%)

    make_table_column(
      tablename = "TO_ES_STUD_02",
      dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_mov_",vcf,"_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_53"),
                     " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," ",
                     language_string(language_use = language_use, str = "OS_55"))) #Number of children eligible for `=upper("`d'")' on the study day
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  print("Totals...")

  # Any invalid
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_any_valid_",vcf,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_57"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Received 1+ valid doses (N)
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_any_valid_",vcf,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_57"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Received 1+ valid doses (%)
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_any_valid_",vcf,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_83")) #Was eligible for 1+ doses (N)

  # Any MOV
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_any_mov_",vcf,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_20"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Experienced 1+ MOVs (N)
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_any_mov_",vcf,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_20"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Experienced 1+ MOVs (%)
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_any_mov_",vcf,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_83")) #Was eligible for 1+ doses (N)

  # Total MOV
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_total_movs_",vcf,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_17"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Eligible doses not administered (N)
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_total_movs_",vcf,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_17"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Eligible doses not administered (%)
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_total_movs_",vcf,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_75")) #Total doses for which children with cards were eligible (N)

  # N
  make_table_column(
    tablename = "TO_ES_STUD_02",
    dbfilename = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_had_card_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_13")) #Children with cards (N)

  export_table_to_excel(indicator = "ES_STUD_02",
                        sheet = paste0("ES_STUD_02 ",ANALYSIS_COUNTER," - ",
                                       language_string(language_use = language_use, str = "OS_70"),"5"), #Table5
                        brief = FALSE)

  rm(list = c("TO_ES_STUD_02", "TO_ES_STUD_02_columnlabel", "TO_ES_STUD_02_formatnum","TO_ES_STUD_02_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_ES_STUD_02_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
