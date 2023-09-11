#' Export datasets to Excel for ES_STUD_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# ES_STUD_01_05TOST R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_01_05TOST <- function(VCP = "ES_STUD_01_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_ES_STUD_01", "TO_ES_STUD_01_columnlabel", "TO_ES_STUD_01_formatnum","TO_ES_STUD_01_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()

  vc <- str_to_lower(ES_STUD_01_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)
  vcf <- substr(vc,1,1)

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])

    make_table_column(
      tablename = "TO_ES_STUD_01",
      dbfilename = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_invalid_",vcf,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_59"),
                     " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," ",
                     language_string(language_use = language_use, str = "OS_1"))) #Received invalid dose for `=upper("`d'")' (%)

    make_table_column(
      tablename = "TO_ES_STUD_01",
      dbfilename = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_invalid_",vcf,"_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE,
      label = language_string(language_use = language_use, str = "OS_48")) #N
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  print("Totals...")

  make_table_column(
    tablename = "TO_ES_STUD_01",
    dbfilename = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_any_invalid_",vcf,"_database.rds"),
    variable = "numerator", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_56"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2"))) #Received 1+ invalid doses (N)

  make_table_column(
    tablename = "TO_ES_STUD_01",
    dbfilename = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_any_invalid_",vcf,"_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_56"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #Received 1+ invalid doses (%)

  make_table_column(
    tablename = "TO_ES_STUD_01",
    dbfilename = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_any_invalid_",vcf,"_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_13")) #Children with cards (N)

  export_table_to_excel(indicator = "ES_STUD_01",
                        sheet = paste0("ES_STUD_01 ",ANALYSIS_COUNTER," - ",
                                       language_string(language_use = language_use, str = "OS_70"),"4"), #Table4
                        brief = FALSE)

  rm(list = c("TO_ES_STUD_01", "TO_ES_STUD_01_columnlabel", "TO_ES_STUD_01_formatnum","TO_ES_STUD_01_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_ES_STUD_01_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
