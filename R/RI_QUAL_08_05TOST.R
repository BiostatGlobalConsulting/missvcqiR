#' Export datasets to Excel for RI_QUAL_08
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_08_05TOST R version 1.00 - Biostat Global Consulting - 2023-07-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-24  1.00      Mia Yu          Original R package version
# 2024-08-29  1.01      Caitlin Clary   Update to use multilingual strings
# *******************************************************************************

RI_QUAL_08_05TOST <- function(VCP = "RI_QUAL_08_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_QUAL_08", "TO_RI_QUAL_08_columnlabel",
              "TO_RI_QUAL_08_formatnum", "TO_RI_QUAL_08_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])

    make_table_column(
      tablename = "TO_RI_QUAL_08",
      dbfilename = paste0("RI_QUAL_08_", ANALYSIS_COUNTER, "_",
                          MOV_OUTPUT_DOSE_LIST[d], "_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,

      # Visits with MOV for <dose> (%)
      label = paste0(language_string(language_use = language_use, str = "OS_81"),
                     " ",
                     str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
                     " ",
                     language_string(language_use = language_use, str = "OS_1"))
    )

    make_table_column(
      tablename = "TO_RI_QUAL_08",
      dbfilename = paste0("RI_QUAL_08_", ANALYSIS_COUNTER,"_",
                          MOV_OUTPUT_DOSE_LIST[d], "_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE,

      # N
      label = language_string(language_use = language_use, str = "OS_48"))
  } # end of d loop

  print("Totals...")

  make_table_column(
    tablename = "TO_RI_QUAL_08",
    dbfilename = paste0("RI_QUAL_08_", ANALYSIS_COUNTER, "_any_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,

    # Visits with MOV for any dose (%)
    label = paste0(language_string(language_use = language_use, str = "OS_82"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))
  )

  make_table_column(
    tablename = "TO_RI_QUAL_08",
    dbfilename = paste0("RI_QUAL_08_", ANALYSIS_COUNTER, "_any_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,

    # N
    label = language_string(language_use = language_use, str = "OS_48")
  )

  make_table_column(
    tablename = "TO_RI_QUAL_08",
    dbfilename = paste0("RI_QUAL_08_", ANALYSIS_COUNTER, "_rate_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE, noscale= TRUE,

    # MOVs per Visit
    label = language_string(language_use = language_use, str = "OS_47"),
    varformat = list("0.000"))

  make_table_column(
    tablename = "TO_RI_QUAL_08",
    dbfilename = paste0("RI_QUAL_08_", ANALYSIS_COUNTER, "_rate_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,

    # N
    label = language_string(language_use = language_use, str = "OS_48")
  )

  export_table_to_excel(indicator = "RI_QUAL_08",brief = FALSE)

  rm(list = c("TO_RI_QUAL_08", "TO_RI_QUAL_08_columnlabel",
              "TO_RI_QUAL_08_formatnum", "TO_RI_QUAL_08_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_RI_QUAL_08_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

