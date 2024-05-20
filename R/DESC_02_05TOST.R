#' Export datasets to Excel for DESC_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# DESC_02_05TOST R version 1.01 - Biostat Global Consulting - 2024-03-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-29  1.00      Mia Yu          Original R package version
# 2023-08-21  1.01      Mia Yu          Trim the sheet name if too long
# 2023-09-29  1.02      Mia Yu          Added multi lingual globals
# 2024-03-18  1.03      Mia Yu          Update the sheet name length max from 30 to 31
# *******************************************************************************


DESC_02_05TOST <- function(VCP = "DESC_02_05TOST"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (str_to_upper(DESC_02_WEIGHTED) == "YES"){
    vcqi_global(DESC_02_TO_FOOTNOTE_1,language_string(language_use = language_use, str = "OS_344"))
                #Abbreviations: CI=Confidence Interval
    vcqi_global(DESC_02_TO_FOOTNOTE_2,language_string(language_use = language_use, str = "OS_247"))
                #Respondents could only select one response to this question.
    vcqi_global(DESC_02_TO_FOOTNOTE_3, language_string(language_use = language_use, str = "OS_51"))
                #Note: This measure is a population estimate that incorporates survey weights.,
                # The CI is calculated with software that take the complex survey design into account.
  }

  if (str_to_upper(DESC_02_WEIGHTED) == "NO"){
    vcqi_global(DESC_02_TO_FOOTNOTE_1,language_string(language_use = language_use, str = "OS_52"))
                #Note: This measure is an unweighted summary of proportions from the survey sample.
    vcqi_global(DESC_02_TO_FOOTNOTE_2,language_string(language_use = language_use, str = "OS_247"))
                #Respondents could only select one response to this question.
    if (str_to_upper(DESC_02_DENOMINATOR) == "ALL"){
      vcqi_global(DESC_02_TO_FOOTNOTE_3,language_string(language_use = language_use, str = "OS_16"))
                  #Denominator (N) is the total number of respondents.
    }
    if (str_to_upper(DESC_02_DENOMINATOR) == "RESPONDED"){
      vcqi_global(DESC_02_TO_FOOTNOTE_3,language_string(language_use = language_use, str = "OS_15"))
                  #Denominator (N) is limited to respondents who answered the question.
    }
  }

  vid <- 1

  zpc <- DESC_02_COUNTER
  if (DESC_02_COUNTER < 10){
    zpc <- paste0("0",zpc) # 0 pad the desc_02 counter
  }

  for (d in seq_along(DESC_02_VARIABLES)){
    rm(list = c("TO_DESC_02","TO_DESC_02_columnlabel","TO_DESC_02_formatnum","TO_DESC_02_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds"))

    if ("nwtd" %in% names(dat)){
      wtd <- 1
    } else {
      wtd <- 0
    }

    # We need to tweak the database first
    for (i in seq_along(DESC_02_VORDER)) {
      datname <- paste0(VCQI_OUTPUT_FOLDER,"/DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds")
      db <- paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database")
			format_desc0203_database(i = DESC_02_VORDER[i], db = db,
			                         wtd = wtd, indicator = "DESC_02", datname = datname, vid = vid)
    } # end of DESC_02_VORDER i loop

    for (i in seq_along(DESC_02_VORDER)) {
      if (vcqi_object_exists("DESC_02_LIST_N_BEFORE_PCT")){
        if (str_to_upper(DESC_02_LIST_N_BEFORE_PCT) == "YES"){
          ni <- get(paste0("n",DESC_02_VORDER[i]), envir = .GlobalEnv)
          make_table_column(
            tablename = "TO_DESC_02",
            dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_02_VORDER[i],".rds"),
            variable = "n", replacevar = NA, noannotate = TRUE,
            label = ni)
        }
      }

      if (vcqi_object_exists("DESC_02_LIST_NWTD_BEFORE_PCT")){
        if (wtd == 1 & str_to_upper(DESC_02_LIST_NWTD_BEFORE_PCT) == "YES"){
          nwtdi <- get(paste0("nwtd",DESC_02_VORDER[i]), envir = .GlobalEnv)
          make_table_column(
            tablename = "TO_DESC_02",
            dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_02_VORDER[i],".rds"),
            variable = "nwtd", replacevar = NA, noannotate = TRUE,
            label = nwtdi)
        }
      }

      pcti <- get(paste0("pct",DESC_02_VORDER[i]), envir = .GlobalEnv)
      make_table_column(
        tablename = "TO_DESC_02",
        dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_02_VORDER[i],".rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(pcti, " ", language_string(language_use = language_use, str = "OS_1"))) #(%)

      if (wtd == 1 & suppress_cis != 1){
        make_table_column(
          tablename = "TO_DESC_02",
          dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_02_VORDER[i],".rds"),
          variable = "ci", replacevar = NA, noannotate = TRUE,
          label = language_string(language_use = language_use, str = "OS_4")) #95% CI (%)
      }

      file.remove(paste0(VCQI_OUTPUT_FOLDER,"/DESC_02_",ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_02_VORDER[i],".rds"))
    } # end of DESC_02_VORDER i loop

    # Now we want to do it overall
    make_table_column(
      tablename = "TO_DESC_02",
      dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = DESC_02_N_LABEL)

    if (wtd == 1){
      make_table_column(
        tablename = "TO_DESC_02",
        dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds"),
        variable = "nwtd", replacevar = NA, noannotate = TRUE, label = DESC_02_NWTD_LABEL)
    }

    sheetname <- paste0("DESC_02_",DESC_02_COUNTER,"_",DESC_02_VARIABLES[d])
    sheet1 <- paste0(sheetname, " ", ANALYSIS_COUNTER)
    if(str_count(sheet1) > 31){
      e <- 31-str_count(paste0(" ", ANALYSIS_COUNTER))
      sheetname <- substr(sheetname, 1, e)
    }

    export_table_to_excel(indicator = "DESC_02",sheet = sheetname,brief = FALSE)

    vid = vid + 1

    rm(list = c("TO_DESC_02","TO_DESC_02_columnlabel","TO_DESC_02_formatnum","TO_DESC_02_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  } # end of DESC_02_VARIABLES d loop

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
