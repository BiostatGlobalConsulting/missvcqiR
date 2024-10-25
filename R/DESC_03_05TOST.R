#' Export datasets to Excel for DESC_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# DESC_03_05TOST R version 1.01 - Biostat Global Consulting - 2024-03-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-06-06  1.00      Mia Yu          Original R package version
# 2023-08-21  1.01      Mia Yu          Trim the sheet name if too long
# 2023-09-29  1.02      Mia Yu          Added multi lingual globals
# 2024-03-18  1.03      Mia Yu          Update the sheet name length max from 30 to 31
# *******************************************************************************


DESC_03_05TOST <- function(VCP = "DESC_03_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (str_to_upper(DESC_03_WEIGHTED) == "YES"){
    vcqi_global(DESC_03_TO_FOOTNOTE_1,language_string(language_use = language_use, str = "OS_344"))
                #Abbreviations: CI=Confidence Interval
    vcqi_global(DESC_03_TO_FOOTNOTE_2,language_string(language_use = language_use, str = "OS_248"))
                #Respondents could select more than one response to this question.
    vcqi_global(DESC_03_TO_FOOTNOTE_3,language_string(language_use = language_use, str = "OS_51"))
                #Note: This measure is a population estimate that incorporates survey weights.
                # The CI is calculated with software that take the complex survey design into account.
  }

  if (str_to_upper(DESC_03_WEIGHTED) == "NO"){
    vcqi_global(DESC_03_TO_FOOTNOTE_1,language_string(language_use = language_use, str = "OS_52"))
                #Note: This measure is an unweighted summary of proportions from the survey sample.
    vcqi_global(DESC_03_TO_FOOTNOTE_2,language_string(language_use = language_use, str = "OS_248"))
                #Respondents could select more than one response to this question.
    if (str_to_upper(DESC_03_DENOMINATOR) == "ALL"){
      vcqi_global(DESC_03_TO_FOOTNOTE_3,language_string(language_use = language_use, str = "OS_16"))
                  #Denominator (N) is the total number of respondents.
    }
    if (str_to_upper(DESC_03_DENOMINATOR) == "RESPONDED"){
      vcqi_global(DESC_03_TO_FOOTNOTE_3,language_string(language_use = language_use, str = "OS_15"))
                  #Denominator (N) is limited to respondents who answered the question.
    }
  }

  vid <- DESC_03_COUNTER

  set_back_to_blank <- 0
  if (!vcqi_object_exists("DESC_03_TO_TITLE")){set_back_to_blank <- 1}

  zpc <- DESC_03_COUNTER
  if (DESC_03_COUNTER < 10){
    zpc <- paste0("0", zpc) # 0 pad the desc_03 counter
  }

  rm(list = c("TO_DESC_03","TO_DESC_03_columnlabel","TO_DESC_03_formatnum","TO_DESC_03_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds"))

  # If table title isn't specified, use variable label (default set in GO)
  if (set_back_to_blank %in% 1){
    if (!is.null(attributes(dat$outcome)$label)){
      vcqi_global(DESC_03_TO_TITLE, attributes(dat$outcome)$label)
    }
  }

  if ("nwtd" %in% names(dat)){
    wtd <- 1
  } else {
    wtd <- 0
  }

  # We need to tweak the database first
  for (i in seq_along(DESC_03_VORDER)) {
    datname <- paste0(VCQI_OUTPUT_FOLDER,"/DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds")
    db <- paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database")
    format_desc0203_database(i = DESC_03_VORDER[i], db = db,
                             wtd = wtd, indicator = "DESC_03", datname = datname, vid = vid)
  } # end of DESC_03_VORDER i loop

  for (i in seq_along(DESC_03_VORDER)) {
    if (vcqi_object_exists("DESC_03_LIST_N_BEFORE_PCT")){
      if (str_to_upper(DESC_03_LIST_N_BEFORE_PCT) == "YES"){
        ni <- get(paste0("n",DESC_03_VORDER[i]), envir = .GlobalEnv)
        make_table_column(
          tablename = "TO_DESC_03",
          dbfilename = paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_03_VORDER[i],".rds"),
          variable = "n", replacevar = NA, noannotate = TRUE,
          label = ni)
      }
    }

    if (vcqi_object_exists("DESC_03_LIST_NWTD_BEFORE_PCT")){
      if (wtd == 1 & str_to_upper(DESC_03_LIST_NWTD_BEFORE_PCT) == "YES"){
        nwtdi <- get(paste0("nwtd",DESC_03_VORDER[i]), envir = .GlobalEnv)
        make_table_column(
          tablename = "TO_DESC_03",
          dbfilename = paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_03_VORDER[i],".rds"),
          variable = "nwtd", replacevar = NA, noannotate = TRUE,
          label = nwtdi)
      }
    }

    pcti <- get(paste0("pct",DESC_03_VORDER[i]), envir = .GlobalEnv)
    make_table_column(
      tablename = "TO_DESC_03",
      dbfilename = paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_03_VORDER[i],".rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(pcti, " ", language_string(language_use = language_use, str = "OS_1"))) #(%)

    if (wtd == 1 & suppress_cis != 1){
      make_table_column(
        tablename = "TO_DESC_03",
        dbfilename = paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_03_VORDER[i],".rds"),
        variable = "ci", replacevar = NA, noannotate = TRUE,
        label = language_string(language_use = language_use, str = "OS_4")) #95% CI (%)
    }

    file.remove(paste0(VCQI_OUTPUT_FOLDER,"/DESC_03_",ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database_",DESC_03_VORDER[i],".rds"))
  } # end of DESC_03_VORDER i loop
  # Now we want to do it overall
  make_table_column(
    tablename = "TO_DESC_03",
    dbfilename = paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE, label = DESC_03_N_LABEL)

  if (wtd == 1){
    make_table_column(
      tablename = "TO_DESC_03",
      dbfilename = paste0("DESC_03_", ANALYSIS_COUNTER,"_", zpc,"_",vid, "_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = DESC_03_NWTD_LABEL)
  }

  sheetname <- paste0("DESC_03_",DESC_03_COUNTER,"_",DESC_03_SHORT_TITLE)
  sheet1 <- paste0(sheetname, " ", ANALYSIS_COUNTER)
  if(str_count(sheet1) > 31){
    e <- 31-str_count(paste0(" ", ANALYSIS_COUNTER))
    sheetname <- substr(sheetname, 1, e)
  }

  export_table_to_excel(indicator = "DESC_03",sheet = sheetname,brief = FALSE)

  rm(list = c("TO_DESC_03","TO_DESC_03_columnlabel","TO_DESC_03_formatnum","TO_DESC_03_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c(paste0("DESC_03_labels_",DESC_03_COUNTER)), envir = .GlobalEnv) %>% suppressWarnings()

  # If title global was blank, we used variable labels or names - return global
  # to blank before exiting
  if (set_back_to_blank %in% 1){
    vcqi_global(DESC_03_TO_TITLE, NA)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
