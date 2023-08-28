#' Clean up MISS VCQI globals
#'
#' @return Remove VCQI global values from global environment
#' @export

# cleanup_MISS_VCQI_globals R version 1.0 - Biostat Global Consulting - 2023-08-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-14  1.00      Mia Yu   Original R version
# *******************************************************************************

cleanup_MISS_VCQI_globals <- function(){

  # Remove individual VCQI objects:
  rm(list = c(
    "ANALYSIS_COUNTER",
    "AGGREGATE_VCQI_DATABASES",
    "DELETE_TEMP_VCQI_DATASETS",
    "DELETE_VCQI_DATABASES_AT_END",
    "EXPORT_TO_EXCEL",
    "FORMAT_EXCEL",
    "IWPLOT_SHOWBARS",
    "IWPLOT_TYPE",
    "MAKE_EXCEL_COLUMNS_NARROW",
    "MAKE_PLOTS",
    "MOV_OUTPUT_DOSE_LIST",
    "PLOT_OUTCOMES_IN_TABLE_ORDER",
    "SORT_PLOT_LOW_TO_HIGH",
    "RI_TEMP_DATASETS",
    "VCP",
    "VCTC_globals_path",
    "VCQI_AGGREGATED_DATABASES",
    "VCQI_NON_AGGREGATED_DATABASES",
    "VCQI_DATABASES",
    "LEVEL_2_ID",
    "LEVEL_3_ID",
    "HW_SURVEY_DATASET",
    "dl",
    "i",
    "level4_layout",
    "vhi_log",
    "bold_left",
    "bold_right",
    "col_header",
    "italic_left",
    "italic_left_indented3",
    "italic_right",
    "regular_left",
    "regular_right",
    "table_footnote",
    "table_header",
    "table_subtitle",
    "table_title",
    "shaded_left",
    "shaded_right",
    "use_basic_fmtids",
    "suppress_cis",
    "TIMELY_ABBREV_CAPTION_LINE1",
    "TO_DESC_02_CN",
    "TO_DESC_03_CN",
    "TO_HW_PRAC_01_CN",
    "TO_HW_BAR_01_CN",
    "OUTPUT_LANGUAGE",
    "language_use"
  ), envir = .GlobalEnv) %>% suppressWarnings()

  # Remove VCQI objects by pattern:
  rm(list = ls(pattern = '^LEVEL.+_NAME_DATASET', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^LEVEL.+_ORDER_DATASET', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC_03_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^pct', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^n', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^nwtd', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^HW_PRAC_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^HW_BAR_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_08_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_09_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^VCQI', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '_TEMP_DATASETS$', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = 'UWPLOT_ANNOTATE_', envir = .GlobalEnv), envir = .GlobalEnv)

}
