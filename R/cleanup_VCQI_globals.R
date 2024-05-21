#' Clean up VCQI globals
#'
#' @return Remove VCQI global values from global environment
#' @export

# cleanup_VCQI_globals R version 1.0 - Biostat Global Consulting - 2022-08-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-19  1.00      Caitlin Clary   Original R version
# *******************************************************************************

cleanup_VCQI_globals <- function(){

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
    "NUM_DOSE_SHIFTS",
    "PLOT_OUTCOMES_IN_TABLE_ORDER",
    "SORT_PLOT_LOW_TO_HIGH",
    "RI_DOSE_LIST",
    "RI_RECORDS_NOT_SOUGHT",
    "RI_RECORDS_SOUGHT_FOR_ALL",
    "RI_RECORDS_SOUGHT_IF_NO_CARD",
    "RI_SINGLE_DOSE_LIST",
    "RI_TEMP_DATASETS",
    "RI_VCTC_01_LEVELS",
    "VCP",
    "VCTC_globals_path",
    "VCQI_AGGREGATED_DATABASES",
    "VCQI_NON_AGGREGATED_DATABASES",
    "VCQI_DATABASES",
    "dl",
    "i",
    "level4_layout",
    "vhi_log",
    "VCQI_DOB_PREFER_DOC",
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
    "TITLE_CUTOFF",
    "FOOTNOTE_CUTOFF"
  ), envir = .GlobalEnv) %>% suppressWarnings()

  # Remove VCQI objects by pattern:
  rm(list = ls(pattern = 'min_age_days$', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = 'max_age_days$', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = 'min_interval_days$', envir = .GlobalEnv), envir = .GlobalEnv)

  rm(list = ls(pattern = '^EARLIEST_SVY_VACC_DATE_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^LATEST_SVY_VACC_DATE_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^LEVEL.+_NAME_DATASET', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^LEVEL.+_ORDER_DATASET', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_MULTI_.+_DOSE_LIST$', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC_03_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^pct', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^n', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^nwtd', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_COVG_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_COVG_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_COVG_03_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_DOSES_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_COVG_04_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_CONT_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_CONT_01B_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_07B_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_08_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^RI_QUAL_09_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = "SHIFTFROM_", envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = "SHIFTTO_", envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^VCQI', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '_TEMP_DATASETS$', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = 'UWPLOT_ANNOTATE_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^SHIFTWITHIN_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DROPDUP_', envir = .GlobalEnv), envir = .GlobalEnv)

  # VCTC globals
  rm(list = ls(pattern = '^TIMELY_BARWIDTH', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_DT', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_CD', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_CI', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_DOSE_ORDER', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_FULLY_VXD', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_HBR_LINE', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_LEGEND_ORDER', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_N_DOSES', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_N_DTS', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_NOT_VXD', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_TEXTBAR', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_XLABEL', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_XSCALE', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_PLOT', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_Y_COORDS', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TIMELY_YLABEL', envir = .GlobalEnv), envir = .GlobalEnv)
}
