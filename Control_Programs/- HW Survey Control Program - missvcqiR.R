# User's Guide HW Control Program R version 1.00 - Biostat Global Consulting - 2023-08-31
#
# Missed Opportunities Vaccination Coverage Quality Indicators (MISS-VCQI)
# control program to analyze data from a health worker survey
#
# Change log
#
# Date          Version Number    Name          What Changed
# 2023-08-31    1.00              BGC           Original R Version
#
# This program is configured to analyze the VCQI demonstration datasets from a
# fictional health worker survey.  It serves as a template that users may copy
# to use with new datasets from real surveys.
#
# Written by Biostat Global Consulting

# *************************************************
# Code Block: HW-A             (Do not change) ----
#
# Load the VCQI package
library(missvcqiR, attach.required = TRUE)

# Start with clear memory
cleanup_MISS_VCQI_globals()

# *************************************************
# Code Block: HW-B           (User may change) ----

# Specify input/output folders and analysis name

# Where should the programs look for datasets?
VCQI_DATA_FOLDER <- "C:/Users/clary/Biostat Global Dropbox/Caitlin Clary/CBC GitHub Repos/missvcqiR/Demo_Datasets/HW"

# Where should the programs put output?
VCQI_OUTPUT_FOLDER <- "Q:/PAHO - MISS VCQI in R/Working folder - Cait/HW Test"

# Establish analysis name (used in log file name and Excel file name)
VCQI_ANALYSIS_NAME <- "HW_Test"

# Set VCQI_CHECK_INSTEAD_OF_RUN value to 1 to test all metadata and code that
# makes datasets and calculates derived variables, without running the
# indicators or generating output.
# Note: checks are not fully implemented and tested in the R version of MISS-VCQI
VCQI_CHECK_INSTEAD_OF_RUN <- 0

# *************************************************
# Code Block: HW-C             (Do not change) ----
#
# ** CD to output folder and open VCQI log

setwd(VCQI_OUTPUT_FOLDER)

# Start with a clean, empty Excel file for tabulated output (TO)
unlink(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_ANALYSIS_NAME, "_TO.xlsx"), force = TRUE)

# Give the current program a name, for logging purposes
VCP <- "HW_Survey_Control_Program"

# Open the VCQI log and put a comment in it
vcqi_log_comment(VCP, 3, "Comment", "Run begins...log opened...")

# Document the global macros that were defined before the log opened
vcqi_log_global(VCQI_DATA_FOLDER)
vcqi_log_global(VCQI_OUTPUT_FOLDER)
vcqi_log_global(VCQI_ANALYSIS_NAME)

# Write an entry in the log file documenting the missvcqiR package version
vcqi_log_comment(
  VCP, 3, "Package",
  paste0("missvcqiR package version ", utils::packageVersion("missvcqiR")))

# *************************************************
# Code Block: HW-D           (User may change) ----
#
# Specify dataset names and important metadata
# Dataset names should include file extensions
# Accepted file types: .rds, .dta, .csv

# Name of dataset that holds HW data
vcqi_global(HW_SURVEY_DATASET, "HW_test_data.dta")

# Name of dataset that holds facility data
vcqi_global(VCQI_CM_DATASET, "CM_test_data.dta")

# NOTE: the two globals below will be used in the generate_hw_derived_variables
# program to create the level2namesforlevel3 dataset

# If you would like to use ID02AB, ID02AD, ID02AF and/or ID02AH as a level2
# stratifier, please list the variable name below. Only one level2 stratifier
# variable can be provided.
vcqi_global(LEVEL_2_ID, "ID02AD")

# Provide the name of the variable that uniquely identifies each facility,
# typically ID02AIid
vcqi_global(LEVEL_3_ID, "ID02AIid")

# Set language for output tables
# This can be set to ENGLISH, SPANISH, FRENCH or PORTUGUESE
# May also set to EN for ENGLISH
# May also set to ES for SPANISH
# May also set to FR for FRENCH
# May also set to PT for PORTUGUESE
vcqi_global(OUTPUT_LANGUAGE, "ENGLISH")

# .........................................................................
# Parameters to describe the analysis being requested
# .........................................................................

# Provide the names of the datasets that give the names of the various strata
# and list the order in which strata should appear in tabular output.

vcqi_global(LEVEL1_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level1name.dta"))

vcqi_global(LEVEL3_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level3names.dta"))
vcqi_global(LEVEL3_ORDER_DATASET, paste0(VCQI_DATA_FOLDER, "/level3order.dta"))

# LEVEL4 parameters

# The LEVEL4 parameters determine the geographic and/or demographic strata for
# which results are displayed in tabular output and plots. To use the R version
# of VCQI, there must be at least one variable listed in
# VCQI_LEVEL4_SET_VARLIST. The user may specify a single stratifier (like
# urban/rural) or a set of several stratifiers (like urban/rural and sex and
# household wealth).
#
# For example, setting vcqi_global(VCQI_LEVEL4_SET_VARLIST, c("level1name",
# "level3name")) will produce output for the level 1 stratum (overall/national)
# and each level 3 stratum (e.g. each state). If VCQI_LEVEL4_SET_VARLIST is
# populated and VCQI_LEVEL4_SET_LAYOUT is not defined, then VCQI will generate a
# default layout for tables and figures. That layout file will be saved in the
# VCQI_OUTPUT_FOLDER.
#
# The user may create their own VCQI_LEVEL4_SET_LAYOUT file defining the
# conditions, preferred order, and row labels for the LEVEL4 strata and point to
# that layout file in the control program, e.g.
# vcqi_global(VCQI_LEVEL4_SET_LAYOUT, "Q:/My_VCQI_Output/my_level4_layout.rds").
# See the VCQI User's Guide for more details on creating a layout file.

vcqi_global(VCQI_LEVEL4_SET_VARLIST, c("ID02AD", "level3id"))
vcqi_global(VCQI_LEVEL4_SET_LAYOUT, paste0(VCQI_DATA_FOLDER, "/level4_layout_02.dta"))

# Specify whether the code should export to excel, or not (usually 1)
vcqi_global(EXPORT_TO_EXCEL, 1)

# User specifies the number of digits after the decimal place in coverage outcomes
vcqi_global(VCQI_NUM_DECIMAL_DIGITS, 1)

# Specify whether the code should make plots, or not (usually 1)
# MAKE_PLOTS must be 1 for any plots to be made
vcqi_global(MAKE_PLOTS, 1)

# Set PLOT_OUTCOMES_IN_TABLE_ORDER to 1 if you want inchworm and unweighted
# plots to list strata in the same order as the tables; otherwise the strata
# will be sorted by the outcome and shown in bottom-to-top order of increasing
# indicator performance
vcqi_global(PLOT_OUTCOMES_IN_TABLE_ORDER, 0)

# Make unweighted sample proportion plots? Set to 1 for yes.
vcqi_global(VCQI_MAKE_UW_PLOTS, 1)
vcqi_global(IWPLOT_SHOWBARS, 1)
# Annotate text in the unweighted plot for small sample sizes? Set 1 for yes.
vcqi_global(UWPLOT_ANNOTATE_LOW_MED, 0)
# Add square brackets around N < UWPLOT_ANNOTATE_LOW_N; default is 25
vcqi_global(UWPLOT_ANNOTATE_LOW_N, NA)
# Add parentheses around N < UWPLOT_ANNOTATE_MED_N; default is 50
vcqi_global(UWPLOT_ANNOTATE_MED_N, NA)

# Save the data underlying unweighted plots? Set to 1 for yes. If this option is
# turned on, unweighted plot programs will save a dataset in the Plots_IW_UW
# folder that makes it possible to understand the quantitative details of each
# plot component and can be used to recreate the plot.
vcqi_global(VCQI_SAVE_UW_PLOT_DATA, 1)

# Specify whether the code should save VCQI output databases
#
# WARNING!! If this macro is set to 1, VCQI will delete ALL files that end in
# _database.rds in the VCQI_OUTPUT_FOLDER at the end of the run. If you want to
# save the databases, change the value to 0. (Usually 1)
vcqi_global(DELETE_VCQI_DATABASES_AT_END, 1)

# Specify whether the code should delete intermediate datasets
# at the end of the analysis (Usually 1)
# If you wish to keep them for additional analysis or debugging,
# set the option to 0.
vcqi_global(DELETE_TEMP_VCQI_DATASETS, 1)

# ********************************************************************************
# Code Block: HW-E                                               (Do not change)
#-------------------------------------------------------------------------------
#                  Format the VCQI dose list and pre-process survey data

#-------------------------------------------------------------------------------
# Run MISS-VCQI multi lingual global program
# --------------------------------------------------------------------------
# This will set globals for each output in Miss-VCQI based on the value provided
# in global OUTPUT_LANGUAGE
miss_vcqi_multi_lingual_strings()

# --------------------------------------------------------------------------
# Check the user's metadata for completeness and correctness
# --------------------------------------------------------------------------
check_HW_analysis_metadata()

# Calculate Knowledge, Attitude and Practice barriers per Annex 7 of The PAHO
# Methodology for the Evaluation of Missed Opportunities for Vaccination doc

# This program uses ${HW_SURVEY_DATASET} and saves a new dataset with derived
# variables: ${HW_SURVEY_DATASET}_dv
gen_hw_dv()

# --------------------------------------------------------------------------
# Establish unique IDs
# --------------------------------------------------------------------------

# NOTE: The name of the dataset coming out of the ID step is HW_with_ids
# NOTE: establish_unique_ids needs to be created

# This program uses dataset: ${HW_SURVEY_DATASET}_dv
# And saves all changes to dataset: HW_with_ids
# Then saved as dataset: RI_with_ids

establish_unique_HW_ids()

# If the user requests a check instead of a run, then turn off
# flags that result in databases, excel output, and plots

if (VCQI_CHECK_INSTEAD_OF_RUN == 1){
  vcqi_log_comment(VCP, 3, "Comment", "The user has requested a check instead of a run.")
  VCQI_PREPROCESS_DATA <- 0
  VCQI_GENERATE_DVS <- 0
  VCQI_GENERATE_DATABASES <- 0
  EXPORT_TO_EXCEL <- 0
  MAKE_PLOTS <- 0
}

# ********************************************************************************
#  Code Block: HW-F                                             (User may change)
# -------------------------------------------------------------------------------
#                   Calculate VCQI indicators requested by the user
# -------------------------------------------------------------------------------

# This is a counter that is used to name datasets...it is usually set to 1 but
# the user might change it if requesting repeat analyses with differing
# parameters - see the User's Guide

vcqi_global(ANALYSIS_COUNTER, 1)

# --------------------------------------------------------------------------
# Summarize responses to some multiple-choice questions using DESC_02
# --------------------------------------------------------------------------

vcqi_global(DESC_02_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "HW03AA")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(
  DESC_02_RELABEL_LABEL_1,
  language_string(language_use = language_use, str = "OS_91")) # Missing

# Health Worker Sex
vcqi_global(
  DESC_02_TO_TITLE,
  language_string(language_use = language_use, str = "OS_174"))

vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)

DESC_02_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
vcqi_global(DESC_02_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "age_group")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(
  DESC_02_RELABEL_LABEL_1,
  language_string(language_use = language_use, str = "OS_91")) # Missing

# Health Worker Age
vcqi_global(
  DESC_02_TO_TITLE,
  language_string(language_use = language_use, str = "OS_175"))

vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)

DESC_02_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
vcqi_global(DESC_02_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "HW03AC")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(
  DESC_02_RELABEL_LABEL_1,
  language_string(language_use = language_use, str = "OS_91")) # Missing

# Health Worker's Level of Professional Seniority
vcqi_global(
  DESC_02_TO_TITLE,
  language_string(language_use = language_use, str = "OS_176"))

vcqi_global(DESC_02_TO_SUBTITLE, NA)
# Based on question HW03AC: Professional Training
vcqi_global(
  DESC_02_TO_FOOTNOTE_4,
  language_string(language_use = language_use, str = "OS_177"))

DESC_02_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
vcqi_global(DESC_03_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_03_SHORT_TITLE, "area_work")
vcqi_global(DESC_03_VARIABLES, c("HW03AD_1","HW03AD_2"))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "ALL")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Health Worker Area of Work
vcqi_global(
  DESC_03_TO_TITLE,
  language_string(language_use = language_use, str = "OS_178"))

# Respondent can select more than one response so percentages can add up to more
# than 100 and the two N columns can sum up to more than the number of
# participants.
vcqi_global(
  DESC_03_TO_FOOTNOTE_4,
  language_string(language_use = language_use, str = "OS_179"))

DESC_03_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
vcqi_global(DESC_02_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "experience_category")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(
  DESC_02_RELABEL_LABEL_1,
  language_string(language_use = language_use, str = "OS_91")) # Missing

# Health Worker Years of Experience
vcqi_global(
  DESC_02_TO_TITLE,
  language_string(language_use = language_use, str = "OS_180"))

vcqi_global(DESC_02_TO_SUBTITLE, NA)
# Based on questions HW03AE_1 and HW03AE_2: Time in post
vcqi_global(
  DESC_02_TO_FOOTNOTE_4,
  language_string(language_use = language_use, str = "OS_181"))

DESC_02_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
vcqi_global(DESC_02_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "classes")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(
  DESC_02_RELABEL_LABEL_1,
  language_string(language_use = language_use, str = "OS_91")) # Missing

# Vaccination Classes Offered in Last 12 Months
vcqi_global(
  DESC_02_TO_TITLE,
  language_string(language_use = language_use, str = "OS_182"))
vcqi_global(DESC_02_TO_SUBTITLE, NA)
# Based on questions HW03AH and HW03AI
vcqi_global(
  DESC_02_TO_FOOTNOTE_4,
  language_string(language_use = language_use, str = "OS_183"))

DESC_02_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
vcqi_global(DESC_02_DATASET, "HW_with_ids.rds")
# This currently does not align with question in Box 6
# Need to confirm this is set up correctly
vcqi_global(DESC_02_VARIABLES, "HW03AG")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(
  DESC_02_RELABEL_LABEL_1,
  language_string(language_use = language_use, str = "OS_91")) # Missing

# Years Since Last Vaccination or VPD Training
# This will need to change if HW03AG changes
vcqi_global(
  DESC_02_TO_TITLE,
  language_string(language_use = language_use, str = "OS_184"))
vcqi_global(DESC_02_TO_SUBTITLE, NA)
# Based on questions HW03AG
vcqi_global(
  DESC_02_TO_FOOTNOTE_4,
  language_string(language_use = language_use, str = "OS_185"))

DESC_02_MV(cleanup = TRUE)

# --------------------------------------------------------------------------
# Calculate whether the workers had knowledge & attitude barriers

# Uses dataset: HW_with_ids
# Saves new dataset: HW_BAR_01_${ANALYSIS_COUNTER}

# Health Worker: Knowledge & Attitude Barriers
vcqi_global(HW_BAR_01_TO_TITLE,
            language_string(language_use = language_use, str = "OS_186"))
vcqi_global(HW_BAR_01_TO_SUBTITLE, NA)

# Note: This measure is an unweighted summary of a proportion from the survey
# sample.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_1,
            language_string(language_use = language_use, str = "OS_52"))

# Note: Individual respondents may appear in more than one column, so percent
# figure sum may be > 100%.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_2,
            language_string(language_use = language_use, str = "OS_232"))

# Note: Knowledge Barriers: A respondent is considered to have a knowledge
# barrier if they answered fewer than 16 questions correct in the following
# survey questions: HW04AA-HW04AD, HW04AE_1, HW04AE_2, HW04AE_3, HW04AE_4,
# HW04AE_5, HW04AF-HW04AI, HW04AJ_1, HW04AJ_2, HW04AJ_3, HW04AJ_4, HW04AJ_5,
# HW04AJ_6 and HW04AJ_7.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_3,
            language_string(language_use = language_use, str = "OS_187"))

# Note: Attitude Barriers: A respondent is considered to have an attitude
# barrier if they answered fewer than 4 questions correct in the following
# survey questions: HW05AA-HW05AD and HW05AH-HW05AI.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_4,
            language_string(language_use = language_use, str = "OS_188"))

# Note: No Barriers: Respondents did not have a knowledge or attitude barrier.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_5,
            language_string(language_use = language_use, str = "OS_189"))

# Note: Any Barriers: Respondents had a knowledge or attitude barrier or both
# attitude and knowledge barriers.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_6,
            language_string(language_use = language_use, str = "OS_190"))

# Note: Both Barriers: Respondents that had both a knowledge and an attitude
# barrier.
vcqi_global(HW_BAR_01_TO_FOOTNOTE_7,
            language_string(language_use = language_use, str = "OS_191"))

HW_BAR_01()

# --------------------------------------------------------------------------
# Calculate whether the workers engage in improper vx practices

# Uses dataset: HW_with_ids
# Saves new dataset: HW_PRAC_01_${ANALYSIS_COUNTER}

# Health Worker: Improper Vaccination Practices
vcqi_global(HW_PRAC_01_TO_TITLE,
            language_string(language_use = language_use, str = "OS_192"))
vcqi_global(HW_PRAC_01_TO_SUBTITLE, NA)

# Note: This measure is an unweighted summary of a proportion from the survey
# sample.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_1,
            language_string(language_use = language_use, str = "OS_52"))

# Note: Individual respondents may appear in more than one column, so percent
# figure sum may be > 100%.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_2,
            language_string(language_use = language_use, str = "OS_232"))

# Note: Improper Vx Practices – Part 1: A respondent is considered to have
# improper vaccination practices (part 1) if they answered fewer than 6
# questions correct in the following survey questions: HW06AA-HW06AC, HW06AD_1,
# HW06AD_2, HW06AD_3, HW06AD_4, HW06AD_5, HW06AD_6.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_3,
            language_string(language_use = language_use, str = "OS_193"))

# Note: Improper Vx Practices – Part 2: A respondent is considered to have
# improper vaccination practices (part 2) if they answered fewer than 8
# questions correct in the following survey questions: HW06AE-HW06AJ, HW06AK_1,
# HW06AK_2, HW06AK_3 and HW06AK_4.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_4,
            language_string(language_use = language_use, str = "OS_194"))

# Note: No Barriers: Respondents did not have any improper practice barrier.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_5,
            language_string(language_use = language_use, str = "OS_195"))

# Note: Improper Vx Practices – Any: Respondents had improper vaccination
# practices under part 1 or part 2 or both part 1 and part 2.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_6,
            language_string(language_use = language_use, str = "OS_196"))

#Note: Improper Vx Practices – Both: Respondents that had improper vaccination
#practices under both part 1 and part 2.
vcqi_global(HW_PRAC_01_TO_FOOTNOTE_7,
            language_string(language_use = language_use, str = "OS_197"))

HW_PRAC_01()

# --------------------------------------------------------------------------
vcqi_global(DESC_03_DATASET, "HW_with_ids.rds")
vcqi_global(DESC_03_SHORT_TITLE, "other_improper_vx_prac")
vcqi_global(DESC_03_VARIABLES, c("HW06AL", "HW06AM", "HW06AN", "HW06AO"))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "YES")

# Sufficient staff offering immunization services
vcqi_global(DESC_03_N_RELABEL_LEVELS, 4)
vcqi_global(DESC_03_RELABEL_LEVEL_1, "HW06AL")
vcqi_global(DESC_03_RELABEL_LABEL_1,
            language_string(language_use = language_use, str = "OS_233"))

# Enough vials of all vaccines [3]
vcqi_global(DESC_03_RELABEL_LEVEL_2, "HW06AM")
vcqi_global(DESC_03_RELABEL_LABEL_2,
            language_string(language_use = language_use, str = "OS_234"))

# Have all the syringes, pads or swabs, record sheets, vaccination cards, and
# other materials needed to vaccinate [3]
vcqi_global(DESC_03_RELABEL_LEVEL_3, "HW06AN")
vcqi_global(DESC_03_RELABEL_LABEL_3,
            language_string(language_use = language_use, str = "OS_235"))

# Health care professional is available to replace primary professional in
# charge of vaccination in their absence
vcqi_global(DESC_03_RELABEL_LEVEL_4, "HW06AO")
vcqi_global(DESC_03_RELABEL_LABEL_4,
            language_string(language_use = language_use, str = "OS_236"))

# Healthcare Worker's Views
vcqi_global(DESC_03_TO_TITLE,
            language_string(language_use = language_use, str = "OS_237"))
vcqi_global(DESC_02_TO_SUBTITLE, NA)

# Total number of respondents who administer vaccines
vcqi_global(DESC_03_N_LABEL,
            language_string(language_use = language_use, str = "OS_238"))

# Note: Asked workers in regards to all patients who seek immunization services
vcqi_global(DESC_03_TO_FOOTNOTE_4,
            language_string(language_use = language_use, str = "OS_239"))

DESC_03_MV(cleanup = TRUE)

# *************************************************
# Code Block: RI-G             (Do not change) ----
#
# Exit gracefully

# Close the datasets that hold the results of
# hypothesis tests, and put them into the output spreadsheet
#
# Close the log file and put it into the output spreadsheet
#
# Clean up extra files
#
# Send a message to the screen if there are warnings or errors in the log

miss_vcqi_cleanup()
