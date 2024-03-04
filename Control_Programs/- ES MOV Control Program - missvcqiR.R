# User's Guide ES Control Program R version 1.00 - Biostat Global Consulting - 2023-10-02
#
# Missed Opportunities Vaccination Coverage Quality Indicators (MISS-VCQI)
# control program to analyze data from an MOV exit survey
#
# Change log
#
# Date          Version Number    Name          What Changed
# 2023-10-02    1.00              BGC           Original R Version
# 2023-11-28    1.01              BGC           Remove miss_vcqi_multi_lingual_strings from CP
#
# This program is configured to analyze the VCQI demonstration datasets from a
# fictional exit survey.  It serves as a template that users may copy to use
# with new datasets from real surveys.
#
# After copying the program, make a set of edits in Blocks ES-B and ES-D and
# ES-F below in accordance with guidance in the MISS-VCQI User's Guide.
#
# You will find the latest versions of VCQI documentation and information about
# VCQI programs at the VCQI Resources Website:
# http://www.biostatglobal.com/VCQI_RESOURCES.html
#
# Written by Biostat Global Consulting

# *************************************************
# Code Block: ES-A             (Do not change) ----
#
# Load the VCQI package
library(missvcqiR, attach.required = TRUE)

# Start with clear memory
cleanup_MISS_VCQI_globals()

# *************************************************
# Code Block: ES-B           (User may change) ----

# Specify input/output folders and analysis name

# Where should the programs look for datasets?
VCQI_DATA_FOLDER <- "C:/Users/clary/Biostat Global Dropbox/Caitlin Clary/CBC GitHub Repos/missvcqiR/Demo_Datasets/ES"

# Where should the programs put output?
VCQI_OUTPUT_FOLDER <- "Q:/PAHO - MISS VCQI in R/Working folder - Cait/ES Test"

# Establish analysis name (used in log file name and Excel file name)
VCQI_ANALYSIS_NAME <- "ES_Test"

# Set VCQI_CHECK_INSTEAD_OF_RUN value to 1 to test all metadata and code that
# makes datasets and calculates derived variables, without running the
# indicators or generating output
# Note: checks are not fully implemented and tested in the R version of MISS-VCQI
VCQI_CHECK_INSTEAD_OF_RUN <- 0

# *************************************************
# Code Block: ES-C             (Do not change) ----
#
# ** CD to output folder and open VCQI log

setwd(VCQI_OUTPUT_FOLDER)

# Start with a clean, empty Excel file for tabulated output (TO)
unlink(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_ANALYSIS_NAME, "_TO.xlsx"), force = TRUE)

# Give the current program a name, for logging purposes
VCP <- "RI_Control_Program"

# Open the VCQI log and put a comment in it
vcqi_log_comment(VCP, 3, "Comment", "Run begins...log opened...")

# Document the global macros that were defined before the log opened
vcqi_log_global(VCQI_DATA_FOLDER)
vcqi_log_global(VCQI_OUTPUT_FOLDER)
vcqi_log_global(VCQI_ANALYSIS_NAME)

# Write an entry in the log file documenting the missvcqiR package version
vcqi_log_comment(VCP, 3, "Package",
                 paste0("missvcqiR package version ", utils::packageVersion("missvcqiR")))

# *************************************************
# Code Block: ES-D           (User may change) ----
#
# Specify dataset names and important metadata
# Dataset names should include file extensions
# Accepted file types: .rds, .dta, .csv

# Name of datasets that hold RI data
vcqi_global(ES_SURVEY_DATASET, "ES New Var Names.dta")

# Name of dataset that holds cluster metadata
vcqi_global(VCQI_CM_DATASET, "CM_new_vars.dta")

# Set language for output tables
# This can be set to ENGLISH, SPANISH, FRENCH or PORTUGUESE
# May also set to EN for ENGLISH
# May also set to ES for SPANISH
# May also set to FR for FRENCH
# May also set to PT for PORTUGUESE
vcqi_global(OUTPUT_LANGUAGE, "ENGLISH")

# ..............................................................................
# Parameters to describe RI schedule
# ..............................................................................
# These parameters may change from survey to survey

#  See: http://apps.who.int/immunization_monitoring/globalsummary/schedules
#
#  Single-dose antigens will use a parameter named <dose>_min_age_days (required)
#  Single-dose antigens may use a parameter named <dose>_max_age_days (optional)
#  Note: If a dose is not considered valid AFTER a certain age, then specify
#        that maximum valid age using the _max_age_days parameter.
#        If the dose is considered late, but still valid, then do not specify
#        a maximum age.

bcg_min_age_days <- 0
hepb0_min_age_days <- 0

polio1_min_age_days <- 60
polio2_min_interval_days <- 28
polio2_min_age_days <- polio1_min_age_days + polio2_min_interval_days
polio3_min_interval_days <- 28
polio3_min_age_days <- polio2_min_age_days + polio3_min_interval_days

penta1_min_age_days <- 60
penta2_min_interval_days <- 28
penta2_min_age_days <- penta1_min_age_days + penta2_min_interval_days
penta3_min_interval_days <- 28
penta3_min_age_days <- penta2_min_age_days + penta3_min_interval_days

rota1_min_age_days <- 60
rota2_min_interval_days <- 28
rota2_min_age_days <- rota1_min_age_days + rota2_min_interval_days

pcv1_min_age_days <- 60
pcv2_min_interval_days <- 28
pcv2_min_age_days <- pcv1_min_age_days + pcv2_min_interval_days
pcv3_min_interval_days <- 28
pcv3_min_age_days <- pcv2_min_age_days + pcv3_min_interval_days

mmr1_min_age_days <- 365
mmr2_min_interval_days <- 270
mmr2_min_age_days <- mmr1_min_age_days + mmr2_min_interval_days

penta4_min_age_days <- 540
penta5_min_interval_days <- 181
penta5_min_age_days <- penta4_min_age_days + penta5_min_interval_days

polio4_min_age_days <- 540
polio5_min_interval_days <- 181
polio5_min_age_days <- polio4_min_age_days + polio5_min_interval_days

visit_min_age_days <- 7305  # 20 years old - made big so visit date never has MOV

# ..............................................................................
# Parameters to describe survey
# ..............................................................................
# Specify the earliest and latest possible vaccination date for this survey.

# The software assumes this survey includes birth doses, so the earliest date is
# the first possible birthdate for RI survey respondents and the latest date is
# the last possible vaccination date for this dataset - the latest date might be
# the date of the final survey interview.

vcqi_global(EARLIEST_SVY_VACC_DATE_M, 12)
vcqi_global(EARLIEST_SVY_VACC_DATE_D, 1)
vcqi_global(EARLIEST_SVY_VACC_DATE_Y, 2011)

vcqi_global(LATEST_SVY_VACC_DATE_M, 12)
vcqi_global(LATEST_SVY_VACC_DATE_D, 1)
vcqi_global(LATEST_SVY_VACC_DATE_Y, 2016)

# These parameters indicate the eligible age range for survey respondents
# (age expressed in days)

vcqi_global(VCQI_RI_MIN_AGE_OF_ELIGIBILITY, 0)
vcqi_global(VCQI_RI_MAX_AGE_OF_ELIGIBILITY, 1825)

# ..............................................................................
# Which doses should be included in the analysis?
# ..............................................................................

# Note that these abbreviations must correspond to those used in the names of
# the dose date and dose tick variables *AND* the names used above in the
# schedule globals (<dose>_min_age_days and <dose>_min_interval_days and
# <dose>_max_days.  The variables are named using lower-case acronyms.  The
# globals here may be upper or mixed case; they will be converted to lower case
# in the software.

vcqi_global(RI_SINGLE_DOSE_LIST, c("BCG", "HEPB0", "VISIT"))
vcqi_global(RI_MULTI_2_DOSE_LIST, c("MMR", "ROTA"))
vcqi_global(RI_MULTI_3_DOSE_LIST, c("PENTA", "PCV", "POLIO"))

# The R MISS-VCQI software can handle dose lists with up to 9 doses
# (RI_MULTI_9_DOSE_LIST). In this example we have single doses, two-dose series,
# and three-dose series, and we do not define the RI_MULTI_4_DOSE_LIST through
# RI_MULTI_9_DOSE_LIST globals.

# ..............................................................................
# Parameters to describe the analysis being requested
# ..............................................................................

vcqi_global(LEVEL2_ORDER_DATASET, paste0(VCQI_DATA_FOLDER, "/level2order.dta"))
vcqi_global(LEVEL3_ORDER_DATASET, paste0(VCQI_DATA_FOLDER, "/level3order.dta"))

vcqi_global(LEVEL1_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level1name.dta"))
vcqi_global(LEVEL2_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level2names.dta"))
vcqi_global(LEVEL3_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level3names.dta"))

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

vcqi_global(VCQI_LEVEL4_SET_VARLIST, c("ID02AD"))
vcqi_global(VCQI_LEVEL4_SET_LAYOUT, paste0(VCQI_DATA_FOLDER, "/level4_layout_ES_ID05.dta"))

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

# IWPLOT_SHOWBARS = 0 means show inchworm distributions
# IWPLOT_SHOWBARS = 1 means show horizontal bars instead of inchworms
# For the current version of R VCQI, please always set this to be 1
vcqi_global(IWPLOT_SHOWBARS, 1)

# Make unweighted sample proportion plots? Set to 1 for yes.
vcqi_global(VCQI_MAKE_UW_PLOTS, 1)

#Annotate text in the unweighted plot for small sample sizes? Set 1 for yes.
vcqi_global(UWPLOT_ANNOTATE_LOW_MED, 0)
#Add square brackets around N < UWPLOT_ANNOTATE_LOW_N; default is 25
vcqi_global(UWPLOT_ANNOTATE_LOW_N, NA)
#Add parentheses around N < UWPLOT_ANNOTATE_MED_N; default is 50
vcqi_global(UWPLOT_ANNOTATE_MED_N, NA)

# Save the data underlying unweighted plots? Set to 1 for yes.
# If this option is turned on, unweighted plot programs will save a dataset
# in the Plots_IW_UW folder that makes it possible to understand the quantitative
# details of each plot component and can be used to recreate the plot.
vcqi_global(VCQI_SAVE_UW_PLOT_DATA, 1)

# Specify whether the code should save VCQI output databases
#
# WARNING!! If this macro is set to 1, VCQI will delete ALL files that
# end in _database.rds in the VCQI_OUTPUT_FOLDER at the end of the run
# If you want to save the databases, change the value to 0.
# (Usually 1)
vcqi_global(DELETE_VCQI_DATABASES_AT_END, 1)

# Specify whether the code should delete intermediate datasets
# at the end of the analysis (Usually 1)
# If you wish to keep them for additional analysis or debugging,
# set the option to 0.
vcqi_global(DELETE_TEMP_VCQI_DATASETS, 1)

# For RI analysis, there is an optional report on data quality
# Set this global to 1 to generate that report
# It appears in its own separate Excel file.
vcqi_global(VCQI_REPORT_DATA_QUALITY, 0)

# Set this global to 1 if you would like to create an augmented dataset
# that merges survey dataset with derived variables calculated by VCQI.
# Default value is 0 (no)
vcqi_global(VCQI_MAKE_AUGMENTED_DATASET, 0)

# *************************************************
# Code Block: ES-E             (Do not change) ----

# Format the VCQI dose list and pre-process survey data

# Construct the global RI_DOSE_LIST from what the user specified above
# VCQI currently handles single-dose vaccines and multi-dose vaccines
# with up to nine doses in the series

# svyset syntax
vcqi_global(VCQI_SVYDESIGN_SYNTAX, list(ids = ~1))

# First, list single dose vaccines
if (vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
  RI_DOSE_LIST <- stringr::str_to_lower(RI_SINGLE_DOSE_LIST)
} else{
  RI_DOSE_LIST <- NULL
}

# Second, list doses in multi-dose lists
for(i in 2:9){
  if(vcqi_object_exists(paste0("RI_MULTI_", i, "_DOSE_LIST"))){
    dl <- get(paste0("RI_MULTI_", i, "_DOSE_LIST"))
    if(!is.null(dl) & length(dl > 0)){
      RI_DOSE_LIST <- c(RI_DOSE_LIST, paste0(rep(stringr::str_to_lower(dl), each = i), 1:i))}
  }
}

RI_DOSE_LIST_MINUS_VISIT <- RI_DOSE_LIST[-which(RI_DOSE_LIST == "visit")]

# Put a copy of the dose list in the log
vcqi_log_global(RI_DOSE_LIST)
vcqi_log_global(RI_DOSE_LIST_MINUS_VISIT)


# ..............................................................................
# Run MISS-VCQI multi lingual global program
# ..............................................................................

# ..............................................................................
# Run Exit Survey pre-processing program
# ..............................................................................

# This program will create all miss-VCQI required variables from the current dataset
# It will also look at the level2IDs provided and create the different level2 datasets
# This will also check that all required miss_vcqi ES globals are set

# This program uses dataset: ${ES_SURVEY_DATASET}
# And saves changes to dataset: ${ES_SURVEY_DATASET}_miss_vcqi_ready
pp_exit_survey()

# ..............................................................................
# Check the user's metadata for completeness and correctness
# ..............................................................................

# Starting RI_TEMP_DATASETS as an empty object before starting to record temp dataset list
RI_TEMP_DATASETS <- NULL

check_RI_schedule_metadata()
check_RI_survey_metadata()

# This program uses dataset: ${ES_SURVEY_DATASET}_miss_vcqi_ready
check_ES_analysis_metadata()

# Run the program to look at date of birth (from history, card, and register)
# and look at dates of vaccination from cards and register. This program
# evaluates each date and checks to see that it occurred in the period
# allowed for respondents eligible for this survey. It also checks to see
# that doses in a sequence were given in order. If any vaccination date
# seems to be outside the right range or recorded out of sequence, the date
# is stripped off and replaced with a simple yes/no tick mark. This step
# means less date-checking is necessary in subsequent programs.

# Uses dataset: ${ES_SURVEY_DATASET}_miss_vcqi_ready
# Saves as dataset: ${ES_SURVEY_DATASET}_miss_vcqi_ready_clean
cleanup_RI_dates_and_ticks_MV()

# Call program to create derived variables for table shells
# Uses "${VCQI_OUTPUT_FOLDER}/${ES_SURVEY_DATASET}_miss_vcqi_ready_clean"
# and saves changes to it

gen_es_dv()

# The dataset that comes out of the cleanup steps is named:
# "${VCQI_OUTPUT_FOLDER}/${ES_SURVEY_DATASET}_miss_vcqi_ready_clean"

# ..............................................................................
# Establish unique IDs
# ..............................................................................

# The name of the dataset coming out of the ID step is RI_with_ids
establish_unique_ES_ids()

# Calculate MOV flags for the pre study day visits; set aside
calculate_previsit_MOV_flags()

# Calculate MOV flags for ALL visits
vcqi_global(VCQI_TESTING_CODE, 1)

# Uses dataset: RI_with_ids
# Creates datasets for each step within MOV process...step01-08
calculate_MOV_flags()  # this program makes RI_MOV_step07, which is needed for merging in "gen_MOV_dvs"

# The following programs make derived indicator variables for MOV study date tables

# Merges RI_with_ids & a subset of RI_MOV_step07 variables (1:m)
# Creates additional variables and saves dataset: RI_MOV_by_visit_dose_category
# Keeps only got_visit==1 observations and saves dataset: RI_MOV_merged_vars
merge_data_for_MOV_dvs()

# Uses dataset: RI_MOV_merged_vars
# Creates all variables necessary for table shells and saves dataset: MOV_dvs
gen_MOV_dvs()

# If the user requests a check instead of a run, then turn off
# flags that result in databases, excel output, and plots

if(VCQI_CHECK_INSTEAD_OF_RUN == 1){
  vcqi_log_comment(VCP, 3, "Comment",
                   "The user has requested a check instead of a run.")
  VCQI_PREPROCESS_DATA <- 0
  VCQI_GENERATE_DVS <- 0
  VCQI_GENERATE_DATABASES <- 0
  EXPORT_TO_EXCEL <- 0
  MAKE_PLOTS <- 0
}

# *************************************************
# Code Block: ES-F           (User may change) ----

# Calculate VCQI indicators requested by the user

# This is a counter that is used to name datasets. It is usually set to 1 but
# the user might change it if requesting repeat analyses with differing
# parameters - see the User's Guide

vcqi_global(ANALYSIS_COUNTER, 1)

# Most indicators may be run in any order the user wishes, although there are
# are some restrictions...see the table in the section of Chapter 6 entitled
# Analysis Counter.

# ..............................................................................
# Summarize responses to some multiple-choice questions using DESC_02
# ..............................................................................

# Create table for Caregiver's Age
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "caregiver_age")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_85")) #Caregiver's Age
# No subtitle.
vcqi_global(DESC_02_TO_SUBTITLE, NA)
# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)
DESC_02(cleanup = TRUE)

# ..............................................................................
# Create table for Caregiver's Sex
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "ES04AB")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_86")) # Caregiver's Sex
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)
DESC_02(cleanup = TRUE)

# ..............................................................................
# Where does your child usually receive vaccinations?
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "ES04AG")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 3)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_RELABEL_LEVEL_2, 1)
vcqi_global(DESC_02_RELABEL_LABEL_2, language_string(language_use = language_use, str = "OS_92")) # None: Literate

vcqi_global(DESC_02_RELABEL_LEVEL_3, 2)
vcqi_global(DESC_02_RELABEL_LABEL_3, language_string(language_use = language_use, str = "OS_93")) # None: Illiterate

# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_87")) # Caregiver's Education
vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)
DESC_02(cleanup = TRUE)

# ..............................................................................
# Create table for Caregiver's Occupation
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "ES04AH")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_94")) # Caregiver's Occupation
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)
DESC_02(cleanup = TRUE)

# ..............................................................................
# Create table for Child's Age
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "childs_age")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_95")) # Child's Age in years
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)
#Note: Age groupings based on variable ES03AA_1_1
vcqi_global(DESC_02_TO_FOOTNOTE_4, paste0(language_string(language_use = language_use, str = "OS_95"), " ES03AA_1_1."))
DESC_02(cleanup = TRUE)

# ..............................................................................
# Create table for Child's Sex
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "ES03AB")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_97")) # Child's Sex
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)
DESC_02(cleanup = TRUE)

# ..............................................................................
# Create table for Vaccination Data
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "ES06ABSRC")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # No Vx Data

vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_98")) # Children with Vaccination Data
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)
vcqi_global(DESC_02_TO_FOOTNOTE_4, NA)
DESC_02(cleanup = TRUE)

# ..............................................................................
# Where does your child usually receive vaccinations?
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, "ES04AK")
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 4)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

vcqi_global(DESC_02_RELABEL_LEVEL_2, 1)
vcqi_global(DESC_02_RELABEL_LABEL_2, language_string(language_use = language_use, str = "OS_102")) # Live in Municipality

vcqi_global(DESC_02_RELABEL_LEVEL_3, 2)
vcqi_global(DESC_02_RELABEL_LABEL_3, language_string(language_use = language_use, str = "OS_103")) # Live outside Municipality

vcqi_global(DESC_02_RELABEL_LEVEL_4, 99)
vcqi_global(DESC_02_RELABEL_LABEL_4, language_string(language_use = language_use, str = "OS_104")) # Do Not Know

# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_100")) # Child's Place of Residence
vcqi_global(DESC_02_TO_SUBTITLE, NA)
# Looks at question ES04AK and identifies if the child lives within the same municipality as the facility per caregiver's response
vcqi_global(DESC_02_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_101"))
DESC_02(cleanup = TRUE)

# ..............................................................................
# Run program to create coverage categories
assign_covg_category()

# Create table for Proportion of Coverage
vcqi_global(DESC_02_COVG_CRUDE_OR_VALID, "crude")
vcqi_global(DESC_02_DATASET, "ES_with_ids.rds")
vcqi_global(DESC_02_VARIABLES, paste0("covg_category_", str_to_lower(DESC_02_COVG_CRUDE_OR_VALID)))
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_02_N_RELABEL_LEVELS, 1)
vcqi_global(DESC_02_RELABEL_LEVEL_1, NA)
vcqi_global(DESC_02_RELABEL_LABEL_1, language_string(language_use = language_use, str = "OS_91")) # Missing

#Proportion of Vaccinated, Unvaccinated and Undervaccinated
vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_105"))
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)

if (stringr::str_to_upper(DESC_02_COVG_CRUDE_OR_VALID) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(DESC_02_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(DESC_02_COVG_CRUDE_OR_VALID) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(DESC_02_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_106"))
}

# Note: This table takes into account all eligible vaccinations.
vcqi_global(DESC_02_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_107"))
# Note: To be fully vaccinated, the child must have received: $RI_DOSES_TO_BE_FULLY_VACCINATED
vcqi_global(DESC_02_TO_FOOTNOTE_6,
            paste0(language_string(language_use = language_use, str = "OS_108"),
                   str_flatten(RI_DOSES_TO_BE_FULLY_VACCINATED, collapse = " ")))
# Note: The below definitions were taken from the PAHO Methodology
#      for the Evaluation of Missed Opportunities  for Vaccination Protocol document.
vcqi_global(DESC_02_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_109"))
# Fully Vaccinated: Missing no vaccines for his/her age
vcqi_global(DESC_02_TO_FOOTNOTE_8, paste0("* ",language_string(language_use = language_use, str = "OS_110")))
# Unvaccinated: Missing all vaccines for his/her age
vcqi_global(DESC_02_TO_FOOTNOTE_9, paste0("* ",language_string(language_use = language_use, str = "OS_111")))
# Undervaccinated: Missing one or more vaccines for his/her age
vcqi_global(DESC_02_TO_FOOTNOTE_10,paste0("* ",language_string(language_use = language_use, str = "OS_112")))

DESC_02(cleanup = TRUE)

# ..............................................................................
# Study day table 1
vcqi_global(DESC_02_COVG_CRUDE_OR_VALID, "crude")
vcqi_global(DESC_02_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_02_VARIABLES, paste0("table1_dv_", str_to_lower(DESC_02_COVG_CRUDE_OR_VALID)))
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "ALL")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

# Proportion of Vaccinated, Unvaccinated and Undervaccinated
vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_113"))
# No subtitle or additional footnotes
vcqi_global(DESC_02_TO_SUBTITLE, NA)
# The proportion of children who had cards is the sum of those who were eligible for 1+ doses and those who were not
vcqi_global(DESC_02_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_114"))

vcqi_global(DESC_02_N_SUBTOTALS, 1)
vcqi_global(DESC_02_SUBTOTAL_LEVELS_1, c(0,1))
#Subtotal: Children who had cards
vcqi_global(DESC_02_SUBTOTAL_LABEL_1, language_string(language_use = language_use, str = "OS_115"))
vcqi_global(DESC_02_SUBTOTAL_LIST_1, "After 1")

# Total Interviews (N)
vcqi_global(DESC_02_N_LABEL,
            paste0(language_string(language_use = language_use, str = "OS_220"),
                   " ",
                   language_string(language_use = language_use, str = "OS_2")))

if (stringr::str_to_upper(DESC_02_COVG_CRUDE_OR_VALID) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(DESC_02_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(DESC_02_COVG_CRUDE_OR_VALID) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(DESC_02_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_106"))
}

# Note: Number of children who had cards means they had card with valid dob and
# at least one dose date
vcqi_global(DESC_02_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_89"))

DESC_02(cleanup = TRUE)

# ..............................................................................
# Version 2 of Study Day Table 2
# Breaks up performance by if child was eligible fo 0 doses, 1 dose or 2+ doses
# Study day table 2
vcqi_global(DESC_02_COVG_CRUDE_OR_VALID, "crude")
vcqi_global(DESC_02_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_02_VARIABLES, paste0("table2_dv_", str_to_lower(DESC_02_COVG_CRUDE_OR_VALID)))
vcqi_global(DESC_02_WEIGHTED, "NO")
vcqi_global(DESC_02_DENOMINATOR, "RESPONDED")

vcqi_global(DESC_02_LIST_N_BEFORE_PCT, "Yes")

# Study day performance details
vcqi_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_116"))
# Table 2: 0,1 and 2+ breakdown
vcqi_global(DESC_02_TO_SUBTITLE, language_string(language_use = language_use, str = "OS_117"))

vcqi_global(DESC_02_N_SUBTOTALS,3)
vcqi_global(DESC_02_SUBTOTAL_LEVELS_1, c(1,2))
vcqi_global(DESC_02_SUBTOTAL_LABEL_1, language_string(language_use = language_use, str = "OS_118"))
vcqi_global(DESC_02_SUBTOTAL_LIST_1, "Before 3") #In this dataset no one takes on the value of 2
vcqi_global(DESC_02_SUBTOTAL_LEVELS_2, c(3,4,5,6))
vcqi_global(DESC_02_SUBTOTAL_LABEL_2, language_string(language_use = language_use, str = "OS_119"))
vcqi_global(DESC_02_SUBTOTAL_LIST_2, "Before 7") #In this dataset no other responses after 4
vcqi_global(DESC_02_SUBTOTAL_LEVELS_3, c(7,8,9,10,11,12))
vcqi_global(DESC_02_SUBTOTAL_LABEL_3, language_string(language_use = language_use, str = "OS_120"))

# Total number of children with cards (N)
vcqi_global(DESC_02_N_LABEL, language_string(language_use = language_use, str = "OS_121"))

if (stringr::str_to_upper(DESC_02_COVG_CRUDE_OR_VALID) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(DESC_02_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(DESC_02_COVG_CRUDE_OR_VALID) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(DESC_02_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_106"))
}

# Note: Number of children who had cards means they had a card with valid dob
# and at least one dose date
vcqi_global(DESC_02_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_89"))
# Note: The sum of percentages may not add to 100% because a single visit may
# result in any combination of invalid and valid doses and MOVs
vcqi_global(DESC_02_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_122"))
#[1] No errors (perfect performance)
vcqi_global(DESC_02_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_123"))
#[2] Rec'd valid dose(s)
vcqi_global(DESC_02_TO_FOOTNOTE_8, language_string(language_use = language_use, str = "OS_124"))
#[3] Rec'd invalid dose(s)
vcqi_global(DESC_02_TO_FOOTNOTE_9,language_string(language_use = language_use, str = "OS_125"))
#[4] Experienced MOV(s)
vcqi_global(DESC_02_TO_FOOTNOTE_10, language_string(language_use = language_use, str = "OS_126"))
#[5] Asked reason why not vaccinated
vcqi_global(DESC_02_TO_FOOTNOTE_11, language_string(language_use = language_use, str = "OS_127"))
#[6] Experienced MOV but were not asked why not vaccinated
vcqi_global(DESC_02_TO_FOOTNOTE_12, language_string(language_use = language_use, str = "OS_128"))

DESC_02(cleanup = TRUE)

# ..............................................................................
# Now demonstrate using DESC_03 on a multiple-choice question where the
# respondent can select all answers that apply
# ..............................................................................

# Study day table 3
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "Study Day - Table 3")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("table3_dv_no_errors_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("table3_dv_recd_invalid_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("table3_dv_recd_valid_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("table3_dv_mov_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Study day performance summary
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_129"))
vcqi_global(DESC_03_TO_SUBTITLE, NA)

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_121"))
# Total number of children with cards (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_131"))
# Note: A child could be in more than one column so percentages may sum to more than 100%

if (stringr::str_to_upper(STUDY_DAY_VALID_OR_CRUDE) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(DESC_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(STUDY_DAY_VALID_OR_CRUDE) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(DESC_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_106"))
}

vcqi_global(DESC_03_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_89"))
# Note: Number of children who had cards means they had card with valid dob and
# at least one dose date

DESC_03(cleanup = TRUE)

# ..............................................................................
# Study table 4 completed with ES_STUD_01 Indicator
# Need to identify if CRUDE of VALID
vcqi_global(ES_STUD_01_VALID_OR_CRUDE, "crude") # Set to CRUDE or VALID

# Invalid doses administered on visit day
vcqi_global(ES_STUD_01_TO_TITLE, language_string(language_use = language_use, str = "OS_84"))
vcqi_global(ES_STUD_01_TO_SUBTITLE, NA)
# Note: For each dose, N is the number of children who had cards and a dob and
# were not eligible for that dose
vcqi_global(ES_STUD_01_TO_FOOTNOTE_1, language_string(language_use = language_use, str = "OS_88"))
# Note: Number of children who had cards means they had card with valid dob and
# at least one dose date
vcqi_global(ES_STUD_01_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_89"))

if (stringr::str_to_upper(ES_STUD_01_VALID_OR_CRUDE) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(ES_STUD_01_TO_FOOTNOTE_3, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(ES_STUD_01_VALID_OR_CRUDE) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(ES_STUD_01_TO_FOOTNOTE_3, language_string(language_use = language_use, str = "OS_106"))
}

ES_STUD_01()

# ..............................................................................
# Study table 5 completed with ES_STUD_02 Indicator
# Need to identify if CRUDE of VALID
vcqi_global(ES_STUD_02_VALID_OR_CRUDE, "crude") # Set to CRUDE or VALID

#Missed opportunities and valid doses administered on visit day
vcqi_global(ES_STUD_02_TO_TITLE, language_string(language_use = language_use, str = "OS_132"))
vcqi_global(ES_STUD_02_TO_SUBTITLE, NA)
# Note: Only children who had a vaccination card with a valid dob, at least one
# dose date, and were eligible to receive at least one dose on the study day are
# summarized in the table
vcqi_global(ES_STUD_02_TO_FOOTNOTE_1, language_string(language_use = language_use, str = "OS_133"))

if (stringr::str_to_upper(ES_STUD_02_VALID_OR_CRUDE) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(ES_STUD_02_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(ES_STUD_02_VALID_OR_CRUDE) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(ES_STUD_02_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_106"))
}

ES_STUD_02()

# ..............................................................................
# Study table 6a and table 6b completed with ES_STUD_02 Indicator
# Need to identify if CRUDE of VALID
vcqi_global(ES_STUD_03_VALID_OR_CRUDE, "crude")

# Comparison Of Invalid Doses, Valid Doses And MOVs Before Study Day And On Study Day(s)
vcqi_global(ES_STUD_03_TO_TITLE, language_string(language_use = language_use, str = "OS_134"))
# Across all Antigens
vcqi_global(ES_STUD_03_TO_SUBTITLE_FOR_ALL, language_string(language_use = language_use, str = "OS_135"))
# Broken down by Antigen
vcqi_global(ES_STUD_03_TO_SUBTITLE_ANTIGEN, language_string(language_use = language_use, str = "OS_136"))

# Note: Only children who had a vaccination card with a valid dob, at least one
# dose date, and were eligible to receive at least one dose on the study day are
# summarized in the table
vcqi_global(ES_STUD_03_TO_FOOTNOTE_1, language_string(language_use = language_use, str = "OS_133"))

if (stringr::str_to_upper(ES_STUD_03_VALID_OR_CRUDE) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(ES_STUD_03_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(ES_STUD_03_VALID_OR_CRUDE) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(ES_STUD_03_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_106"))
}

# Note: Dose is included in the INVALID TOTAL category if child was not eligible
# for that dose.
vcqi_global(ES_STUD_03_TO_FOOTNOTE_3, language_string(language_use = language_use, str = "OS_137"))
# Note: RECEIVED INVALID DOSE means the child was not eligible and received the
# dose.
vcqi_global(ES_STUD_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_138"))
# Note: RECEIVED VALID DOSE means the child was eligible and received the dose.
vcqi_global(ES_STUD_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_139"))
# Note: EXPERIENCED MOV means the child was eligible and did not receive the dose.
vcqi_global(ES_STUD_03_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_140"))
# Note: Dose is included in the ELIGIBLE TOTAL category if child was eligible for that dose.
vcqi_global(ES_STUD_03_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_141"))

ES_STUD_03()

# ..............................................................................
# Create table for MOV reasons overview 1
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "mov_rsns_all_1")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("reasons_overview1_A_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("reasons_overview1_B_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("reasons_overview1_C_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("reasons_overview1_D_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Overview of reasons for MOV from respondents who were not eligible for any doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_142"))
vcqi_global(DESC_03_TO_SUBTITLE,NA)

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_143"))
# Total number of children who were not eligible for any doses and were asked
# this question (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, paste0(language_string(language_use = language_use, str = "OS_144"), " 1A"))
# [1] Details in Table MOV reasons 1A

vcqi_global(DESC_03_TO_FOOTNOTE_5, paste0(language_string(language_use = language_use, str = "OS_145"), " 1B"))
# [2] Details in Table MOV reasons 1B

vcqi_global(DESC_03_TO_FOOTNOTE_6, paste0(language_string(language_use = language_use, str = "OS_146"), " 1C"))
# [3] Details in Table MOV reasons 1C

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for tab MOV reasons 1A
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "MOV_rsns_1A")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("ES08AA_1A_01_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_02_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_01_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_02_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_03_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_04_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_05_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_06_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_07_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_08_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_09_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_10_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_11_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_12_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Related to Health Workers: Reasons for MOV from respondents who were not
# eligible for any doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_148"))
# MOV reasons 1A
vcqi_global(DESC_03_TO_SUBTITLE,paste0(language_string(language_use = language_use, str = "OS_149"), " 1A"))

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_147"))
# Total number of respondents who gave a reason related to health workers (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_150"))
# Note: These are not the respondents of primary interest since they were not
# eligible for any doses

vcqi_global(DESC_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_151"))
# Note: Denominators for b-c is column p

vcqi_global(DESC_03_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_152"))
# For questions 3.#, the doctor or nurse said that it could not be done because
# the child is sick

vcqi_global(DESC_03_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_153"))
# More than one item from the list in 3.# may have been checked for an individual

vcqi_global(DESC_03_TO_FOOTNOTE_8, language_string(language_use = language_use, str = "OS_154"))
# May not equal the row total because individuals were allowed to check more
# than one from 3.#

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for tab MOV reasons 1B
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "MOV_rsns_1B")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("ES08AA_1B_01_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_02_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_03_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_04_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_05_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_06_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_07_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_08_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_09_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_10_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_11_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_12_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Reasons for MOV from respondents who were not eligible for any doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_155"))
# MOV reasons 1B
vcqi_global(DESC_03_TO_SUBTITLE, paste0(language_string(language_use = language_use, str = "OS_149"), " 1B"))

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_156"))
# Total number of respondents who gave a reason related to the caregiver (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_157"))
# Note: Denominators for b-m is column n

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for tab MOV reasons 1C
vcqi_global(STUDY_DAY_VALID_OR_CRUDE,"crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "MOV_rsns_1C")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("ES08AA_1C_01_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_02_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_03_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_04_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_05_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_06_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_07_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_08_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_09_ineligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Reasons for MOV from respondents who were not eligible for any doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_155"))
# MOV reasons 1C
vcqi_global(DESC_03_TO_SUBTITLE, paste0(language_string(language_use = language_use, str = "OS_149"), " 1C"))

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_158"))
# Total number of respondents who gave a reason related to health service’s
# logistics and organization (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_159"))
# Note: Denominators for b-j is column k

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for MOV reasons overview 1
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "mov_rsns_all_2")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("reasons_overview2_A_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("reasons_overview2_B_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("reasons_overview2_C_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("reasons_overview2_D_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Overview of reasons for MOV from respondents who were eligible for 1+ doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_160"))
vcqi_global(DESC_03_TO_SUBTITLE,NA)

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_161"))
# Total number of children eligible for 1+ doses and were asked this question (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, paste0(language_string(language_use = language_use, str = "OS_144"), " 2A"))
# [1] Details in Table MOV reasons 2A

vcqi_global(DESC_03_TO_FOOTNOTE_5, paste0(language_string(language_use = language_use, str = "OS_145"), " 2B"))
# [2] Details in Table MOV reasons 2B

vcqi_global(DESC_03_TO_FOOTNOTE_6, paste0(language_string(language_use = language_use, str = "OS_146"), " 2C"))
# [3] Details in Table MOV reasons 2C

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for tab MOV reasons 2A
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "MOV_rsns_2A")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("ES08AA_1A_01_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_02_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_01_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_02_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_03_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_04_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_05_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_06_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_07_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_08_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_09_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_10_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_11_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1A_03_12_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Related to Health Workers: Reasons for MOV from respondents who were not eligible for any doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_148"))
# MOV reasons 2A
vcqi_global(DESC_03_TO_SUBTITLE,paste0(language_string(language_use = language_use, str = "OS_149"), " 2A"))

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_147"))
# Total number of respondents who gave a reason related to health workers (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_162"))
# Note: The study protocol only administers this question if the child received
# 0 doses on the study day. Some respondents may receive some doses AND
# experience 1+ MOVs.

vcqi_global(DESC_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_151"))
# Note: Denominators for b-c is column p

vcqi_global(DESC_03_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_152"))
# For questions 3.#, the doctor or nurse said that it could not be done because
# the child is sick

vcqi_global(DESC_03_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_153"))
# More than one item from the list in 3.# may have been checked for an individual

vcqi_global(DESC_03_TO_FOOTNOTE_8, language_string(language_use = language_use, str = "OS_154"))
# May not equal the row total because individuals were allowed to check more
# than one from 3.#

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for tab MOV reasons 2B
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "MOV_rsns_2B")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("ES08AA_1B_01_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_02_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_03_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_04_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_05_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_06_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_07_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_08_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_09_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_10_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_11_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1B_12_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Reasons for MOV from respondents who were eligible for 1+ doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_163"))
# MOV reasons 2B
vcqi_global(DESC_03_TO_SUBTITLE, paste0(language_string(language_use = language_use, str = "OS_149"), " 2B"))

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_156"))
# Total number of respondents who gave a reason related to the caregiver (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_162"))
# Note: The study protocol only administers this question if the child received
# 0 doses on the study day. Some respondents may receive some doses AND
# experience 1+ MOVs.
vcqi_global(DESC_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_157"))
# Note: Denominators for b-m is column n

DESC_03(cleanup = TRUE)

# ..............................................................................
# Create table for tab MOV reasons 2C
vcqi_global(STUDY_DAY_VALID_OR_CRUDE, "crude")
vcqi_global(DESC_03_DATASET, "MOV_dvs.rds")
vcqi_global(DESC_03_SHORT_TITLE, "MOV_rsns_2C")
vcqi_global(
  DESC_03_VARIABLES,
  c(paste0("ES08AA_1C_01_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_02_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_03_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_04_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_05_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_06_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_07_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_08_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE)),
    paste0("ES08AA_1C_09_eligible_", str_to_lower(STUDY_DAY_VALID_OR_CRUDE))))
vcqi_global(DESC_03_WEIGHTED, "NO")
vcqi_global(DESC_03_DENOMINATOR, "RESPONDED")
vcqi_global(DESC_03_SELECTED_VALUE, 1)

# Reasons for MOV from respondents who were eligible for 1+ doses
vcqi_global(DESC_03_TO_TITLE, language_string(language_use = language_use, str = "OS_163"))
# MOV reasons 2C
vcqi_global(DESC_03_TO_SUBTITLE,paste0(language_string(language_use = language_use, str = "OS_149"), " 2C"))

vcqi_global(DESC_03_LIST_N_BEFORE_PCT, "Yes")

vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_158"))
# Total number of respondents who gave a reason related to health service’s
# logistics and organization (N)

vcqi_global(DESC_03_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_162"))
# Note: The study protocol only administers this question if the child received
# 0 doses on the study day. Some respondents may receive some doses AND
# experience 1+ MOVs.

vcqi_global(DESC_03_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_159"))
# Note: Denominators for b-j is column k

DESC_03(cleanup = TRUE)

# Drop rows with no useful cards before calling RI_QUAL_08/09 so the count of those with cards
# will line up with ES_STUD_*.  (Otherwise the dose VISIT confuses the RI_QUAL_09 counter.)

dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))
saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids_full_dataset.rds"))

dat <- dat %>% filter(has_card_with_dob_and_dosedate %in% 1)
saveRDS(dat,paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))

# ..............................................................................

# Drop rows with no useful cards before calling RI_QUAL_08/09 so the count of
# those with cards will line up with ES_STUD_*. (Otherwise the dose VISIT
# confuses the RI_QUAL_09 counter.)

RI_with_ids <- readRDS(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))
saveRDS(RI_with_ids, paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids_full_dataset.rds"))

RI_with_ids <- RI_with_ids[RI_with_ids$has_card_with_dob_and_dosedate %in% 1,]

saveRDS(RI_with_ids, paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))

# Take out visit from dose list because don't want to loop over it for the next indicators
vcqi_global(RI_DOSE_LIST, RI_DOSE_LIST[-which(RI_DOSE_LIST == "visit")])

# Estimate the proportion of visits that had MOVs
vcqi_global(RI_QUAL_08_VALID_OR_CRUDE, "crude")

# Percent of Visits with MOVs
vcqi_global(RI_QUAL_08_TO_TITLE, language_string(language_use = language_use, str = "OS_164"))
vcqi_global(RI_QUAL_08_TO_SUBTITLE, NA)
vcqi_global(RI_QUAL_08_TO_FOOTNOTE_1, language_string(language_use = language_use, str = "OS_165"))
# Percent of visits where children were eligible for the dose and did not receive it.

if (stringr::str_to_upper(RI_QUAL_08_VALID_OR_CRUDE) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(RI_QUAL_08_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(RI_QUAL_08_VALID_OR_CRUDE) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(RI_QUAL_08_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_106"))
}

# Note: The final measure on this sheet, MOVs per Visit, is not a percent. It
# is a ratio.
vcqi_global(RI_QUAL_08_TO_FOOTNOTE_3, language_string(language_use = language_use, str = "OS_166"))
vcqi_global(SORT_PLOT_LOW_TO_HIGH, 0)

RI_QUAL_08()

# ..............................................................................

# Estimate the proportion of children who experienced 1+ MOVs
vcqi_global(RI_QUAL_09_VALID_OR_CRUDE, "crude")

vcqi_global(RI_QUAL_09_TO_TITLE, language_string(language_use = language_use, str = "OS_167"))
# Percent of Respondents with MOVs
vcqi_global(RI_QUAL_09_TO_SUBTITLE, NA)
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_1, language_string(language_use = language_use, str = "OS_168"))
# Percent of respondents who had date of birth and visit date data who failed to
# receive a vaccination for which they were eligible on an occasion when they
# received another vaccination.
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_2, language_string(language_use = language_use, str = "OS_169"))
# An uncorrected MOV means that the respondent had still not received a valid
# dose at the time of the survey.
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_3, language_string(language_use = language_use, str = "OS_170"))
# A corrected MOV means that the respondent had received a valid dose by the
# time of the survey.
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_4, language_string(language_use = language_use, str = "OS_171"))
# The denominator for Had MOV (%) is the number of respondents who had visits
# eligible.
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_5, language_string(language_use = language_use, str = "OS_172"))
# The denominator for MOV uncorrected and corrected (%) is the number of MOVs.
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_6, language_string(language_use = language_use, str = "OS_173"))
# Note that for individual doses, the % MOV uncorrected + % MOV corrected adds
# up to 100%.

if (stringr::str_to_upper(RI_QUAL_09_VALID_OR_CRUDE) == "VALID"){
  # Note: Early doses are ignored in this analysis; the respondent is considered
  # to have not received them.
  vcqi_global(RI_QUAL_09_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_90"))
}

if (stringr::str_to_upper(RI_QUAL_09_VALID_OR_CRUDE) == "CRUDE"){
  # Note: Early doses are accepted in this analysis; all doses are considered
  # valid doses.
  vcqi_global(RI_QUAL_09_TO_FOOTNOTE_7, language_string(language_use = language_use, str = "OS_106"))
}

RI_QUAL_09()

# *************************************************
# Code Block: ES-G             (Do not change) ----
#
# Exit gracefully

# Close the datasets that hold the results of hypothesis tests and put them into
# the output spreadsheet
# Close the log file and put it into the output spreadsheet
# Clean up extra files
# Send a message to the screen if there are warnings or errors in the log
miss_vcqi_cleanup()
