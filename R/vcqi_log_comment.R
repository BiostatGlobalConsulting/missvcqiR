#' Write comment to VCQI program progress log
#'
#' @param program The program this comment is associated with
#' @param level The level of this comment (1 = high level, e.g. errors, 2 = second-order, e.g. warnings, 3 = general comments, e.g. about files being saved; 4 = comments on properties of datasets, 5 = debugging detail, e.g. program flow comments)
#' @param keyword The category this comment belongs to, e.g. Error, Warning, Comment, Data, or Flow
#' @param comment The comment to be written
#'
#' @import readr
#'
#' @export
#'
#' @examples
#' vcqi_log_comment("Program 1", 1, "Error", "Required value X not defined")

# vcqi_log_comment R version 1.04 - Biostat Global Consulting - 2022-11-03
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-22  1.00      Caitlin Clary   Original R version
# 2022-07-06  1.01      Caitlin Clary   Use vcqi_source() to open log
# 2022-07-28  1.02      Caitlin Clary   Use vcqi_object_exists()
# 2022-10-05  1.03      Mia Yu          Package version
# 2022-11-03  1.04      Caitlin Clary   Add'l check for vcqi_open_log
# *******************************************************************************

vcqi_log_comment <- function(program, level, keyword, comment){
  # Note: vcqi_open_log() checks that VCQI_OUTPUT_FOLDER and VCQI_ANALYSIS_NAME exist
  # and creates VCQI_LOGFILE_NAME

  # If there isn't an open log file, open one now
  if(vcqi_object_exists("VCQI_LOGOPEN") == FALSE){
    vcqi_global(VCQI_LOGOPEN, 0)
  }

  if(VCQI_LOGOPEN != 1){
    vcqi_open_log()
  }

  if(vcqi_object_exists("VCQI_OUTPUT_FOLDER") & vcqi_object_exists("VCQI_LOGFILE_NAME")){
    if(!file.exists(
      paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_LOGFILE_NAME, ".csv"))){
      vcqi_open_log()
    }}

  # Post comment to log file
  readr::write_csv(data.frame(
    as.character(Sys.Date()),
    as.character(format(Sys.time(), format="%H:%M:%S")),
    program, level, keyword, comment
  ),
  file = paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_LOGFILE_NAME, ".csv"),
  append = TRUE)

}
