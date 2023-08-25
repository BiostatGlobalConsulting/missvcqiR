#' Open a clean VCQI log file
#'
#' @return A CSV file in VCQI_OUTPUT_FOLDER
#'
#' @import openxlsx
#' @import readr

 # vcqi_open_log R version 1.03 - Biostat Global Consulting - 2022-12-07
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-22  1.00      Caitlin Clary   Original R version
# 2022-07-28  1.01      Caitlin Clary   Add formatting/comments, use vcqi_object_exists
# 2022-10-10  1.02      Caitlin Clary   Modify how logfile name and logopen values
#                                       are stored (for package context)
# 2022-12-07  1.03      Caitlin Clary   Use the GetVCQIHelp@biostatglobal.com
#                                       email in log help instructions
# *******************************************************************************

# Note: this program is called from vcqi_log_comment

# If the user has not established the required global variables then exit. We do
# not call the program to exit gracefully here, because that, too requires these
# global macros to be in place...this is a hard stop very early in the process
# of the run if these basics are not in place.

vcqi_open_log <- function(){
  if(vcqi_object_exists("VCQI_LOGOPEN") == TRUE){
    if(get("VCQI_LOGOPEN", envir = globalenv()) == 1){
      stop("You are attempting to open a VCQI log file, but the VCQI_LOGOPEN value indicates that one is already open.")
    }
  }

  if(vcqi_object_exists("VCQI_OUTPUT_FOLDER") == FALSE){
    stop("Define the VCQI_OUTPUT_FOLDER before attempting to open a log file.")
  }

  if(vcqi_object_exists("VCQI_ANALYSIS_NAME") == FALSE){
    stop("Define the VCQI_ANALYSIS_NAME before attempting to open a log file.")
  }

  assign("VCQI_LOGFILE_NAME", paste0("VCQI_", VCQI_ANALYSIS_NAME, "_LOG"), envir = .GlobalEnv)

  # Export placeholder text to Excel tabular output file

  TO_wb <- createWorkbook()
  addWorksheet(TO_wb, sheetName = "Log")

  # TO DO review the instructions to open/view the log - in Stata the process is to type vcqi_cleanup() - the current process is a functional placeholder
  writeData(TO_wb, "Log", data.frame(
    `Placeholder Log Text` = c(
      "This text is a placeholder.",
      "If VCQI exits in a clean manner then this text will disappear.",
      "If VCQI has halted and this text is in the worksheet, you might find an informative log in the R log dataset.",
      paste0("If that happens, navigate to your VCQI_OUTPUT_FOLDER (", VCQI_OUTPUT_FOLDER, ") and look for the log file named ", VCQI_LOGFILE_NAME, ".csv"),
      "Open this VCQI log and scroll to the bottom to discover clues as to what went wrong.",
      # "If that happens, go to the R command line and type the following line:",
      # 'log <- read.csv(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_LOGFILE_NAME, ".csv"))',
      # "",
      # "Now the VCQI log will be loaded into R as a dataset called 'log'. You can type View(log) in the R console to view the dataset and scroll to the bottom to discover clues as to what went wrong.",
      "Contact GetVCQIHelp@biostatglobal.com if you have questions. If possible, attach your log file to the e-mail.")
  ))

  openxlsx::saveWorkbook(
    TO_wb,
    file = paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_ANALYSIS_NAME, "_TO.xlsx"),
    overwrite = TRUE)

  # Open log file
  vcqi_log <- data.frame(
    date = Sys.Date(),
    time = format(Sys.time(), format="%H:%M:%S"),
    program = "VCQI_LOG_OPEN",
    level = 3,
    entry_type = "c(return)",
    entry = "The following comments document characteristics of the computer that is running VCQI."
  )

  vcqi_log[2,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "VCQI_LOG_OPEN", 3, "c(return)",
                    R.version.string)

  vcqi_log[3,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "VCQI_LOG_OPEN", 3, "c(return)",
                    paste0("OS: ", Sys.info()[1]))

  vcqi_log[4,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "VCQI_LOG_OPEN", 3, "c(return)",
                    paste0("Release: ", Sys.info()[2]))

  vcqi_log[5,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "VCQI_LOG_OPEN", 3, "c(return)",
                    paste0("Machine type: ", Sys.info()[5]))

  readr::write_csv(vcqi_log,
                   file = paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_LOGFILE_NAME, ".csv"),
                   append = FALSE)

  assign("VCQI_LOGOPEN", 1, envir = .GlobalEnv)
}
