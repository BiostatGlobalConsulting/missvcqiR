#' Check upstream output from RI_COVG_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors and/or warnings if conditions not met

# check_RI_COVG_02_03DV R version 1.01 - Biostat Global Consulting - 2022-11-02
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-02  1.00      Mia Yu          Original R version
# 2022-11-02  1.01      Mia Yu          Package version
# *******************************************************************************

# VCQI users are encouraged to generate output and then change the value of
# ANALYSIS_COUNTER and change some options and generate a second (or third...)
# set of output from within the same control program.

# When they do that, the indicators that rely on output from earlier indicators
# may be confused when they do not find output from upstream indicators
# tagged with the new ANALYSIS_COUNTER.

# This program looks for the upstream output and if it is not found, it looks
# to see if there is output from when the ANALYSIS_COUNTER was 1.  If so, it
# copies the output into a new dataset and renames it to have _ANALYSIS_COUNTER
# in the name.

# The program puts a warning in the log if it copies and renames a file
# because it is based on the assumption that the earlier indicator was run
# with ANALYSIS_COUNTER set to 1 and it is okay to use that output for
# the new analysis.

check_RI_COVG_02_03DV <- function(VCP = "check_RI_COVG_02_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  filename <- paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_02_",ANALYSIS_COUNTER,".rds")

  if (!file.exists(filename)){

    if (file.exists(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_02_1.rds"))) {
      file.copy(from = paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_02_1.rds"), to = filename, overwrite = TRUE)

      if (!vcqi_object_exists("RI_COVG_02_TEMP_DATASETS")){
        RI_COVG_02_TEMP_DATASETS <- NULL
      }

      vcqi_global(RI_COVG_02_TEMP_DATASETS,c(RI_COVG_02_TEMP_DATASETS,paste0("RI_COVG_02_",ANALYSIS_COUNTER,".rds")))
      vcqi_log_comment(VCP, 2, "Warning", paste0(filename, " does not exist. VCQI will make a copy of RI_COVG_02_1.rds and proceed."))
    } else{
      exitflag <- 1
      errormsgs <- c(errormsgs,
                     paste0("The file", filename, " does not exist. Run RI_COVG_02_03DV"))
      vcqi_log_comment(VCP, 1, "Error", paste0("The file ", filename, " does not exist. Run RI_COVG_02_03DV"))
    }

  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
