#' Exit MISS VCQI gracefully
#'
#' @return Call to miss_vcqi_halt_immediately
#'
#' @export
#'
#' @examples
#' miss_vcqi_cleanup()

# miss_vcqi_cleanup R version 1.00 - Biostat Global Consulting - 2023-08-25
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-25  1.00      Mia Yu      Copied and revised from vcqi_cleanup
# *******************************************************************************

miss_vcqi_cleanup <- function(){

  # If the user was doing a check run, then unset this flag so VCQI will exit
  # gracefully
  vcqi_global(VCQI_CHECK_INSTEAD_OF_RUN, 0)

  # Close the datasets that hold the results of hypothesis tests and put them into
  # the output spreadsheet
  # Close the log file and put it into the output spreadsheet
  # Clean up extra files
  # Send a message to the screen if there are warnings or errors in the log
  miss_vcqi_halt_immediately()
}
