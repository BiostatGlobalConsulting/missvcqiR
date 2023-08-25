#' Define and log a value in the global environment
#'
#' @param global The name of the global value
#' @param value The value to be assigned
#'
#' @return An R object in the global environment
#'
#' @export
#'
#' @examples
#' vcqi_global(x, 100)
#' vcqi_global(y, c("a", "b", "c"))
#'
# vcqi_global R version 1.00 - Biostat Global Consulting - 2022-06-27
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-27  1.00      Caitlin Clary   Original R version
# *******************************************************************************

# If the user calls vcqi_global(GLOBAL_NAME, value) then this program will
# a) assign GLOBAL_NAME <- value and then
# b) put the current value of GLOBAL_NAME into the VCQI log

vcqi_global <- function(global, value){

  assign(deparse(substitute(global)), value, envir = .GlobalEnv)
  do.call(vcqi_log_global, list(substitute(global)))

}
