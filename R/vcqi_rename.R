#' Rename a variable, allowing for quoted names
#'
#' @param dat Name of dataset to modify
#' @param old_name Variable name to change
#' @param new_name New variable name to assign
#'
#' @return Modified data frame
#'
#' @export
#'
#' @examples
#' vcqi_rename(dat, "varname", "newvarname")

# vcqi_rename R version 1.00 - Biostat Global Consulting - 2026-06-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2026-06-08  1.00      Caitlin Clary   Original  version
# *******************************************************************************

vcqi_rename <- function(dat, old_name, new_name){
  names(dat)[which(names(dat) == old_name)] <- new_name
  dat
}
