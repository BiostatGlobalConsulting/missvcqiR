#' Detect file type and read dataset
#'
#' @param file File to read
#'
#' @return A data frame or tibble
#'
#' @import haven
#' @import readr
#' @rawNamespace import(tools, except = makevars_user)

# vcqi_read R version 1.01 - Biostat Global Consulting - 2022-10-04
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-23  1.00      Caitlin Clary   Original R version
# 2022-10-04  1.01      Caitlin Clary   Package version
# *******************************************************************************

vcqi_read <- function(file){

  infile_ext <- file_ext(file)

  # Read CM dataset OR error if file extension not supported
  if(infile_ext == "dta"){
    indat <- haven::read_dta(file)
  } else if(infile_ext == "csv"){
    indat <- readr::read_csv(file)
  } else if(infile_ext == "rds"){
    indat <- readRDS(file)
  } else {
    indat <- paste0("Error: file extension ", infile_ext, " is not supported. Supported file extensions are: rds, csv, dta")
  }

  # Return:
  indat
}
