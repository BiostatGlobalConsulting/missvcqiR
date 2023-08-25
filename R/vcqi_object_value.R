#' Check if an object takes on a specified value or is in a range of values
#'
#' @param object an object to check
#' @param value a value or a range of values
#'
#' @return TRUE or FALSE
#'
#' @export
#'
#' @examples
#' X <- 1
#' vcqi_object_value("X", 1)
#' vcqi_object_value("X", 2)

# vcqi_object_value R version 1.01 - Biostat Global Consulting - 2022-10-05
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-08  1.00      Caitlin Clary   Original R version
# 2022-10-05  1.01      Mia Yu          Package version
# *******************************************************************************

# Function checks if an object takes on a specified value or is in a range of values
# TRUE if so
# FALSE if not, or if the object doesn't exist in the global environment

vcqi_object_value <- function(object, value){

  if(is.na(value)){stop("This function cannot check if an object is NA")}
  if(is.null(value)){stop("This function cannot check if an object is NULL")}

  if(exists(object, envir = globalenv()) == FALSE){
    returnval <- FALSE
  } else if(exists(object, envir = globalenv()) == TRUE){
    obj <- get(object, envir = globalenv())

    if(obj %in% value){
      returnval <- TRUE
    } else {
      returnval <- FALSE
    }
  }

  returnval
}
