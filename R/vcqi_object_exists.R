#' Check if an object is defined, not NULL, not NA, and not an empty vector
#'
#' @param object Name of object to be tested
#'
#' @return TRUE or FALSE
#'
#' @import stringr
#'
#' @export
#'
#' @examples
#' vcqi_object_exists("test_value")

# vcqi_object_exists R version 1.02 - Biostat Global Consulting - 2022-10-05
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-23  1.00      Caitlin Clary   Original R version
# 2022-08-01  1.01      Caitlin Clary   Avoid str_squish error when object isn't an atomic vector
# 2022-10-05  1.02      Mia Yu          Package version
# *******************************************************************************

# Function checks if an object is in the global enviroment and
# not NULL and not NA and not an empty string

vcqi_object_exists <- function(object){

  # Set starting value, switch to FALSE if any checks fail
  existsval <- TRUE

  # Check 1: object in global environment
  if(exists(object, envir = globalenv()) == FALSE){
    existsval <- FALSE
  } else if(exists(object, envir = globalenv()) == TRUE){
    obj <- get(object)

    # Check 2: object null
    if(is.null(obj)){
      existsval <- FALSE

      # Check 3: object NA
    } else if(all(is.na(obj))){
      existsval <- FALSE

      # Checks for atomic vectors only:
    } else if(is.atomic(obj)){

      # Check 4: empty string
      if(all(obj == "")){
        existsval <- FALSE

        # Check 5: string of only spaces
      } else if(all(str_squish(obj) == "")){
        existsval <- FALSE
      }
    } # end checks when object is a vector

  } # end if object exists

  existsval
}
