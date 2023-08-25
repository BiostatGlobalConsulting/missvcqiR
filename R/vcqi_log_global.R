#' Put the current value of a global into the VCQI log
#'
#' @param global A global value to be logged
#'
#' @import stringr
#'
#' @return Call to vcqi_log_comment
#'
#' @export
#'
#' @examples
#' test_value <- 1
#' vcqi_log_global(test_value)

# vcqi_log_global R version 1.04 - Biostat Global Consulting - 2022-12-01
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-23  1.00      Caitlin Clary   Original R version
# 2022-06-28  1.01      Caitlin Clary   Added process for handling null global values
# 2022-07-08  1.02      Caitlin Clary   Gracefully handle quoted/pasted input as well as unquoted
# 2022-10-05  1.03      Mia Yu          Package version
# 2022-12-01  1.04      Caitlin Clary   New handling for list-type inputs
# *******************************************************************************

# NOTE Stata version includes process to split into multiple comments; not implemented here

vcqi_log_global <- function(global){

  # Global value
  gv <- try(get(deparse(substitute(global))), silent = TRUE)
  if(inherits(gv, "try-error")){gv <- try(get(global), silent = TRUE)}

  # Global name
  gn <- try(deparse(substitute(global)), silent = TRUE)
  if(str_detect(gn, "\"") == TRUE){
    gn <- global
  }

  # Handling list and c() values
  if (is.list(gv)) {
    gv <- lapply(seq_along(gv), function(x) paste(
      names(gv)[[x]],
      " = ",
      paste(gv[[x]], collapse = ""))
      )
    gv <- paste(gv, collapse = ", ")
  } else if (length(gv) > 1){
    gv <- paste(gv, collapse = ", ")
  }
  # Handling non-existent values
  if(length(gv) == 0){
    gv <- "null/not specified"
  }

  if(exists(gn) == FALSE){
    vcqi_log_comment(VCP, 3, "Global", paste0("Global value ", gn, " is not defined at this time."))
  } else {
    vcqi_log_comment(VCP, 3, "Global", paste0("Global value ", gn, " is ", gv))
  }
}
