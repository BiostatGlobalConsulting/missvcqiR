#' Split the text with default/customized cutoff point
#'
#' @param text_string The original title, a string
#' @param text_cutoff  The cutoff point, an integer
#'
#' @return A string
#'
#' @import stringr

# split_text R version 1.00 - Biostat Global Consulting - 2024-05-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-05-20  1.00      Mia Yu          Original R package version
# *******************************************************************************

split_text <- function(text_string, text_cutoff = -99){
  print(text_string)

  l = nchar(text_string)

  # If the default cut off value is used, set it to the text length
  if (text_cutoff == -99){
    text_cutoff = l
  }

  # Check to see if the text length is > TEXT_CUTOFF characters (48 can fit across the plots)
  # If it is, we will need to split them up

  if (l > text_cutoff){
    wordlength = sapply(strsplit(text_string, " "), length)

    final_text <- stringr::word(text_string,1)
    currentline <- stringr::word(text_string,1)

    for (i in 2:wordlength){

      t <- paste0(currentline, " ", stringr::word(text_string,i))
      check <- stringr::str_trim(t)

      if (nchar(check) > text_cutoff){
        currentline <- stringr::word(text_string,i)
        final_text <- paste0(final_text, "\n", stringr::word(text_string,i))
      } else {
        currentline <- paste0(currentline, " ", stringr::word(text_string,i))
        final_text <- paste0(final_text, " ", stringr::word(text_string,i))
      }

    } #end of wordlength i loop

  } else {
    final_text <- text_string
  }

  return(final_text)
}
