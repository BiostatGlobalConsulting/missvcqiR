#' Check the length of title and split into two lines if two long
#'
#' @param title_string The original title, a string
#'
#' @return A string
#'
#' @import stringr

# create_multi_lingual_plot_title R version 1.00 - Biostat Global Consulting - 2023-08-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# *******************************************************************************

create_multi_lingual_plot_title <- function(title_string){
  print(title_string)

  l = nchar(title_string)

  if (l > 40){
    wordlength = sapply(strsplit(title_string, " "), length)

    final_title <- stringr::word(title_string,1)
    currentline <- stringr::word(title_string,1)

    for (i in 2:wordlength){

      t <- paste0(currentline, " ", stringr::word(title_string,i))
      check <- stringr::str_trim(t)

      if (nchar(check) > 40){
        currentline <- stringr::word(title_string,i)
        final_title <- paste0(final_title, "\n", stringr::word(title_string,i))
      } else {
        currentline <- paste0(currentline, " ", stringr::word(title_string,i))
        final_title <- paste0(final_title, " ", stringr::word(title_string,i))
      }

    } #end of wordlength i loop

  } else {
    final_title <- title_string
  }

  return(final_title)
}
