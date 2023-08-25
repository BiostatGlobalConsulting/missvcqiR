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

  worldlength = sapply(strsplit(title_string, " "), length)

  final_title <- title_string

  if (worldlength > 5){
    title1 <- word(title_string, start = 1, end = 5)
    title2 <- word(title_string, start = 6, end = worldlength)
    final_title <- paste0(title1, "\n", title2)
  }

  if (worldlength > 10){
    title1 <- word(title_string, start = 1, end = 5)
    title2 <- word(title_string, start = 6, end = 10)
    title3 <- word(title_string, start = 11, end = worldlength)
    final_title <- paste0(title1, "\n", title2, "\n", title3)
  }

  if (worldlength > 15){
    title1 <- word(title_string, start = 1, end = 5)
    title2 <- word(title_string, start = 6, end = 10)
    title3 <- word(title_string, start = 11, end = 15)
    title4 <- word(title_string, start = 11, end = worldlength)
    final_title <- paste0(title1, "\n", title2, "\n", title3, "\n", title4)
  }

  return(final_title)
}
