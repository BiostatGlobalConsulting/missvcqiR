#' Get the specific string with the user required language
#'
#' @param language_use The global that stored the language the user choose
#' @param str The string to be retrived from the language file
#'
#' @return A string
#'
#' @import openxlsx
#' @import stringr

# language_string R version 1.00 - Biostat Global Consulting - 2023-08-12
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-12  1.00      Mia Yu          Original R package version
# *******************************************************************************

language_string <- function(language_use, str, replaceq = FALSE) {

  if (file.exists(paste0(VCQI_DATA_FOLDER,"/MISS VCQI Label Phrases - En Fr Es Pt.xlsx"))){
    dat <- openxlsx::read.xlsx(xlsxFile = paste0(VCQI_DATA_FOLDER,"/MISS VCQI Label Phrases - En Fr Es Pt.xlsx"),
                               sheet = "Latest version")
  } else if (file.exists(paste0(VCQI_DATA_FOLDER,"/Multi-Lingual Phrases - En Fr Es Pt.xlsx"))){
    dat <- openxlsx::read.xlsx(xlsxFile = paste0(VCQI_DATA_FOLDER,"/Multi-Lingual Phrases - En Fr Es Pt.xlsx"),
                               sheet = "Latest version")
  }

  names(dat) <- str_to_upper(names(dat))
  var <- get(language_use,dat)
  returnstr <- var[which(dat$STRING_NAME == str)]

  if (replaceq == TRUE){
    returnstr <- gsub("'", "\\\\'", returnstr)
  }

  return(returnstr)
}
