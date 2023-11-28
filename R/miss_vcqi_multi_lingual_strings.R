#' Check language choice and language file for MISS VCQI
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A global that records the language choice and errors and/or warnings if conditions not met
#'
#' @import stringr
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' miss_vcqi_multi_lingual_strings()

# miss_vcqi_multi_lingual_strings R version 1.01 - Biostat Global Consulting - 2023-11-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-10  1.00      Mia Yu          Original R package version
# 2023-11-08  1.01      Mia Yu          Only accepts Multi-Lingual Phrases - En Fr Es Pt.xlsx file
# *******************************************************************************


miss_vcqi_multi_lingual_strings <- function(VCP = "miss_vcqi_multi_lingual_strings"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  vcqi_global(VCQI_HALT_PROGRAM_NAME,"miss_vcqi_halt_immediately")

  if (!vcqi_object_exists("OUTPUT_LANGUAGE")){
    language <- "ENGLISH"
  } else {

    language <- get("OUTPUT_LANGUAGE", envir = .GlobalEnv)
    language <- str_to_upper(language)

    if (language == "EN"){
      language <- "ENGLISH"
    } else if (language == "ES"){
      language <- "SPANISH"
    } else if (language == "FR"){
      language <- "FRENCH"
    } else if (language == "PT"){
      language <- "PORTUGUESE"
    }
  }

  if (!language %in% c("ENGLISH","SPANISH","FRENCH","PORTUGUESE")){
    errormsgs <- c(errormsgs,paste0("Global OUTPUT_LANGUAGE is set to invalid value ", OUTPUT_LANGUAGE,
                                    ". Change to English, EN, Spanish, SP, French, FR, Portuguese or PT and rerun."))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("Global OUTPUT_LANGUAGE is set to invalid value ", OUTPUT_LANGUAGE,
                            ". Change to English, EN, Spanish, SP, French, FR, Portuguese or PT and rerun."))
    exitflag <- 1
  } else {

    if (file.exists(paste0(VCQI_DATA_FOLDER,"/Multi-Lingual Phrases - En Fr Es Pt.xlsx"))){
      dat <- openxlsx::read.xlsx(xlsxFile = paste0(VCQI_DATA_FOLDER,"/Multi-Lingual Phrases - En Fr Es Pt.xlsx"),
                                 sheet = "Latest version")
    } else {
      errormsgs <- c(errormsgs,paste0("Multi-Lingual file not found in ", VCQI_DATA_FOLDER))
      vcqi_log_comment(VCP,1,"Error", paste0("Multi-Lingual file not found in ", VCQI_DATA_FOLDER))
      exitflag <- 1
    }

    if(exitflag == 1){
      vcqi_global(VCQI_ERROR, 1)
      miss_vcqi_halt_immediately(halt_message = errormsgs)
    }

    names(dat) <- str_to_upper(names(dat))

    var <- get(language,dat)
    var <- haven::zap_label(var)
    var <- haven::zap_labels(var)
    #TODO: double check that when importing xlsx missing str is NA
    if ("character" %in% class(var)){
      if (any(is.na(var)| var == "")){
        errormsgs <- c(errormsgs,paste0("Variable ", language, " is missing values. Populate and rerun."))
        vcqi_log_comment(VCP,1,"Error", paste0("Variable ", language, " is missing values. Populate and rerun."))
        exitflag <- 1
      }
    } else if (any(is.na(var))){
      errormsgs <- c(errormsgs,paste0("Variable ", language, " is missing values. Populate and rerun."))
      vcqi_log_comment(VCP,1,"Error", paste0("Variable ", language, " is missing values. Populate and rerun."))
      exitflag <- 1
    }

  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(halt_message = errormsgs)
  } else {
    assign("language_use",language, envir = .GlobalEnv)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
