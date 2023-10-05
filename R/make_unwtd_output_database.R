#' Create unweighted database results
#'
#' @param variable Outcome variable
#' @param estlabel Label for the estimate variable
#' @param vid Outcome variable ID for filenames
#' @param measureid Analysis indicator ID
#' @param VCP VCQI current program name to be logged, default to be the function name
#' @param ... Other arguments
#'
#' @return A database in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import haven

# make_unwtd_output_database R version 1.00 - Biostat Global Consulting - 2022-12-16
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-16  1.00      Mia Yu          Original R package version
# *******************************************************************************


make_unwtd_output_database <- function(
    variable,
    estlabel,
    vid,
    measureid,
    VCP = "make_unwtd_output_database",
    keepnumerator = FALSE,
    ...){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", measureid, "_", ANALYSIS_COUNTER, ".rds"))

  tempvar <- get(variable, dat)
  templabel <- estlabel
  tempvid <- vid
  tempmeasure <- measureid

  # Log the database being made
  vcqi_log_comment(VCP, 3, "Comment",
                   paste0("measureid: ", measureid,
                          " variable: ", variable,
                          " vid: ", vid,
                          " label: ", estlabel))

  go <- NULL

  if (!vcqi_object_exists("VCQI_DATABASES")){VCQI_DATABASES <- NULL}

  vcqi_global(VCQI_DATABASES,
              c(VCQI_DATABASES,paste0(tempmeasure, "_", ANALYSIS_COUNTER, "_", tempvid, "_database.rds")))

  l <- 4

  for (j in 1:nrow(level4_layout)){

    # Pass along the name and id of the sub-stratum
    l4name <- level4_layout$label[j]
    rowtype <- level4_layout$rowtype[j]

    if (rowtype == "DATA_ROW"){
      condition <- level4_layout$condition[j]

      subdat <- subset(dat, eval(rlang::parse_expr(condition)) &
                         tempvar %in% c(0,1))

      count <- nrow(subdat)
      subvar <- get(variable, subdat)

      if (count > 0){
        gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable, estimate = mean(subvar), n = count)
        go <- rbind(go, gotemp)
      }

      if (count == 0){
        gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable, estimate = 0, n = 0)
        go <- rbind(go, gotemp)
      }
    }

    if (rowtype == "BLANK_ROW"){
      gotemp <- data.frame(level = l, level4id = j, level4name = "BLANK_ROW", outcome = variable, estimate = NA, n = NA)
      go <- rbind(go, gotemp)
    }


    if (rowtype == "LABEL_ONLY"){
      gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable, estimate = NA, n = NA)
      go <- rbind(go, gotemp)
    }
  }

  saveRDS(go, file = paste0(VCQI_OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                            "_", tempvid, "_database.rds"))

  # Now do a little work to put the ids and names of the various stratum
  # levels into the database
  #
  # The database will serve at least two purposes:
  #
  # 1. It can be exported to a flat file or excel file or database and
  #    may be used with mail-merge software to generate reporting forms
  #    in programs like Microsoft Word.  This provides future flexibility.
  #
  # 2. It will serve as the basis of the `measureid'_05TO program that
  #    exports requested records out to Microsoft Excel.

  # We have all the components of the names; make a single name variable that
  # holds what we think would be best to list in a table (but also keep the
  # components)

  dat <- go %>%
    mutate(name = NA,
           level4name = as.character(level4name),
           # Append the name to the front of the level4name if we have a single
           # stratifier; otherwise leave it off.
           name = ifelse(!is.na(level4name), level4name, name),
           name = ifelse(level4name == "BLANK_ROW", NA, name))

  dat <- dat %>%
    relocate(c(name, level4id, level4name), .after = level) %>%
    arrange(level, level4id) # Arranging by level4id no longer matches Stata output

  if (keepnumerator == TRUE){
    dat <- dat %>% mutate(numerator = estimate*n)
    dat$numerator <- haven::labelled(dat$numerator, label = "Numerator") %>% suppressWarnings()
  }

  dat$level <- haven::labelled(dat$level, label = "Stratum level") %>% suppressWarnings()
  dat$level4id <- haven::labelled(dat$level4id, label = "Sub-stratum ID") %>% suppressWarnings()
  dat$name <- haven::labelled(dat$name, label = "Stratum name for table output") %>% suppressWarnings()
  dat$outcome <- haven::labelled(dat$outcome, label = "Outcome variable") %>% suppressWarnings()
  dat$estimate <- haven::labelled(dat$estimate, label = templabel) %>% suppressWarnings()


  saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                      "_", tempvid, "_database.rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
