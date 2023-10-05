#' Create database with estimated counts
#'
#' @param numerator The variable to use as the numerator
#' @param denominator The variable to use as the denominator
#' @param estlabel Label for the estimate variable
#' @param vid Variable id
#' @param measureid Analysis indicator id
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A database in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import haven

# make_count_output_database R version 1.02 - Biostat Global Consulting - 2022-10-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# 2022-10-20  1.02      Mia Yu          Add variable labels
# *******************************************************************************

make_count_output_database <- function(numerator, denominator, estlabel, vid, measureid,
                                       VCP = "make_count_output_database",keepnumerator = FALSE){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", measureid, "_", ANALYSIS_COUNTER, ".rds"))

  templabel <- estlabel
  tempvid <- vid
  tempmeasure <- measureid

  vcqi_log_comment(VCP, 3, "Comment",
                   paste0("measureid: ", measureid,
                          " numerator: ", numerator,
                          " denominator", denominator,
                          " vid: ", vid,
                          " label: ", estlabel))

  go <- NULL

  if (!vcqi_object_exists("VCQI_DATABASES")){VCQI_DATABASES <- NULL}

  vcqi_global(VCQI_DATABASES,
              c(VCQI_DATABASES, paste0(tempmeasure, "_", ANALYSIS_COUNTER, "_", tempvid, "_database.rds")))

  l <- 4
  for (j in 1:nrow(level4_layout)){
    l4name <- level4_layout$label[j]
    rowtype <- level4_layout$rowtype[j]

    if (rowtype == "DATA_ROW"){
      condition <- level4_layout$condition[j]

      # TO DO - check if this process accommodates more complex filter conditions
      # Count respondents meeting the level4 condition(s)
      subdat <- subset(dat, eval(rlang::parse_expr(condition)))
      tempnum <- get(numerator, subdat)
      tempden <- get(denominator, subdat)

      #TO DO - double check that NAs are removed in Stata
      den <- sum(tempden,na.rm = TRUE)
      num <- sum(tempnum,na.rm = TRUE)

      if (den > 0){
        tempestimate = num/den
      }


      if (den == 0){
        if (keepnumerator == TRUE){
          gotemp <- data.frame(
            level = l, level4id = j, level4name = l4name, outcome = paste0(numerator," / ",denominator),
            estimate = 0, n = 0, numerator = 0
          )
        } else {
          gotemp <- data.frame(
            level = l, level4id = j, level4name = l4name, outcome = paste0(numerator," / ",denominator),
            estimate = 0, n = 0
          )
        }

        go <- rbind(go,gotemp)
      }

      if (den > 0){
        if (keepnumerator == TRUE){
          gotemp <- data.frame(
            level = l, level4id = j, level4name = l4name, outcome = paste0(numerator," / ",denominator),
            estimate = tempestimate, n = den, numerator = num
          )
        } else {
          gotemp <- data.frame(
            level = l, level4id = j, level4name = l4name, outcome = paste0(numerator," / ",denominator),
            estimate = tempestimate, n = den
          )
        }

        go <- rbind(go,gotemp)
      }

  } #end of data_row

    if (rowtype == "BLANK_ROW"){
      if (keepnumerator == TRUE){
        gotemp <- data.frame(
          level = l, level4id = j, level4name = "BLANK_ROW", outcome = paste0(numerator," / ",denominator),
          estimate = NA, n = NA, numerator = NA
        )
      } else {
        gotemp <- data.frame(
          level = l, level4id = j, level4name = "BLANK_ROW", outcome = paste0(numerator," / ",denominator),
          estimate = NA, n = NA
        )
      }

      go <- rbind(go,gotemp)
    } #end of blank_row

    if (rowtype == "LABEL_ONLY"){
      if (keepnumerator == TRUE){
        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = paste0(numerator," / ",denominator),
          estimate = NA, n = NA, numerator = NA
        )
      } else {
        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = paste0(numerator," / ",denominator),
          estimate = NA, n = NA
        )
      }

      go <- rbind(go,gotemp)
    } #end of label_only

  } #end of j loop

  filename <- paste0(VCQI_OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                     "_", tempvid, "_database.rds")
  saveRDS(go, filename)

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

  dat$level <- haven::labelled(dat$level, label = "Stratum level") %>% suppressWarnings()
  dat$level4id <- haven::labelled(dat$level4id, label = "Sub-stratum ID") %>% suppressWarnings()
  dat$name <- haven::labelled(dat$name, label = "Stratum name for table output") %>% suppressWarnings()
  dat$outcome <- haven::labelled(dat$outcome, label = "Outcome") %>% suppressWarnings()
  dat$estimate <- haven::labelled(dat$estimate, label = templabel) %>% suppressWarnings()
  if ("numerator" %in% names(dat)){
    dat$numerator <- haven::labelled(dat$numerator, label = "Numerator") %>% suppressWarnings()
  }

  saveRDS(dat, filename)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
