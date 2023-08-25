#' Create database for DESC_02 and DESC_03
#'
#' @param variable Outcome variable
#' @param label Label for the outcome variable
#' @param vid Outcome variable ID for filenames
#' @param measureid Analysis indicator ID
#' @param printprogress Progress tracking string to print
#' @param VCP VCQI current program name to be logged, default to be the function name
#' @param ... Other arguments
#'
#' @return A database in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import survey
#' @import haven
#' @import stringr

# make_DESC_0203_output_database R version 1.00 - Biostat Global Consulting - 2023-05-25
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-25  1.00      Mia Yu          Original R Package version
# *******************************************************************************

# NOTE: functional, with some details pending (see TO DO notes)

make_DESC_0203_output_database <- function(
    variable,
    label = NA,
    vid,
    measureid,
    printprogress = NULL,
    VCP = "make_DESC_0203_output_database",
    ...){

  if(!is.null(printprogress)){
    print(printprogress)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Starting")
  #browser()
  # This program is used for DESC_02 and DESC_03...sort out which
  # is calling it now and set the appropriate local macro

  templabel <- label
  tempvid <- vid
  tempmeasure <- measureid

  if (tempmeasure == "DESC_02"){
    mid <- "02"
  } else if (tempmeasure == "DESC_03"){
    mid <- "03"
  } else {
    errormsgs <- paste0("MEASUREID should be DESC_02 or DESC_03 to call ",VCP)
    vcqi_log_comment(VCP, 1, "Error",paste0("MEASUREID should be DESC_02 or DESC_03 to call ",VCP))
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Log the database being made
  vcqi_log_comment(VCP, 3, "Comment",
                   paste0("measureid: ", measureid,
                          " variable: ", variable,
                          " vid: ", vid,
                          " label: ", label))
  counter <- get(paste0("DESC_",mid,"_COUNTER"), envir = .GlobalEnv)
  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/DESC_", mid, "_", ANALYSIS_COUNTER, "_", counter, ".rds"))

  # Set survey design based on VCQI_SVYDESIGN_SYNTAX

  weight <- get(paste0("DESC_",mid,"_WEIGHTED"), envir = .GlobalEnv)
  if (str_to_upper(weight) == "YES"){
    svy_design <- get("VCQI_SVYDESIGN_SYNTAX", envir = .GlobalEnv)

    if (substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]] == "1"){
      datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                             weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
    } else {
      clusterid <- get(substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]], dat)

      if (length(unique(clusterid)) == 1){
        datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                               weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
      } else {
        datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                               strata = VCQI_SVYDESIGN_SYNTAX$strata,
                               weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
      }
    }

    wtd <- 1
  }

  if (str_to_upper(weight) == "NO"){
    datdesign <- svydesign(ids = ~1, data = dat) %>% suppressWarnings()
    wtd <- 0
  }

  # Build the lists of variables to post
  vlist <- NULL
  vorder <- NULL
  level_count_without_subtotals <- get(paste0("DESC_",mid,"_LVL_COUNT_",tempvid), envir = .GlobalEnv)

  if (!vcqi_object_exists(paste0("DESC_",mid,"_SHOW_SUBTOTALS_ONLY"))){
    for (i in 1:level_count_without_subtotals){

      if (mid == "03"){
        valuematch <- DESC_03_VARIABLES[i]
      }
      if (mid == "02"){
        valuematch <- get(paste0("DESC02_VALUE_LEVEL_",i), envir = .GlobalEnv)
      }

      # If a subtotal is supposed to be listed *before* this individual response...add it here
      if (vcqi_object_exists(paste0("DESC_",mid,"_ST_COUNT_",tempvid))){
        sub_count <- get(paste0("DESC_",mid,"_ST_COUNT_",tempvid), envir = .GlobalEnv)
        if (sub_count != 0){
          for (k in 1:sub_count){
            if (vcqi_object_exists(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k))){
              sub_list <- get(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k), envir = .GlobalEnv)
              worldmatch = ((word(sub_list,2) == as.character(valuematch)) %in% TRUE) | (is.na(valuematch) & word(sub_list,2) == "NA")
              if (((str_to_upper(word(sub_list,1)) == "BEFORE") %in% TRUE & worldmatch) %in% TRUE){
                vorder <- c(vorder,level_count_without_subtotals + k)
                vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_st",k))
              }
            }
          } #end of sub_count k loop
        }
      }

      # List the individual variable
      vorder <- c(vorder,i)
      vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_",i))

      # If a subtotal is supposed to be listed *after* this individual response...add it here
      if (vcqi_object_exists(paste0("DESC_",mid,"_ST_COUNT_",tempvid))){
        sub_count <- get(paste0("DESC_",mid,"_ST_COUNT_",tempvid), envir = .GlobalEnv)
        if (sub_count != 0){
          for (k in 1:sub_count){
            if (vcqi_object_exists(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k))){
              sub_list <- get(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k), envir = .GlobalEnv)
              worldmatch = (word(sub_list,2) == as.character(valuematch) %in% TRUE) | (is.na(valuematch) & word(sub_list,2) == "NA")
              if ((str_to_upper(word(sub_list,1)) == "AFTER" %in% TRUE & worldmatch) %in% TRUE){
                vorder <- c(vorder,level_count_without_subtotals + k)
                vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_st",k))
              }
            }
          } #end of sub_count k loop
        }
      }

    }#end of level_count_without_subtotals i loop

    assign(paste0("DESC_",mid,"_VORDER"),vorder, envir = .GlobalEnv)
  }

  # If we haven't already listed this subtotal above either before or after an individual response, then list it here
  if (vcqi_object_exists(paste0("DESC_",mid,"_ST_COUNT_",tempvid))){
    sub_count <- get(paste0("DESC_",mid,"_ST_COUNT_",tempvid), envir = .GlobalEnv)
    if (sub_count != 0){
      for (k in 1:sub_count){
        if (!vcqi_object_exists(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k))){
          vorder <- c(vorder,level_count_without_subtotals + k)
          vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_st",k))
        }
      } #end of sub_count k loop

      assign(paste0("DESC_",mid,"_VORDER"),vorder, envir = .GlobalEnv)
    }
  }

  go <- NULL
  DESC_labels <- NULL

  # Record and save the labels for pct_*
  for (k in seq_along(vlist)){
    varlabel <- attr(get(vlist[k], dat),"label")
    label_temp <- data.frame(var = paste0("pct",vorder[k]), label = varlabel)
    DESC_labels <- rbind(DESC_labels,label_temp)
  }
  assign(paste0("DESC_",mid,"_labels_",tempvid),DESC_labels, envir = .GlobalEnv)


  l <- 4
  for (j in 1:nrow(level4_layout)){
    # Pass along the name and id of the sub-stratum
    l4name <- level4_layout$label[j]
    rowtype <- level4_layout$rowtype[j]

    #weighted case
    if (wtd == 1){
      if (rowtype == "DATA_ROW"){

        #First build gotemp
        gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA, !!paste0("cill",vorder[o]) := NA, !!paste0("ciul",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA, nwtd = NA)

        condition <- level4_layout$condition[j]

        for (k in seq_along(vlist)){
          tempvar <- get(vlist[k], dat)
          varlabel <- attr(tempvar,"label")
          # Count respondents meeting the level4 condition(s)
          count <- subset(dat, eval(rlang::parse_expr(condition)) &
                            tempvar %in% c(0,1)) %>% nrow()

          # Only do the calculation and put out the results if there are
          # respondents in this sub-stratum
          if (count > 0){
            ptest <- svypd(
              svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
              var = vlist[k],
              subset_condition = condition,
              ci_level = 95,
              ci_method = VCQI_CI_METHOD,
              adjust = TRUE,
              truncate = TRUE
            )

            gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[k]) := ptest$estimate,
                                        !!paste0("cill",vorder[k]) := ptest$cill,
                                        !!paste0("ciul",vorder[k]) := ptest$ciul)

          } #end of count

        } #end of vlist k loop

        gotemp <- gotemp %>% mutate(n = ptest$n, nwtd = ptest$nwtd)
        go <- rbind(go, gotemp)
      }

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(level = l, level4id = j, level4name = "BLANK_ROW", outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA, !!paste0("cill",vorder[o]) := NA, !!paste0("ciul",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA, nwtd = NA)
        go <- rbind(go, gotemp)
      }

      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA, !!paste0("cill",vorder[o]) := NA, !!paste0("ciul",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA, nwtd = NA)
        go <- rbind(go, gotemp)
      }
    } #end of weighted case

    if (wtd == 0){

      if (rowtype == "DATA_ROW"){

        # First build gotemp
        gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)

        condition <- level4_layout$condition[j]

        for (k in seq_along(vlist)){
          tempvar <- get(vlist[k], dat)
          # Count respondents meeting the level4 condition(s)
          count <- subset(dat, eval(rlang::parse_expr(condition)) &
                            tempvar %in% c(0,1)) %>% nrow()

          # Only do the calculation and put out the results if there are
          # respondents in this sub-stratum
          if (count > 0){
            ptest <- svypd(
              svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
              var = vlist[k],
              subset_condition = condition,
              ci_level = 95,
              ci_method = VCQI_CI_METHOD,
              adjust = TRUE,
              truncate = TRUE
            )
            gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[k]) := ptest$estimate)
          } #end of count

        } #end of vlist k loop
        if (count == 0){
          gotemp <- gotemp %>% mutate(n = count)
        } else {
          gotemp <- gotemp %>% mutate(n = ptest$n)
        }
        go <- rbind(go, gotemp)
      }

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(level = l, level4id = j, level4name = "BLANK_ROW", outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)
        go <- rbind(go, gotemp)
      }


      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)
        go <- rbind(go, gotemp)
      }
    } #end of unweighted case
  } #end of j loop

  if (counter < 10){
    counter <- paste0("0",counter)
  }

  filename <- paste0(VCQI_OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                     "_", counter,"_",tempvid, "_database.rds")
  saveRDS(go, filename)

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

  # Label variable name "Survey name for table output"
  dat <- dat %>%
    relocate(c(name, level4id, level4name), .after = level) %>%
    arrange(level, level4id) # Arranging by level4id no longer matches Stata output

  # Save these variables to the database for future reference...
  weight_option <- get(paste0("DESC_",mid,"_WEIGHTED"), envir = .GlobalEnv)
  den_option <- get(paste0("DESC_",mid,"_DENOMINATOR"), envir = .GlobalEnv)
  dat <- dat %>% mutate(weighted = weight_option, denominator = den_option)

#   if "${DESC_`mid'_SHOW_SUBTOTALS_ONLY"}" == "" {
#
# 			forvalues i = 1/${DESC_`mid'_LVL_COUNT_`vid'} {
# 				label variable pct`i' "`vlabel`i''"
# 			}
# 		}
#
# 		forvalues i = 1/${DESC_`mid'_ST_COUNT_`vid'} {
# 			local j `=${DESC_`mid'_LVL_COUNT_`vid'}+`i''
# 			label variable pct`j' "`vlabel`j''"
# 		}

  dat$level <- haven::labelled(dat$level, label = "Stratum level") %>% suppressWarnings()
  dat$level4id <- haven::labelled(dat$level4id, label = "Sub-stratum ID") %>% suppressWarnings()
  dat$name <- haven::labelled(dat$name, label = "Stratum name for table output") %>% suppressWarnings()
  if (str_length(templabel) > 80){
    comment(dat$outcome) <- templabel
    dat$outcome <- haven::labelled(dat$outcome, label = "Table title is very long so it is stored in comment") %>% suppressWarnings()
  } else{
    dat$outcome <- haven::labelled(dat$outcome, label = templabel) %>% suppressWarnings()
  }
  dat$weighted <- haven::labelled(dat$weighted, label = "Are the percentages weighted?") %>% suppressWarnings()
  dat$denominator <- haven::labelled(dat$denominator, label = "Which respondents are in the denominator?") %>% suppressWarnings()

  saveRDS(dat, filename)

  if (!vcqi_object_exists("VCQI_DATABASES")){VCQI_DATABASES <- NULL}

  vcqi_global(VCQI_DATABASES,
              c(VCQI_DATABASES,paste0(tempmeasure, "_", ANALYSIS_COUNTER,"_", counter,"_",tempvid, "_database.rds")))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
