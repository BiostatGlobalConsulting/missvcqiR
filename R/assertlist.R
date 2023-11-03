#' This program mimics Stata's assertlist program. It lists how many dataset rows contradict the assertion, which row contradict the assertion, and how.
#'
#' @param dat the datast the user wants to check an assertion
#' @param var the variable the user wants  to to check an assertion
#' @param f An ogical expression that resolves to either TRUE or FALSE for each row of the dataset. All rows where the expression is FALSE will be displayed on the screen (default) or sent to an EXCEL spreadsheet.
#' @param condition An ogical expression that resolves to either TRUE or FALSE for each row of the dataset. If specified, the assertion check will only be applied to the subset of rows that meet the condition.
#' @param idlist List of variables that uniquely identify each observation.  These variables will be included in the replace syntax for corrections.
#' @param checklist List of variables used in exp that you may wish to correct later.  Every variable listed here will receive extra columns in the spreadsheet to facilitate corrections.
#' @param tag User-specified string to list with the output (Often a short description of what you tested and why.)
#' @param assignto The name of the datafram that holds the detailed results of assertlist check
#' @param combinewith The name of the dataframe that the user wants to combines detailed results from previous run with; usually the same as assignto
#' @param overview The name of the dataframe that holds the assertlist summary table
#' @param sum_table The name of the dataframe that the user wants to combines assertlist summary from previous run with; usually the same as overview
#' @param fix TRUE or FALSE, default to be FALSE. If set to be true, asserlist generates additional columns to help data managers correct (or 'fix') errant data values
#'
#' @return Two datasets: one lists detailed assert results including which row contradict the assertion and how if any, one lits assertion summary including how many observations in the dataset contradict the assertion
#'
#' @import dplyr
#' @import tidyselect
#' @rawNamespace import(rlang, except = c(local_options,with_options))

# assertlist R version 1.00 - Biostat Global Consulting - 2023-11-01
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-11-01  1.00      Mia Yu          Original R package version
# *******************************************************************************

assertlist <-
  function(dat,
           var,
           f,
           condition = NA,
           idlist = vector("double", 0),
           checklist = vector("double", 0),
           tag = NA,
           assignto = "assertlist_output",
           combinewith = "assertlist_output",
           overview = "assertlist_summary",
           summary = "assertlist_summary",
           fix = FALSE) {

    if (!is.na(condition)) {
      dat <- dat %>% filter(eval(parse_expr(condition)) %in% TRUE)
    } #filter the data with the condition provided

    rownum <-
      which(eval(parse_expr(f)) == FALSE) #find the rows that failed the assertion

    if (length(rownum) > 0) {
      out_data <- data.frame()
      for (i in rownum) {
        temp <- data.frame(matrix(nrow = 1, ncol = 1))
        for (v in seq_along(var)) {
          list_var <- data.frame(matrix(nrow = 1, ncol = 3))
          list_var[, 1] <- var[v]
          listvalue <- get(var[v], dat)
          list_var[, 2] <- as.character(listvalue[i])
          names(list_var) <- c(paste0("list_var", v), paste0("list_value", v), paste0("list_correct", v))
          temp <- cbind(temp, list_var)
        }
        temp <- data.frame(temp[,-1])

        temp <-
          mutate(temp, function_used = f, tag_used = tag) #get the information for var

        if (length(checklist) > 0) {
          check_var <- data.frame(matrix(nrow = 1, ncol = 1))
          for (l in seq_along(checklist)) {
            check_var_temp <- data.frame(matrix(nrow = 1, ncol = 2))
            check_var_temp[, 1] <- checklist[l]
            value <- get(checklist[l], dat)
            check_var_temp[, 2] <- as.character(value[i])
            names(check_var_temp) <- c(paste0("check_var", l), paste0("check_value", l))
            check_var <- cbind(check_var, check_var_temp)
          }
          check_var <- data.frame(check_var[,-1])
          row.names(check_var) <- row.names(temp)
          temp <-
            cbind(temp, check_var) #get the information for check_var
        }


        if (length(idlist) > 0) {
          id_var <- data.frame(matrix(nrow = 1, ncol = 1))
          for (l in seq_along(idlist)) {
            id_var_temp <- data.frame(matrix(nrow = 1, ncol = 1))
            value <- get(idlist[l], dat)
            id_var_temp[, 1] <- as.character(value[i])
            # names(id_var_temp) = idlist[l]
            id_var <- cbind(id_var, id_var_temp)
          }
          id_var <- data.frame(id_var[, -1])
          names(id_var) <- idlist
          row.names(id_var) <- row.names(temp)
          temp <- cbind(id_var, temp)
        }

        out_data <- rbind(out_data, temp)
      }

      if (fix == FALSE){
        out_data <- out_data %>% select(-c(starts_with("list_correct")))
      }

      if (exists(combinewith, envir = .GlobalEnv)) {
        combinedf <- get(combinewith, envir = .GlobalEnv)

        out_data <- bind_rows(combinedf, out_data)

      } else {
        out_data <- out_data
      }

      assign(assignto, out_data, envir = .GlobalEnv)
    }


    sum_table = data.frame(
      assertion = f,
      additional_inf = tag,
      totalN = as.numeric(nrow(dat)),
      failN = as.numeric(length(rownum)),
      note = NA,
      idvars = paste(idlist, collapse = ","),
      checkvars = paste(checklist, collapse = ",")
    )
    sum_table = mutate(
      sum_table,
      note = case_when(
        failN == 0 ~ "All observations passed the assertion.",
        failN == 1 ~  "1 observation failed the assertion",
        failN > 1 ~ paste0(failN, " observations failed the assertion")
      )
    )
    sum_table = mutate(sum_table,
                       passN = as.numeric(nrow(dat)) - as.numeric(length(rownum)),
                       .before = note)
    if (exists(summary, envir = .GlobalEnv)) {
      sumdf <- get(summary, envir = .GlobalEnv)

      sum_table <- bind_rows(sumdf, sum_table)

    } else {
      sum_table <- sum_table
    }

    assign(overview, sum_table, envir = .GlobalEnv)
  }
