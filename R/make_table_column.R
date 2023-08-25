#' Extract a column of data from a database and save it to a table to be exported later
#'
#' @param tablename The name of the table (usually TO_indicator name)
#' @param dbfilename Value of the database file that holds the column (not a VAD)
#' @param variable Name of the variable to put in the column
#' @param varformat The format of the variable
#' @param replacevar Name of the variable(s) you want to remove/replace
#' @param withvar Name of the variable(s) you want to replace with
#' @param noannotate Annotate the variable with parentheses or * based on annovar or not, default to be FALSE
#' @param annovar Name of the variable to use for deciding whether to annotate the output
#' @param label Label of the variable
#' @param annoparenval Threshold for annotating results: results based on samples with fewer than annoparenval observations will be surrounded by parentheses (default = 50)
#' @param annosuppval Threshold for suppressing results: results based on samples with fewer than annosuppval observations will be suppress (default = 25)
#' @param noscale Do not scale the variable to % (the default value of FALSE means results will be put on the percentage scale)
#'
#' @return A dataset augmented with an additional table column
#'
#' @import stringr
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))

# make_table_column R version 1.06 - Biostat Global Consulting - 2022-10-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-02  1.00      Mia Yu          Original R version
# 2022-09-07  1.01      Mia Yu          Add parts to record format
# 2022-09-08  1.02      Mia Yu          Add a part that gives a warning messeage
#                                       if the dbfile is not valid
# 2022-09-20  1.03      Mia Yu          Adapt to allow empty data frame
# 2022-10-08  1.04      Mia Yu          Package version
# 2022-10-11  1.05      Mia Yu          Update the usage of vcqi_object_exists
# 2022-10-24  1.06      Mia Yu          Add vcqi_halt_immediately
# *******************************************************************************

make_table_column <- function(
    #required
    tablename,
    dbfilename,
    variable,
    #not required
    varformat = NA,
    #if the variable is ci then varformat needs to be list(width = , digits = )
    #if the variable is other numeric variable the varformat needs to be list(numFmt)
    #where numFmt is an option in createStyle
    replacevar = NA,
    withvar = NA,
    noannotate = FALSE,
    annovar = NA,
    label = NA,
    annoparenval = 50,
    annosuppval = 25,
    noscale = FALSE){

  # Copy the input
  tablename <- str_to_upper(tablename)
  tempvar <- str_to_lower(variable)
  templabel <- label

  # Create/get the table
  if (exists(tablename, envir = .GlobalEnv) & exists(paste0(tablename,"_CN"), envir = .GlobalEnv)){
    temptable <- get(tablename, envir = .GlobalEnv)
  }

  if (!exists(tablename, envir = .GlobalEnv) | !exists(paste0(tablename,"_CN"), envir = .GlobalEnv)){
    temptable <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", dbfilename))
    if (!is.data.frame(temptable) | nrow(temptable) == 0){
      errormsgs <- paste0("The file", dbfilename, "is not in valid format or it is an empty dataset")
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    } else{
      #NOTE: in Stata we used level4name but since in VCQI in R is level4 only we use the variable name here
      temptable <- temptable %>% select(c(level4id,name)) %>% arrange(level4id) %>%
        mutate(name = ifelse((name == "BLANK_ROW") %in% TRUE, NA, name))
      assign(paste0(tablename,"_CN"), 2,envir = .GlobalEnv)
    }
  }

  if (any(is.na(varformat))){
    if (tempvar %in% c("estimate", "stderr", "cill", "ciul", "lcb", "ucb")){
      formatnum <- 1
      tempformat <- list(NA)
    } else if (tempvar == "ci"){
      formatnum <- NA
      tempformat <- list(width = 5, digits = VCQI_NUM_DECIMAL_DIGITS)
    } else if (tempvar == "icc"){
      formatnum <- 2
      tempformat <- list(NA)
    } else if (tempvar %in% c("n", "nwtd", "nwtd_est")){
      formatnum <- 3
      tempformat <- list(NA)
    } else if (tempvar == "deff"){
      formatnum <- 4
      tempformat <- list(NA)
    } else {
      formatnum <- 4
      tempformat <- list(NA)
    }
  } else {
    if (tempvar == "ci"){
      formatnum <- NA
      tempformat <- varformat
    } else {
      formatnum <- 5
      tempformat <- varformat
    }
  }

  # If label was not provided for common variables, lets set those
  if (is.na(templabel)){
    if (tempvar == "stderr"){
      templabel = "StdErr (%)"
    }
    if (tempvar == "lcb"){
      templabel = "95% LCB (%)"
    }
    if (tempvar == "ucb"){
      templabel = "95% UCB (%)"
    }
    if (tempvar == "deff"){
      templabel = "DEFF"
    }
    if (tempvar == "icc"){
      templabel = "ICC"
    }
    if (tempvar == "n"){
      templabel = "N"
    }
    if (tempvar == "nwtd"){
      templabel = "Weighted N"
    }
  }

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/",dbfilename))

  if (!vcqi_object_exists(paste0(tablename,"_CN"))){
    colnum <- 0
  } else{
    colnum <- get(paste0(tablename,"_CN"),envir = .GlobalEnv)
  }
  colnum = colnum + 1

  assign(paste0(tablename,"_CN"),colnum,envir = .GlobalEnv)


  colname <- paste0("c",colnum)
  make_empty_column <- 0

  if (nrow(dat) == 0){
    warning(paste0("No rows in the dataset match the input list criteria.  The column named ",
                    colname," will be blank."))
    make_empty_column <- 1
  } else if (any(duplicated(dat$level4id)) %in% TRUE) {
    warning(paste0("Input criteria do not result in rows with unique values of level4id. The column named ",
                    colname," will be blank."))
    make_empty_column <- 1
  }

  if(tempvar == "ci"){
    if (!("cill" %in% names(dat)) | !("ciul" %in% names(dat))){
      warning(paste0("User requested variable ci but its constituents, cill and ciul were not found in ",
                      dbfilename,".  The column named ",colname," will be blank."))
      make_empty_column <- 1
    }
  } else{
    if (!(tempvar %in% names(dat))){
      warning(paste0("The variable ", tempvar, " is not found in the ",
                      dbfilename,".  The column named ",colname," will be blank."))
      make_empty_column <- 1
    }
  }

  # User has the opportunity to specify a variable other than n
  # to use for annotating or suppressing results from small groups
  #
  # If they do not specify the option, the program defaults to using
  # a variable named n.

  if (is.na(annovar)){
    tempannovar <- "n"
  } else{
    tempannovar = annovar
  }

  if (make_empty_column == 1){
    if (nrow(dat) == 0){
      dat <- data.frame(tempvar = rep(NA, nrow(temptable)))
      names(dat)[which(names(dat) == "tempvar")] <- colname
      row.names(dat) <- row.names(temptable)
      temptable <- cbind(temptable,dat)
    } else{
      dat <- dat %>% select(c(level4id))
      dat <- unique(dat)
      dat <- dat %>% arrange(level4id) %>% mutate(tempvar = NA) %>%
        relocate(c(level4id,tempvar),.after = last_col())
      names(dat)[which(names(dat) == "tempvar")] <- colname

      # tempfile column
      # save `column', replace
      # if "$TOST_DEBUG" == "1" capture save `table'_${ANALYSIS_COUNTER}_`colname', replace

      temptable <- inner_join(temptable, dat, by = "level4id")
    }


    if (!vcqi_object_exists(paste0(tablename,"_columnlabel"))){
      assign(paste0(tablename,"_columnlabel"), NULL, envir = .GlobalEnv)
    }

    #append templabel to columnlabel
    columnlabel <- get(paste0(tablename,"_columnlabel"))
    columnlabel <- c(columnlabel,templabel)
    assign(paste0(tablename,"_columnlabel"), columnlabel, envir = .GlobalEnv)

    #assign temptable back to the original table
    assign(tablename,temptable,envir = .GlobalEnv)

    #append formatnum to colformatnum
    if (!vcqi_object_exists(paste0(tablename,"_formatnum"))){
      assign(paste0(tablename,"_formatnum"), NULL, envir = .GlobalEnv)
    }
    colformatnum <- get(paste0(tablename,"_formatnum"))
    colformatnum <- c(colformatnum,NA)
    assign(paste0(tablename,"_formatnum"), colformatnum, envir = .GlobalEnv)

    #append tempformat to colformat
    if (!vcqi_object_exists(paste0(tablename,"_colformat"))){
      assign(paste0(tablename,"_colformat"), NULL, envir = .GlobalEnv)
    }
    colformat <- get(paste0(tablename,"_colformat"))
    colformat <- append(colformat,NA)
    assign(paste0(tablename,"_colformat"), colformat, envir = .GlobalEnv)

  } else {
    if (tempvar %in% c("stderr","cill","ciul","lcb","ucb")){
      eval(parse_expr(paste0("dat <- mutate(dat,",tempvar," = 100 * ",tempvar,")")))
    }
    if (tempvar == "estimate" & noscale == FALSE){
      dat <- mutate(dat, estimate = 100 * estimate)
    }
    if (tempvar == "ci"){
      #NOTE: In R, digits means the total digits the number can have which is w in Stata format.
      #      For now, we only controlled the number of digits after the decimal point
      dat <- dat %>% mutate(cill = 100 * cill,
                            ciul = 100 * ciul) %>%
        mutate(tempcill = sprintf(paste0("%.",tempformat$digits,"f"), cill),
               tempciul = sprintf(paste0("%.",tempformat$digits,"f"), ciul)) %>%
        mutate(tempciul = ifelse(round(ciul) == 100, "100", tempciul)) %>%
        mutate(ci = ifelse(!is.na(cill) & !is.na(ciul), paste0("(" ,tempcill,", ",tempciul,")"), NA)) %>%
        select(-c(tempcill,tempciul))
      templabel = "95% CI (%)"
    }

    if (noannotate == FALSE){
      if (annosuppval > annoparenval){
        warning(paste0("You have specified a value of annosuppval (",annosuppval,
                        ") that is >= the value of annoparenval (",annoparenval,")."))
        warning(paste0("This will result in no values ever being wrapped in parentheses and all values based on ",
                        tempannovar," < ",annosuppval," will be suppressed."))
      } else {
        #annotate the variable
        vartochange <- rlang::sym(tempvar)
        dat <- dat %>% mutate(changedvar = !!vartochange) %>%
          mutate(changedvar = ifelse((!!vartochange >= annosuppval & !!vartochange < annoparenval) %in% TRUE,
                                     paste0("(",changedvar, ")"), changedvar)) %>%
          mutate(changedvar = ifelse(!!vartochange < annosuppval, "(*)", changedvar))
        #rename the column
        names(dat)[which(names(dat) == "changedvar")] <- colname
      }
    } else{
      #generate colname
      varibaletoget <- rlang::sym(tempvar)
      dat <- dat %>% mutate(changedvar = !!varibaletoget)
      #rename the column
      names(dat)[which(names(dat) == "changedvar")] <- colname
    }

    #label var `colname' "`: var label `variable''"  // use the db var label by default
    #if "`label'" != "" capture label variable `colname' `"`label'"'	// allow user to override the default label


    # If user specifies a value to replace and what to replace it with, do that here.
    # Note that the option with() can be blank, in which case the replacement string is empty.
    # This is often what we want.
    if (!is.na(replacevar)){
      vartoreplace <- rlang::sym(replacevar)
      varreplacewith <- rlang::sym(withvar)
      dat <- mutate(dat, !!vartoreplace := ifelse(gsub(" ","",colname) == replacevar,
                                                 !!varreplacewith, !!vartoreplace))
    }

    dat <- dat %>% arrange(level4id) %>% select(c(level4id,all_of(colname))) %>%
      relocate(c(level4id,all_of(colname)),.after = last_col())

    # tempfile column
    # save `column', replace
	  # if "$TOST_DEBUG" == "1" capture save `table'_${ANALYSIS_COUNTER}_`colname', replace

    temptable <- inner_join(temptable, dat, by = "level4id")

    if (!vcqi_object_exists(paste0(tablename,"_columnlabel"))){
      assign(paste0(tablename,"_columnlabel"), NULL, envir = .GlobalEnv)
    }

    #append templabel to columnlabel
    columnlabel <- get(paste0(tablename,"_columnlabel"), envir = .GlobalEnv)
    columnlabel <- c(columnlabel,templabel)
    assign(paste0(tablename,"_columnlabel"), columnlabel, envir = .GlobalEnv)

    #assign temptable back to the original table
    assign(tablename,temptable,envir = .GlobalEnv)

    #append formatnum to colformatnum
    if (!vcqi_object_exists(paste0(tablename,"_formatnum"))){
      assign(paste0(tablename,"_formatnum"), NULL, envir = .GlobalEnv)
    }
    colformatnum <- get(paste0(tablename,"_formatnum"), envir = .GlobalEnv)
    colformatnum <- c(colformatnum,formatnum)
    assign(paste0(tablename,"_formatnum"), colformatnum, envir = .GlobalEnv)

    #append tempformat to colformat
    if (!vcqi_object_exists(paste0(tablename,"_colformat"))){
      assign(paste0(tablename,"_colformat"), NULL, envir = .GlobalEnv)
    }
    colformat <- get(paste0(tablename,"_colformat"), envir = .GlobalEnv)
    colformat <- append(colformat, list(tempformat))
    assign(paste0(tablename,"_colformat"), colformat, envir = .GlobalEnv)

  } #end of check make_empty_stable_column

}
