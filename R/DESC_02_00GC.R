#' Check global macros for DESC_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr

# DESC_02_00GC R version 1.01 - Biostat Global Consulting - 2023-09-28
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-27  1.00      Mia Yu          Original R package version
# 2023-09-28  1.01      Mia Yu          Added multi lingual globals for N and NWTD
# *******************************************************************************

DESC_02_00GC <- function(VCP = "DESC_02_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  if (!vcqi_object_exists("DESC_02_DATASET")){
    errormsgs <- c(errormsgs, "You must specify DESC_02_DATASET.")
    vcqi_log_comment(VCP, 1, "Error", "You must specify DESC_02_DATASET.")
    exitflag <- 1
  }

  # Exit out for errors once here since we will use DESC_02_DATASET later
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Make this code backward compatible; in the old days the user simply
  # specified RI, TT or SIA for the dataset string and this code assumed
  # that the dataset was equal to that string plus _with_ids.dta.
  # Now the user is encouraged to specify the name of the dataset explicitly
  # but for backward compatibility, if they specify only RI, TT or SIA then
  # we check to see if the _with_ids.dta dataset exists.  If it does, we
  # concatenate the string _with_ids onto the dataset name global.  If it does
  # not exist, but there is a dataset named RI or TT or SIA then we do not
  # concatenate onto the global.

  # Strip off the .dta if the user provided it
  datname <- gsub(".rds","",DESC_02_DATASET)

  if (str_to_upper(datname) %in% c("RI","SIA", "TT")){
    filename <- paste0(VCQI_OUTPUT_FOLDER, "/", datname,"_with_ids.rds")
    if (file.exists(filename)){
      vcqi_global(DESC_02_DATASET,paste0(datname,"_with_ids.rds"))
    }

    if (!file.exists(filename)){
      if (!file.exists(paste0(VCQI_OUTPUT_FOLDER, "/", datname,".rds"))){
        errormsgs <- c(errormsgs,
                       paste0("DESC_02_DATASET is ",datname, " but there is no dataset named ",
                              datname, " or named ",datname,"_with_ids in the VCQI output folder."))
        vcqi_log_comment(VCP, 1, "Error",
                         paste0("DESC_02_DATASET is ",datname, " but there is no dataset named ",
                                datname, " or named ",datname,"_with_ids in the VCQI output folder."))
        exitflag <- 1
      } else{
        vcqi_global(DESC_02_DATASET,paste0(datname,".rds"))
      }
    }
  }

  # Confirm dataset exists
  if (!file.exists(paste0(VCQI_OUTPUT_FOLDER, "/", DESC_02_DATASET))){
    errormsgs <- c(errormsgs,
                   paste0(VCQI_OUTPUT_FOLDER, "/", DESC_02_DATASET," does not exist."))
    vcqi_log_comment(VCP, 1, "Error",
                     paste0(VCQI_OUTPUT_FOLDER, "/", DESC_02_DATASET," does not exist."))
    exitflag <- 1
  } else {
    dat <- vcqi_read(file = paste0(VCQI_OUTPUT_FOLDER, "/", DESC_02_DATASET))
  }

  # Confirm variables have been provided
  if (!vcqi_object_exists("DESC_02_VARIABLES")){
    errormsgs <- c(errormsgs, "You must define global variable DESC_02_VARIABLES")
    vcqi_log_comment(VCP, 1, "Error", "You must define global variable DESC_02_VARIABLES")
    exitflag <- 1
  }

  # Exit out for errors once here since we will use dat in the following code
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  for (g in seq_along(DESC_02_VARIABLES)){
    if (!DESC_02_VARIABLES[g] %in% names(dat)){
      errormsgs <- c(errormsgs,paste0("The variable ",DESC_02_VARIABLES[g],
            " provided in global macro DESC_02_VARIABLES does not exist ",
            "in dataset and will not be included in the output."))
      vcqi_log_comment(VCP,1,"Error",
        paste0("The variable ",DESC_02_VARIABLES[g],
          " provided in global macro DESC_02_VARIABLES does not exist ",
          "in dataset and will not be included in the output."))
      exitflag <- 1 #TODO: this was commented out in Stata and I wasn't sure why
    }
  } #end of DESC_02_VARIABLES g loop

  # Confirm DESC_02_WEIGHTED is valid & defined
  if (!str_to_upper(DESC_02_WEIGHTED) %in% c("YES", "NO")){
    errormsgs <- c(errormsgs,paste0("DESC_02_WEIGHTED must be YES or NO.  The current value is ",
                                    DESC_02_WEIGHTED))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("DESC_02_WEIGHTED must be YES or NO.  The current value is ",
                            DESC_02_WEIGHTED))
    exitflag <- 1
  }

  # R only:
  if (vcqi_object_exists("DESC_02_SHOW_SUBTOTALS_ONLY")){
    if (!str_to_upper(DESC_02_SHOW_SUBTOTALS_ONLY) %in% "YES"){
      errormsgs <- c(errormsgs,paste0("DESC_02_SHOW_SUBTOTALS_ONLY must be YES or NA. The current value is ",
                                      DESC_02_SHOW_SUBTOTALS_ONLY))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("DESC_02_SHOW_SUBTOTALS_ONLY must be YES or NA. The current value is ",
                              DESC_02_SHOW_SUBTOTALS_ONLY))
      exitflag <- 1
    }
  }
  if (vcqi_object_exists("DESC_02_LIST_N_BEFORE_PCT")){
    if (!str_to_upper(DESC_02_LIST_N_BEFORE_PCT) %in% "YES"){
      errormsgs <- c(errormsgs,paste0("DESC_02_LIST_N_BEFORE_PCT must be YES or NA. The current value is ",
                                      DESC_02_LIST_N_BEFORE_PCT))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("DESC_02_LIST_N_BEFORE_PCT must be YES or NA. The current value is ",
                              DESC_02_LIST_N_BEFORE_PCT))
      exitflag <- 1
    }
  }
  if (vcqi_object_exists("DESC_02_LIST_NWTD_BEFORE_PCT")){
    if (!str_to_upper(DESC_02_LIST_NWTD_BEFORE_PCT) %in% "YES"){
      errormsgs <- c(errormsgs,paste0("DESC_02_LIST_NWTD_BEFORE_PCT must be YES or NA. The current value is ",
                                      DESC_02_LIST_NWTD_BEFORE_PCT))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("DESC_02_LIST_NWTD_BEFORE_PCT must be YES or NA. The current value is ",
                              DESC_02_LIST_NWTD_BEFORE_PCT))
      exitflag <- 1
    }
  }

  # Set default N and NWTD labels if not specified

  if (!vcqi_object_exists("DESC_02_N_LABEL")){
    vcqi_global(DESC_02_N_LABEL, language_string(language_use = language_use, str = "OS_48")) #N
  }
  if (!vcqi_object_exists("DESC_02_NWTD_LABEL")){
    vcqi_global(DESC_02_NWTD_LABEL, language_string(language_use = language_use, str = "OS_323")) #Weighted N
  }

  # Confirm DESC_02_DENOMINATOR is valid and defined
  # If DESC_02_WEIGHTED is YES DESC_02_DENOMINATOR must be ALL
  if (str_to_upper(DESC_02_WEIGHTED) %in% "YES" & !str_to_upper(DESC_02_DENOMINATOR) %in% "ALL"){
    errormsgs <- c(errormsgs,paste0("DESC_02_DENOMINATOR must be ALL if DESC_02_WEIGHTED is YES.  The current value is ",
                                    DESC_02_DENOMINATOR))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("DESC_02_DENOMINATOR must be ALL if DESC_02_WEIGHTED is YES.  The current value is ",
                            DESC_02_DENOMINATOR))
    exitflag <- 1
  }
  # If DESC_02_WEIGHTED is NO DESC_02_DENOMINATOR can be ALL or RESPONDED
  if (!str_to_upper(DESC_02_DENOMINATOR) %in% c("ALL", "RESPONDED")){
    errormsgs <- c(errormsgs,paste0("DESC_02_DENOMINATOR must be ALL or RESPONDED.  The current value is ",
                                    DESC_02_DENOMINATOR))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("DESC_02_DENOMINATOR must be ALL or RESPONDED.  The current value is ",
                            DESC_02_DENOMINATOR))
    exitflag <- 1
  }
  # Gently remind the user that missing values are not tabulated when the denominator is RESPONDED
  if (str_to_upper(DESC_02_DENOMINATOR) %in% "RESPONDED"){
    vcqi_log_comment(VCP,3,"Comment",
                     "DESC_02 denominator is RESPONDED so missing values will not be tabulated.")
  }

  # If the user has used the outdated global macro nomenclature with the word MISSING in the macro names,
  # convert them to the new nomenclature with the word RELABEL

  if (vcqi_object_exists("DESC_02_N_MISSING_LEVELS")){
    assign("DESC_02_N_RELABEL_LEVELS",DESC_02_N_MISSING_LEVELS,envir = .GlobalEnv)
    for (i in seq_along(DESC_02_N_RELABEL_LEVELS)){
      if (vcqi_object_exists(paste0("DESC_02_MISSING_LEVEL_",i))){
        miss_level <- get(paste0("DESC_02_MISSING_LEVEL_",i),envir = .GlobalEnv)
        assign(paste0("DESC_02_RELABEL_LEVEL_",i),miss_level,envir = .GlobalEnv)
      }
      if (vcqi_object_exists(paste0("DESC_02_MISSING_LEVEL_",i))){
        miss_label <- get(paste0("DESC_02_MISSING_LABEL_",i),envir = .GlobalEnv)
        assign(paste0("DESC_02_RELABEL_LABEL_",i),miss_label,envir = .GlobalEnv)
      }
    } #end of DESC_02_N_RELABEL_LEVELS i loop
  }

  # Confirm global values are either missing or a number for Globals DESC_02_N_RELABEL_LEVELS and DESC_02_N_SUBTOTALS
  globs <- c("DESC_02_N_RELABEL_LEVELS","DESC_02_N_SUBTOTALS")
  for (g in seq_along(globs)){
    if (!vcqi_object_exists(globs[g])){
      assign(globs[g], 0, envir = .GlobalEnv)
      glob <- 0
    } else {
      glob <- get(globs[g], envir = .GlobalEnv)
    }

    if(!is.numeric(glob)){
      errormsgs <- c(errormsgs, paste0("Global variable ",globs[g],
                                      " must be a numeric value. The current value is ",glob))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable ",globs[g],
                              " must be a numeric value. The current value is ",glob))
      exitflag <- 1
    } else if (glob < 0){
      errormsgs <- c(errormsgs, paste0("Global variable ",globs[g],
                                       " must be a number >= zero. The current value is ",glob))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("Global variable ",globs[g],
                              " must be a number >= zero. The current value is ",glob))
      exitflag <- 1
    }

  } #end of globs g loop

  # Exit out for errors once here since we will use DESC_02_N_RELABEL_LEVELS and DESC_02_N_SUBTOTALS later
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # If DESC_02_N_RELABEL_LEVELS was blank and changed to 0 all other missing level globals will be ignored
  if ((DESC_02_N_RELABEL_LEVELS == 0) %in% TRUE){
    if (vcqi_object_exists("DESC_02_RELABEL_LEVEL_1")){
      warning("Warning: Global macro DESC_02_RELABEL_LEVEL_1 is defined, but DESC_02_N_RELABEL_LEVELS is 0 or not defined, so no levels will be re-labeled")
      vcqi_log_comment(VCP,2,"Warning","Warning: Global macro DESC_02_RELABEL_LEVEL_1 is defined,",
      " but DESC_02_N_RELABEL_LEVELS is 0 or not defined, so no levels will be re-labeled")
    }
  }

  # Confirm each missing level and label is populated for each DESC_02_N_RELABEL_LEVELS LEVEL
  if ((DESC_02_N_RELABEL_LEVELS != 0) %in% TRUE){
    for (i in 1:DESC_02_N_RELABEL_LEVELS){
      #DESC_02_RELABEL_LEVEL_i
      if(exists(paste0("DESC_02_RELABEL_LEVEL_",i), envir = .GlobalEnv) %in% TRUE){
        relab_level <- get(paste0("DESC_02_RELABEL_LEVEL_",i), envir = .GlobalEnv)
      } else {
        relab_level <- "NO"
      }

      if((!exists(paste0("DESC_02_RELABEL_LEVEL_",i)) & !is.na(relab_level)) %in% TRUE){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_02_RELABEL_LEVEL_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_02_RELABEL_LEVEL_",i,
                                " must be defined."))
        exitflag <- 1
      }

      #DESC_02_RELABEL_LABEL_i
      if(!exists(paste0("DESC_02_RELABEL_LABEL_",i))){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_02_RELABEL_LABEL_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_02_RELABEL_LABEL_",i,
                                " must be defined."))
        exitflag <- 1
      }

    } #end of DESC_02_N_RELABEL_LEVELS i loop
  }

  # If DESC_02_N_SUBTOTALS was blank and changed to 0 all other subtotal level globals be ignored
  if ((DESC_02_N_SUBTOTALS == 0) %in% TRUE){
    if (vcqi_object_exists("DESC_02_SUBTOTAL_LEVELS_1")){
      warning("Warning: Global macro DESC_02_SUBTOTAL_LEVELS_1 is defined, but DESC_02_N_SUBTOTALS is 0 or not defined, so no subtotals will be calculated.")
      vcqi_log_comment(VCP,2,"Warning: Global macro DESC_02_SUBTOTAL_LEVELS_1 is defined, but DESC_02_N_SUBTOTALS is 0 or not defined, so no subtotals will be calculated.")
    }
  }

  # Confirm each subtotal level and label is populated for each DESC_02_N_SUBTOTALS LEVEL
  if ((DESC_02_N_SUBTOTALS != 0) %in% TRUE){
    for (i in 1:DESC_02_N_SUBTOTALS){
      #DESC_02_SUBTOTAL_LEVELS_i
      if(exists(paste0("DESC_02_SUBTOTAL_LEVELS_",i), envir = .GlobalEnv) %in% FALSE){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_02_SUBTOTAL_LEVELS_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_02_SUBTOTAL_LEVELS_",i,
                                " must be defined."))
        exitflag <- 1
      }

      #DESC_02_SUBTOTAL_LABEL_i
      if(!vcqi_object_exists(paste0("DESC_02_SUBTOTAL_LABEL_",i))){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_02_SUBTOTAL_LABEL_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_02_SUBTOTAL_LABEL_",i,
                                " must be defined."))
        exitflag <- 1
      }


      # If SUBTOTAL_LIST is defined, then confirm that
      # a) it holds only two words
      # b) the first word is either BEFORE or AFTER
      # c) the second word is a level from DESC_02_VARIABLES

      if (vcqi_object_exists(paste0("DESC_02_SUBTOTAL_LIST_",i))){
        sub_list <- get(paste0("DESC_02_SUBTOTAL_LIST_",i), envir = .GlobalEnv)
        if (sapply(strsplit(sub_list, " "), length) != 2){
          errormsgs <- c(errormsgs, paste0("Global variable DESC_02_SUBTOTAL_LIST_",i,
                                           ' is defined but does not have two words. (It is currently "',
                                           sub_list,
                                           '".) The first word should be BEFORE or AFTER.  The second word should be the name of a level of the DESC_02_VARIABLES.'))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Global variable DESC_02_SUBTOTAL_LIST_",i,
                                  ' is defined but does not have two words. (It is currently "',
                                  sub_list,
                                  '".) The first word should be BEFORE or AFTER.  The second word should be the name of a level of the DESC_02_VARIABLES.'))
          exitflag <- 1
        }

        if (!str_to_upper(word(sub_list,1)) %in% c("BEFORE","AFTER")){
          errormsgs <- c(errormsgs, paste0("The first word of global DESC_02_SUBTOTAL_LIST_",i,
                                           " should be BEFORE or AFTER.  It is currently ",
                                           word(sub_list,1)))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("The first word of global DESC_02_SUBTOTAL_LIST_",i,
                                  " should be BEFORE or AFTER.  It is currently ",
                                  word(sub_list,1)))
          exitflag <- 1
        }
      } #end of checking DESC_02_SUBTOTAL_LIST_i

      # Foreach value provided in SUBTOTAL_LIST confirm it is an appropriate value
      # for the variable provided
      for (v in seq_along(DESC_02_VARIABLES)){
        var <- get(DESC_02_VARIABLES[v],dat)
        var2 <- zap_labels(var)
        if (class(var2) == "character"){
          errormsgs <- c(errormsgs, paste0("DESC_02 can only compute subtotals for numeric variables; ",DESC_02_VARIABLES[v],
                                           " is a string variable."))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("DESC_02 can only compute subtotals for numeric variables; ",DESC_02_VARIABLES[v],
                                  " is a string variable."))
          exitflag <- 1
        } else {

          if (!is.null(attr(var,"labels"))){
            labels <- unique(stack(attr(var, 'labels')))
            lalist <- as.character(labels$ind)
            llist <- sort(labels$values)
            llist_str <- llist

            # Add the missing option if ALL was selected as denominator
            if (str_to_upper(DESC_02_DENOMINATOR) == "ALL"){
              llist_str <- c(llist, "NA")
              llist <- c(llist, NA)
            }

            if (exists(paste0("DESC_02_SUBTOTAL_LEVELS_",i), envir = .GlobalEnv) %in% TRUE){
              sub_level <- get(paste0("DESC_02_SUBTOTAL_LEVELS_",i), envir = .GlobalEnv)
              for (g in seq_along(sub_level)){

                listmatch <- 0

                if (!is.na(sub_level[g])){
                  if (!is.numeric(sub_level[g])){
                    errormsgs <- c(errormsgs, paste0("Global Variable DESC_02_SUBTOTAL_LEVELS_",i,
                                                     " must be all numeric values. It currently contains the value of ",
                                                     sub_level[g]))
                    vcqi_log_comment(VCP,1,"Error",
                                     paste0("Global Variable DESC_02_SUBTOTAL_LEVELS_",i,
                                            " must be all numeric values. It currently contains the value of ",
                                            sub_level[g]))
                    exitflag <- 1
                  } else {
                    for (p in seq_along(llist)){
                      if ((sub_level[g] == llist[p]) %in% TRUE){
                        listmatch <- 1
                      }
                    } #end of llist p loop
                  }
                } else {
                  for (p in seq_along(llist)){
                    if (is.na(llist[p]) & is.na(sub_level[g])){
                      listmatch <- 1
                    }
                  } #end of llist p loop
                }

                if (listmatch == 0){
                  errormsgs <- c(errormsgs, paste0("The values of global DESC_02_SUBTOTAL_LEVELS_",i,
                                                   " should be one of the values associated with variable ",
                                                   DESC_02_VARIABLES[v]," listed in DESC_02_VARIABLES(",
                                                   str_flatten_comma(llist_str),"). It currently contains the value of ",sub_level[g]))
                  vcqi_log_comment(VCP,1,"Error",
                                   paste0("The values of global DESC_02_SUBTOTAL_LEVELS_",i,
                                          " should be one of the values associated with variable ",
                                          DESC_02_VARIABLES[v]," listed in DESC_02_VARIABLES(",
                                          str_flatten_comma(llist_str),"). It currently contains the value of ",sub_level[g]))
                  exitflag <- 1
                }

              }#end of sub_level g loop
            }

            # Complete the same check for values in DESC_02_SUBTOTAL_LIST if specified
            # Create a local to find the number to be used as before or after
            if (vcqi_object_exists(paste0("DESC_02_SUBTOTAL_LIST_",i))){
              sub_list <- get(paste0("DESC_02_SUBTOTAL_LIST_",i), envir = .GlobalEnv)
              sub_list <- str_to_upper(sub_list)
              if (grepl("AFTER",sub_list,fixed = TRUE)){
                l <- str_to_sentence(gsub("AFTER ","",sub_list, fixed = TRUE))
              }
              if (grepl("BEFORE",str_to_upper(sub_list),fixed = TRUE)){
                l <- str_to_sentence(gsub("BEFORE ","",sub_list, fixed = TRUE))
              }

              listmatch <- 0

              for (p in seq_along(llist)){
                if ((l == as.character(llist[p])) %in% TRUE | (is.na(llist[p]) & l == "Na")){
                  listmatch <- 1
                }
              } #end of llist p loop

              if (listmatch == 0){
                errormsgs <- c(errormsgs, paste0("Global variable DESC_02_SUBTOTAL_LIST_",i,
                                                 " must contain a valid variable value number from variable ",
                                                 DESC_02_VARIABLES[v],": ",str_flatten_comma(llist_str),". The current value is: ",l))
                vcqi_log_comment(VCP,1,"Error",
                                 paste0("Global variable DESC_02_SUBTOTAL_LIST_`",i,
                                        " must contain a valid variable value number from variable ",
                                        DESC_02_VARIABLES[v],": ",str_flatten_comma(llist_str),". The current value is: ",l))
                exitflag <- 1
              }

            }

          }
        }
      } #end of DESC_02_VARIABLES v loop

    } #end of DESC_02_N_SUBTOTALS i loop

    # Confirm that if "${DESC_02_SHOW_SUBTOTALS_ONLY"}" then the user has not also specified
		# any SUBTOTAL_LEVEL globals

    if (vcqi_object_exists("DESC_02_SHOW_SUBTOTALS_ONLY")){
      for (i in 1: DESC_02_N_SUBTOTALS){
        if (vcqi_object_exists(paste0("DESC_02_SUBTOTAL_LIST_",i))){
          warning(paste0("If you specify to DESC_02_SHOW_SUBTOTALS_ONLY then you cannot also specify SUBTOTAL_LIST; DESC_02_SUBTOTAL_LIST_",
                         i," will be ignored."))
          vcqi_log_comment(VCP,2,"Warning",paste0("If you specify to DESC_02_SHOW_SUBTOTALS_ONLY then you cannot also specify SUBTOTAL_LIST; DESC_02_SUBTOTAL_LIST_",
                                                  i," will be ignored."))
          # Clear out SUBTOTAL_LIST global
          rm(list = c(paste0(paste0("DESC_02_SUBTOTAL_LIST_",i))))
          vcqi_log_comment(VCP, 3, "Global", paste0("Global value DESC_02_SUBTOTAL_LIST_", i, " has been removed."))
        }
      } #end of DESC_02_N_SUBTOTALS i loop
    }
  }


  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
    halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
