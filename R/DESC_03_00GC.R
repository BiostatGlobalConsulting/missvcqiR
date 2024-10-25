#' Check global macros for DESC_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr
#' @rawNamespace import(rlang, except = c(local_options,with_options))


# DESC_03_00GC R version 1.02 - Biostat Global Consulting - 2024-10-22
# ###############################################################################
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-06-06  1.00      Mia Yu          Original R package version
# 2023-08-21  1.01      Mia Yu          Add the part that check if all variables
#                                       in DESC_03_VARIABLES have the same # of
#                                       people who answered the question
# 2024-10-22  1.02      Caitlin Clary   Revert check requiring all vars in
#                                       DESC_03_VARIABLES to be defined for the
#                                       same observations
# ###############################################################################

DESC_03_00GC <- function(VCP = "DESC_03_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  if (!vcqi_object_exists("DESC_03_DATASET")){
    errormsgs <- c(errormsgs, "You must specify DESC_03_DATASET")
    vcqi_log_comment(VCP, 1, "Error", "You must specify DESC_03_DATASET")
    exitflag <- 1
  }

  # Exit out for errors once here since we will use DESC_03_DATASET later
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
  datname <- gsub(".rds", "", DESC_03_DATASET)

  if (str_to_upper(datname) %in% c("RI","SIA", "TT")){
    filename <- paste0(VCQI_OUTPUT_FOLDER, "/", datname, "_with_ids.rds")
    if (file.exists(filename)){
      vcqi_global(DESC_03_DATASET, paste0(datname, "_with_ids.rds"))
    }

    if (!file.exists(filename)){
      if (!file.exists(paste0(VCQI_OUTPUT_FOLDER, "/", datname,".rds"))){
        errormsgs <- c(errormsgs,
                       paste0("DESC_03_DATASET is ", datname, " but there is no dataset named ",
                              datname, " or named ", datname, "_with_ids in the VCQI output folder."))
        vcqi_log_comment(VCP, 1, "Error",
                         paste0("DESC_03_DATASET is ", datname, " but there is no dataset named ",
                                datname, " or named ", datname, "_with_ids in the VCQI output folder."))
        exitflag <- 1
      } else {
        vcqi_global(DESC_03_DATASET, paste0(datname, ".rds"))
      }
    }
  }

  # Confirm dataset exists
  if (!file.exists(paste0(VCQI_OUTPUT_FOLDER, "/", DESC_03_DATASET))){
    errormsgs <- c(errormsgs,
                   paste0(VCQI_OUTPUT_FOLDER, "/", DESC_03_DATASET," does not exist."))
    vcqi_log_comment(VCP, 1, "Error",
                     paste0(VCQI_OUTPUT_FOLDER, "/", DESC_03_DATASET," does not exist."))
    exitflag <- 1
  } else {
    dat <- vcqi_read(file = paste0(VCQI_OUTPUT_FOLDER, "/", DESC_03_DATASET))
  }

  # Confirm variables have been provided
  if (!vcqi_object_exists("DESC_03_VARIABLES")){
    errormsgs <- c(errormsgs, "You must define global variable DESC_03_VARIABLES")
    vcqi_log_comment(VCP, 1, "Error", "You must define global variable DESC_03_VARIABLES")
    exitflag <- 1
  }

  # Exit out for errors once here since we will use dat in the following code
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # pcount <- 1
  for (g in seq_along(DESC_03_VARIABLES)){

    if (!DESC_03_VARIABLES[g] %in% names(dat)){
      errormsgs <- c(
        errormsgs,
        paste0("The variable ", DESC_03_VARIABLES[g],
               " provided in global macro DESC_03_VARIABLES does not exist in dataset"))

      vcqi_log_comment(
        VCP, 1, "Error",
        paste0(
          "The variable ", DESC_03_VARIABLES[g],
          " provided in global macro DESC_03_VARIABLES does not exist in dataset"))

      exitflag <- 1
    }
    # else {
    #   qvar <- rlang::sym(DESC_03_VARIABLES[g])
    #   dat <- dat %>% mutate(tempvar1 = ifelse(!is.na(!!qvar),1,0))
    #   if (pcount == 1){
    #     g1 <- DESC_03_VARIABLES[g]
    #   }
    #   if (pcount > 1){
    #     var1 <- get("tempvar1",dat)
    #     var2 <- get("populated_1",dat)
    #     if (any(!(var1 == var2) %in% TRUE)){
    #       errormsgs <- c(errormsgs,paste0("The variable ",DESC_03_VARIABLES[g],
    #                                       " provided in global macro DESC_03_VARIABLES is not defined for precisely the same observations as ",
    #                                       g1, ". Each variable in global macro DESC_03_VARIABLES needs to be defined for the same observations."))
    #       vcqi_log_comment(VCP,1,"Error",
    #                        paste0("The variable ",DESC_03_VARIABLES[g],
    #                               " provided in global macro DESC_03_VARIABLES is not defined for precisely the same observations as ",
    #                               g1, ". Each variable in global macro DESC_03_VARIABLES needs to be defined for the same observations."))
    #       exitflag <- 1
    #     }
    #   }
    #   names(dat)[which(names(dat) == "tempvar1")] <- paste0("populated_",pcount)
    #   pcount = pcount + 1
    # }
  } #end of DESC_03_VARIABLES g loop

  # Confirm global variables DESC_03_SHORT_TITLE and DESC_03_SELECTED_VALUE are defined
  if (!vcqi_object_exists("DESC_03_SHORT_TITLE")){
    errormsgs <- c(errormsgs, "Global variable DESC_03_SHORT_TITLE must be defined.")
    vcqi_log_comment(VCP, 1, "Error", "Global variable DESC_03_SHORT_TITLE must be defined.")
    exitflag <- 1
  }

  if (!vcqi_object_exists("DESC_03_SELECTED_VALUE")){
    errormsgs <- c(errormsgs, "Global variable DESC_03_SELECTED_VALUE must be defined.")
    vcqi_log_comment(VCP, 1, "Error", "Global variable DESC_03_SELECTED_VALUE must be defined.")
    exitflag <- 1
  }

  # Confirm that all variables within DESC_03_VARIABLES are the same var type
  classes <- NULL
  for (g in seq_along(DESC_03_VARIABLES)){
    if (DESC_03_VARIABLES[g] %in% names(dat)){
      var <- get(DESC_03_VARIABLES[g],dat)
      var <- haven::zap_label(var)
      var <- haven::zap_labels(var)
      classes <- c(classes,class(var))
    }
  } #end of DESC_03_VARIABLES g loop

  if (length(unique(classes)) != 1){
    errormsgs <- c(errormsgs, "All variables specified in DESC_03_VARIABLES must have the same value type to be included in this measurement.")
    vcqi_log_comment(VCP, 1, "Error", "All variables specified in DESC_03_VARIABLES must have the same value type to be included in this measurement.")
    exitflag <- 1
  }

  # R only:
  if (vcqi_object_exists("DESC_03_SHOW_SUBTOTALS_ONLY")){
    if (!str_to_upper(DESC_03_SHOW_SUBTOTALS_ONLY) %in% "YES"){
      errormsgs <- c(errormsgs,paste0("DESC_03_SHOW_SUBTOTALS_ONLY must be YES or NA. The current value is ",
                                      DESC_03_SHOW_SUBTOTALS_ONLY))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("DESC_03_SHOW_SUBTOTALS_ONLY must be YES or NA. The current value is ",
                              DESC_03_SHOW_SUBTOTALS_ONLY))
      exitflag <- 1
    }
  }
  if (vcqi_object_exists("DESC_03_LIST_N_BEFORE_PCT")){
    if (!str_to_upper(DESC_03_LIST_N_BEFORE_PCT) %in% "YES"){
      errormsgs <- c(errormsgs,paste0("DESC_03_LIST_N_BEFORE_PCT must be YES or NA. The current value is ",
                                      DESC_03_LIST_N_BEFORE_PCT))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("DESC_03_LIST_N_BEFORE_PCT must be YES or NA. The current value is ",
                              DESC_03_LIST_N_BEFORE_PCT))
      exitflag <- 1
    }
  }
  if (vcqi_object_exists("DESC_03_LIST_NWTD_BEFORE_PCT")){
    if (!str_to_upper(DESC_03_LIST_NWTD_BEFORE_PCT) %in% "YES"){
      errormsgs <- c(errormsgs,paste0("DESC_03_LIST_NWTD_BEFORE_PCT must be YES or NA. The current value is ",
                                      DESC_03_LIST_NWTD_BEFORE_PCT))
      vcqi_log_comment(VCP,1,"Error",
                       paste0("DESC_03_LIST_NWTD_BEFORE_PCT must be YES or NA. The current value is ",
                              DESC_03_LIST_NWTD_BEFORE_PCT))
      exitflag <- 1
    }
  }


  # Set default N and NWTD labels if not specified
  if (!vcqi_object_exists("DESC_03_N_LABEL")){
    vcqi_global(DESC_03_N_LABEL, language_string(language_use = language_use, str = "OS_48")) #N
  }
  if (!vcqi_object_exists("DESC_03_NWTD_LABEL")){
    vcqi_global(DESC_03_NWTD_LABEL, language_string(language_use = language_use, str = "OS_323")) #Weighted N
  }

  # Confirm DESC_03_WEIGHTED is valid & defined
  if (!str_to_upper(DESC_03_WEIGHTED) %in% c("YES", "NO")){
    errormsgs <- c(errormsgs,paste0("DESC_03_WEIGHTED must be YES or NO.  The current value is ",
                                    DESC_03_WEIGHTED))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("DESC_03_WEIGHTED must be YES or NO.  The current value is ",
                            DESC_03_WEIGHTED))
    exitflag <- 1
  }

  # Confirm DESC_03_DENOMINATOR is valid and defined
  # If DESC_03_WEIGHTED is YES DESC_03_DENOMINATOR must be ALL
  if (str_to_upper(DESC_03_WEIGHTED) %in% "YES" & !str_to_upper(DESC_03_DENOMINATOR) %in% "ALL"){
    errormsgs <- c(errormsgs,paste0("DESC_03_DENOMINATOR must be ALL if DESC_03_WEIGHTED is YES.  The current value is ",
                                    DESC_03_DENOMINATOR))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("DESC_03_DENOMINATOR must be ALL if DESC_03_WEIGHTED is YES.  The current value is ",
                            DESC_03_DENOMINATOR))
    exitflag <- 1
  }
  # If DESC_03_WEIGHTED is NO DESC_03_DENOMINATOR can be ALL or RESPONDED
  if (!str_to_upper(DESC_03_DENOMINATOR) %in% c("ALL", "RESPONDED")){
    errormsgs <- c(errormsgs,paste0("DESC_03_DENOMINATOR must be ALL or RESPONDED.  The current value is ",
                                    DESC_03_DENOMINATOR))
    vcqi_log_comment(VCP,1,"Error",
                     paste0("DESC_03_DENOMINATOR must be ALL or RESPONDED.  The current value is ",
                            DESC_03_DENOMINATOR))
    exitflag <- 1
  }

  # If the user has used the outdated global macro nomenclature with the word MISSING in the macro names,
  # convert them to the new nomenclature with the word RELABEL

  if (vcqi_object_exists("DESC_03_N_MISSING_LEVELS")){
    assign("DESC_03_N_RELABEL_LEVELS",DESC_03_N_MISSING_LEVELS,envir = .GlobalEnv)
    for (i in seq_along(DESC_03_N_RELABEL_LEVELS)){
      if (vcqi_object_exists(paste0("DESC_03_MISSING_LEVEL_",i))){
        miss_level <- get(paste0("DESC_03_MISSING_LEVEL_",i),envir = .GlobalEnv)
        assign(paste0("DESC_03_RELABEL_LEVEL_",i),miss_level,envir = .GlobalEnv)
      }
      if (vcqi_object_exists(paste0("DESC_03_MISSING_LEVEL_",i))){
        miss_label <- get(paste0("DESC_03_MISSING_LABEL_",i),envir = .GlobalEnv)
        assign(paste0("DESC_03_RELABEL_LABEL_",i),miss_label,envir = .GlobalEnv)
      }
    } #end of DESC_03_N_RELABEL_LEVELS i loop
  }

  # Confirm global values are either missing or a number for Globals DESC_03_N_RELABEL_LEVELS and DESC_03_N_SUBTOTALS
  globs <- c("DESC_03_N_RELABEL_LEVELS", "DESC_03_N_SUBTOTALS")
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

  # Exit out for errors once here since we will use DESC_03_N_RELABEL_LEVELS and DESC_03_N_SUBTOTALS later
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # If DESC_03_N_RELABEL_LEVELS was blank and changed to 0 all other missing level globals will be ignored
  if ((DESC_03_N_RELABEL_LEVELS == 0) %in% TRUE){
    if (vcqi_object_exists("DESC_03_RELABEL_LEVEL_1")){
      warning("Warning: Global macro DESC_03_RELABEL_LEVEL_1 is defined, but DESC_03_N_RELABEL_LEVELS is 0 or not defined, so no subtotals will be calculated.")
      vcqi_log_comment(VCP,2,"Warning: Global macro DESC_03_RELABEL_LEVEL_1 is defined, but DESC_03_N_RELABEL_LEVELS is 0 or not defined, so no subtotals will be calculated.")
    }
  }

  # Confirm each missing level and label is populated for each DESC_03_N_RELABEL_LEVELS LEVEL
  if ((DESC_03_N_RELABEL_LEVELS != 0) %in% TRUE){
    for (i in 1:DESC_03_N_RELABEL_LEVELS){
      #DESC_03_RELABEL_LEVEL_i
      if(exists(paste0("DESC_03_RELABEL_LEVEL_",i), envir = .GlobalEnv) %in% TRUE){
        relab_level <- get(paste0("DESC_03_RELABEL_LEVEL_",i), envir = .GlobalEnv)
      } else {
        relab_level <- "NO"
      }

      if((!vcqi_object_exists(paste0("DESC_03_RELABEL_LEVEL_",i)) & !is.na(relab_level)) %in% TRUE){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_03_RELABEL_LEVEL_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_03_RELABEL_LEVEL_",i,
                                " must be defined."))
        exitflag <- 1
      }

      #DESC_03_RELABEL_LABEL_i
      if(!vcqi_object_exists(paste0("DESC_03_RELABEL_LABEL_",i))){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_03_RELABEL_LABEL_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_03_RELABEL_LABEL_",i,
                                " must be defined."))
        exitflag <- 1
      }

    } #end of DESC_03_N_RELABEL_LEVELS i loop
  }

  # If DESC_03_N_SUBTOTALS was blank and changed to 0 all other subtotal level globals be ignored
  if ((DESC_03_N_SUBTOTALS == 0) %in% TRUE){
    if (vcqi_object_exists("DESC_03_SUBTOTAL_LEVELS_1")){
      warning("Warning: Global macro DESC_03_SUBTOTAL_LEVELS_1 is defined, but DESC_03_N_SUBTOTALS is 0 or not defined, so no subtotals will be calculated.")
      vcqi_log_comment(VCP,2,"Warning: Global macro DESC_03_SUBTOTAL_LEVELS_1 is defined, but DESC_03_N_SUBTOTALS is 0 or not defined, so no subtotals will be calculated.")
    }
  }

  # Confirm each missing level and label is populated for each N_MISSING LEVEL
  if ((DESC_03_N_SUBTOTALS != 0) %in% TRUE){
    for (i in 1:DESC_03_N_SUBTOTALS){
      #DESC_03_SUBTOTAL_LEVELS_i
      if(exists(paste0("DESC_03_SUBTOTAL_LEVELS_",i), envir = .GlobalEnv) %in% FALSE){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_03_SUBTOTAL_LEVELS_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_03_SUBTOTAL_LEVELS_",i,
                                " must be defined."))
        exitflag <- 1
      }

      #DESC_03_SUBTOTAL_LABEL_i
      if(!vcqi_object_exists(paste0("DESC_03_SUBTOTAL_LABEL_",i))){
        errormsgs <- c(errormsgs, paste0("Global variable DESC_03_SUBTOTAL_LABEL_",i,
                                         " must be defined."))
        vcqi_log_comment(VCP,1,"Error",
                         paste0("Global variable DESC_03_SUBTOTAL_LABEL_",i,
                                " must be defined."))
        exitflag <- 1
      }


      # If SUBTOTAL_LIST is defined, then confirm that
      # a) it holds only two words
      # b) the first word is either BEFORE or AFTER
      # c) the second word is a variable from DESC_03_VARIABLES

      if (vcqi_object_exists(paste0("DESC_03_SUBTOTAL_LIST_",i))){
        sub_list <- get(paste0("DESC_03_SUBTOTAL_LIST_",i), envir = .GlobalEnv)
        if (sapply(strsplit(sub_list, " "), length) != 2){
          errormsgs <- c(errormsgs, paste0("Global variable DESC_03_SUBTOTAL_LIST_",i,
                                           ' is defined but does not have two words. (It is currently "',
                                           sub_list,
                                           '".) The first word should be BEFORE or AFTER.  The second word should be the name of a level of the DESC_03_VARIABLES'))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Global variable DESC_03_SUBTOTAL_LIST_",i,
                                  ' is defined but does not have two words. (It is currently "',
                                  sub_list,
                                  '".) The first word should be BEFORE or AFTER.  The second word should be the name of a level of the DESC_03_VARIABLES'))
          exitflag <- 1
        }

        if (!str_to_upper(word(sub_list,1)) %in% c("BEFORE","AFTER")){
          errormsgs <- c(errormsgs, paste0("The first word of global DESC_03_SUBTOTAL_LIST_",i,
                                           " should be BEFORE or AFTER.  It is currently ",
                                           word(sub_list,1)))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("The first word of global DESC_03_SUBTOTAL_LIST_",i,
                                  " should be BEFORE or AFTER.  It is currently ",
                                  word(sub_list,1)))
          exitflag <- 1
        }

        listmatch <- 0

        for (g in seq_along(DESC_03_VARIABLES)){
          if ((DESC_03_VARIABLES[g] == word(sub_list,2))%in% TRUE){
            listmatch <- 1
          }
        } #end of DESC_03_VARIABLES g loop

        if (listmatch == 0){
          errormsgs <- c(errormsgs, paste0("The second word of global DESC_03_SUBTOTAL_LIST_",i,
                                           " should be one of the variables listed in DESC_03_VARIABLES. It is currently ",
                                           word(sub_list,2)))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("The second word of global DESC_03_SUBTOTAL_LIST_",i,
                                  " should be one of the variables listed in DESC_03_VARIABLES. It is currently ",
                                  word(sub_list,2)))
          exitflag <- 1
        }

      } #end of checking DESC_03_SUBTOTAL_LIST_i

    } #end of DESC_03_N_SUBTOTALS i loop

    # Confirm that if "${DESC_03_SHOW_SUBTOTALS_ONLY"}" then the user has not also specified
    # any SUBTOTAL_LEVEL globals

    if (vcqi_object_exists("DESC_03_SHOW_SUBTOTALS_ONLY")){
      for (i in 1: DESC_03_N_SUBTOTALS){
        if (vcqi_object_exists(paste0("DESC_03_SUBTOTAL_LIST_",i))){
          warning(paste0("If you specify to DESC_03_SHOW_SUBTOTALS_ONLY then you cannot also specify SUBTOTAL_LIST; DESC_03_SUBTOTAL_LIST_",
                         i," will be ignored."))
          vcqi_log_comment(VCP,2,"Warning",paste0("If you specify to DESC_03_SHOW_SUBTOTALS_ONLY then you cannot also specify SUBTOTAL_LIST; DESC_03_SUBTOTAL_LIST_",
                                                  i," will be ignored."))
          # Clear out SUBTOTAL_LIST global
          rm(list = c(paste0(paste0("DESC_03_SUBTOTAL_LIST_",i))))
          vcqi_log_comment(VCP, 3, "Global", paste0("Global value DESC_03_SUBTOTAL_LIST_", i, " has been removed."))
        }
      } #end of DESC_03_N_SUBTOTALS i loop
    }

  }

  # exit out here first since we we use DESC_03_SUBTOTAL_LEVELS_i
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Confirm all variables listed in the DESC_03_RELABEL_LEVELS and DESC_03_SUBTOTAL_LEVELS
  # Are included in the DESC_03_VARIABLES global variable

  if (DESC_03_N_SUBTOTALS != 0){
    for (i in 1:DESC_03_N_SUBTOTALS){
      sub_level <- get(paste0("DESC_03_SUBTOTAL_LEVELS_",i),envir = .GlobalEnv)
      for (g in seq_along(sub_level)){
        match <- 0
        for (d in seq_along(DESC_03_VARIABLES)){
          if((sub_level[g] == DESC_03_VARIABLES[d]) %in% TRUE){
            match <- 1
          }
        } #end of DESC_03_VARIABLES d loop

        if (match == 0){
          errormsgs <- c(errormsgs, paste0("Variable",sub_level[g], " in DESC_03_SUBTOTAL_LEVELS_",i,
                                           " is not inlcuded in DESC_03_VARIABLES"))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Variable",sub_level[g], " in DESC_03_SUBTOTAL_LEVELS_",i,
                                  " is not inlcuded in DESC_03_VARIABLES"))
          exitflag <- 1
        }

      } #end of sub_level g loop
    } #end of DESC_03_N_SUBTOTALS i loop
  }

  if (DESC_03_N_RELABEL_LEVELS != 0){
    for (i in 1:DESC_03_N_RELABEL_LEVELS){
      relabel <- get(paste0("DESC_03_RELABEL_LEVEL_",i),envir = .GlobalEnv)
      for (g in seq_along(relabel)){
        match <- 0
        for (d in seq_along(DESC_03_VARIABLES)){
          if((relabel[g] == DESC_03_VARIABLES[d]) %in% TRUE){
            match <- 1
          }
        } #end of DESC_03_VARIABLES d loop

        if (match == 0){
          errormsgs <- c(errormsgs, paste0("Variable",relabel[g], " in DESC_03_RELABEL_LEVEL_",i,
                                           " is not inlcuded in DESC_03_VARIABLES"))
          vcqi_log_comment(VCP,1,"Error",
                           paste0("Variable",relabel[g], " in DESC_03_RELABEL_LEVEL_",i,
                                  " is not inlcuded in DESC_03_VARIABLES"))
          exitflag <- 1
        }

      } #end of sub_level g loop
    } #end of DESC_03_N_SUBTOTALS i loop
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
