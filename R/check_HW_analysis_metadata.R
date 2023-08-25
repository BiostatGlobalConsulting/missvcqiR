#' Check HW analysis-related globals, datasets and variables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#'
#' @return Log entries; errors if conditions not met
#'
#' @export
#'
#' @examples
#' check_HW_analysis_metadata()

# check_HW_analysis_metadata R version 1.00 - Biostat Global Consulting - 2023-08-10
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-10  1.00      Mia Yu          Original R package version
# *******************************************************************************


check_HW_analysis_metadata <- function(VCP = "check_HW_analysis_metadata"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  # Check that HW dataset was specified
  if (!vcqi_object_exists("HW_SURVEY_DATASET")){
    errormsgs <- c(errormsgs,
                   "Please set HW_SURVEY_DATASET.")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error",
                     "Please set HW_SURVEY_DATASET.")
  } else {
    # Check that HW dataset exists
    vcqi_log_global(VCQI_DATA_FOLDER)
    vcqi_log_global(HW_SURVEY_DATASET)

    if (!file.exists(paste0(VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET))){
      errormsgs <- c(errormsgs,paste0("The file defined by global macros VCQI_DATA_FOLDER/HW_SURVEY_DATASET (",
                                      VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET, ") does not exist"))
      vcqi_log_comment(VCP,1,"Error", paste0("RI dataset: ", VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET, " does not exist"))
      exitflag <- 1
    } else {
      # Check that HW variables used across all DESC indicators are present
      # and have the correct variable type
       dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET))

       if(is.data.frame(dat) == FALSE){
         errormsgs <- c(errormsgs,paste0("The file defined by global macros", VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET," is not in valid format"))
         exitflag <- 1
         vcqi_log_comment(VCP, 1, "Error",
                          paste0("The file defined by global macros", VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET," is not in valid format"))
       } else {

         dlist <- NULL

         var1 <- c("04AF", "04AG", "04AH", "05AA", "05AB", "05AC", "05AD", "05AH","05AI",
                   "06AA", "06AB", "06AC", "06AE", "06AF", "06AG", "06AH", "06AI", "06AJ")
         for (v in seq_along(var1)){
           dlist <- c(dlist,paste0("HW",var1[v]))
         } #end of var1 v loop

         var2 <- c("A", "B", "C", "D", "E", "F", "G", "H")
         for(i in seq_along(var2)){
           dlist <- c(dlist,paste0("ID02A",var2[i]))
         } #end of var2 i loop

         var3 <- c("HW01AA", "HW04AA_1", "HW04AA_2", "HW04AA_3", "HW04AA_4", "HW04AA_5",
                   "HW04AB_1", "HW04AB_2", "HW04AB_3", "HW04AB_4", "HW04AB_5", "HW04AC_1",
                   "HW04AC_2", "HW04AC_3", "HW04AC_4", "HW04AC_5", "HW04AD_1", "HW04AD_2",
                   "HW04AD_3", "HW04AD_4", "HW04AD_5", "HW04AE_1", "HW04AE_2", "HW04AE_3",
                   "HW04AE_4", "HW04AE_5", "HW04AI_1", "HW04AI_2", dlist, "HW04AJ_1",
                   "HW04AJ_2", "HW04AJ_3", "HW04AJ_4", "HW04AJ_5", "HW04AJ_6", "HW04AJ_7",
                   "HW06AD_1", "HW06AD_2", "HW06AD_3", "HW06AD_4", "HW06AD_5", "HW06AD_6",
                   "HW06AK_1", "HW06AK_2", "HW06AK_3", "HW06AK_4",	"ID02AIid", "ID02AIname",
                   "ID02AJm", "ID02AJd", "ID02AJy")

         for (v in seq_along(var3)){
           if (var3[v] %in% names(dat)){

             if (var3[v] %in% c("ID02AB", "ID02AD", "ID02AF", "ID02AH", "ID02AIid")){
               var <- get(var3[v],dat)
               var <- haven::zap_label(var)
               var <- haven::zap_labels(var)
               if ("character" %in% class(var)){
                 if (any(is.na(var)| var == "")){
                   errormsgs <- c(errormsgs,paste0(var3[v], "cannot have a missing value in the HW dataset."))
                   vcqi_log_comment(VCP,1,"Error", paste0(var3[v], "cannot have a missing value in the HW dataset."))
                   exitflag <- 1
                 }
               } else if (any(is.na(var))){
                 errormsgs <- c(errormsgs,paste0(var3[v], "cannot have a missing value in the HW dataset."))
                 vcqi_log_comment(VCP,1,"Error", paste0(var3[v], "cannot have a missing value in the HW dataset."))
                 exitflag <- 1
               }
             }

           } else {
             errormsgs <- c(errormsgs,paste0("Variable ", var3[v],
                                             " does not exist in HW dataset and is required to run MISS-VCQI."))
             vcqi_log_comment(VCP,1,"Error", paste0("Variable ", var3[v],
                                                    " does not exist in HW dataset and is required to run MISS-VCQI."))
             exitflag <- 1
           }
         } #end of var3 v loop
       }
    }
  }

  # Check globals that are populated

  globs <- c("VCQI_DATA_FOLDER", "VCQI_OUTPUT_FOLDER", "VCQI_ANALYSIS_NAME", "VCQI_CM_DATASET")
  for (g in seq_along(globs)){
    if (!vcqi_object_exists(globs[g])){
      errormsgs <- c(errormsgs,paste0("Please set ",globs[g]))
      vcqi_log_comment(VCP,1,"Error", paste0("Please set ",globs[g]))
      exitflag <- 1
    } else {
      # If the globals are populated, other fields can be checked

      if (globs[g] == "VCQI_CM_DATASET" & vcqi_object_exists("VCQI_DATA_FOLDER")){
        if (!file.exists(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))){
          errormsgs <- c(errormsgs,paste0("The file defined by global macros ",
                                          VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET, " does not exist"))
          vcqi_log_comment(VCP,1,"Error",paste0("The file defined by global macros ",
                                                VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET, " does not exist"))
          exitflag <- 1
        } else {
          dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET))

          if(is.data.frame(dat) == FALSE){
            errormsgs <- c(errormsgs,paste0("The file defined by global macros ",VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET," is not in valid format"))
            exitflag <- 1
            vcqi_log_comment(VCP, 1, "Error",
                             paste0("The file defined by global macros ",VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET," is not in valid format"))
          } else{
            # Begin check
            var4 <- c("ID02AIid", "ID02AIname", "province_id", "urban_cluster")
            for (v in seq_along(var4)){
              if (var4[v] %in% names(dat)){
                i <- "numeric"

                if (var4[v] %in% "ID02AIname"){
                  i <- "character"
                }

                var <- get(var4[v],dat)
                var <- haven::zap_label(var)
                var <- haven::zap_labels(var)
                if (!(class(var) == i) %in% TRUE){
                  errormsgs <- c(errormsgs,paste0(var4[v]," needs to be a ",i," variable in CM dataset."))
                  vcqi_log_comment(VCP,1,"Error", paste0(var4[v]," needs to be a ", i," variable in CM dataset."))
                  exitflag <- 1
                } else {

                  if (!var4[v] %in% c("province_id", "urban_cluster")){
                    if (any(is.na(var)| var == "")){
                      errormsgs <- c(errormsgs,paste0(var4[v], "cannot have a missing value in the CM dataset."))
                      vcqi_log_comment(VCP,1,"Error", paste0(var4[v], "cannot have a missing value in the CM dataset."))
                      exitflag <- 1
                    }
                  }

                } #check the class of the variable

              } else {
                errormsgs <- c(errormsgs,paste0("Variable ", var4[v],
                                                " does not exist in CM dataset and is required to run VCQI."))
                vcqi_log_comment(VCP,1,"Error", paste0("Variable ", var4[v],
                                                       " does not exist in CM dataset and is required to run VCQI."))
                exitflag <- 1
              }
            } #end of var4 v loop
          } #check file in right format
        } #check file exists
      }
    }
  } #end of globs g loop

  # Check for existence of datasets listing strata names and orders

  # Confirm contents of levelof datasets to ensure dataset exists
  # Confirm the level*id, level*name and level*order variables exist
  # Confirm variables are not missing and have the correct variable type.

  for(i in c(1,3)){

    # Check that LEVEL_n_NAME_DATASET value exists
    if(vcqi_object_exists(paste0("LEVEL", i, "_NAME_DATASET")) == FALSE){
      errormsgs <- c(errormsgs,
                     paste0("Please set LEVEL", i, "_NAME_DATASET"))
      vcqi_log_comment(VCP, 1, "Error",
                       paste0("Please set LEVEL", i, "_NAME_DATASET"))
      exitflag <- 1
    } else {
      # Log global
      vcqi_log_global(paste0("LEVEL", i, "_NAME_DATASET"))

      # Check that LEVEL_n_NAME_DATASET file exists
      if(file.exists(get(paste0("LEVEL", i, "_NAME_DATASET"))) == FALSE){
        errormsgs <- c(errormsgs,
                       paste0("LEVEL", i, "_NAME_DATASET does not exist"))
        vcqi_log_comment(VCP, 1, "Error",
                         paste0("LEVEL", i, "_NAME_DATASET does not exist"))
        exitflag <- 1
      } else {

        # Read LEVEL_n_NAME_DATASET file
        levelfile <- vcqi_read(get(paste0("LEVEL", i, "_NAME_DATASET")))

        if(is.data.frame(levelfile) == FALSE){
          errormsgs <- c(errormsgs,paste0("The file defined by global macros VCQI_DATA_FOLDER/VCQI_RI_DATASET ",get(paste0("LEVEL", i, "_NAME_DATASET"))," is not in valid format"))
          exitflag <- 1
          vcqi_log_comment(VCP, 1, "Error",
                           paste0("Dataset: ", get(paste0("LEVEL", i, "_NAME_DATASET"))," is not in valid format"))
        } else{

          # Check for required variables
          if(!paste0("level", i, "id") %in% names(levelfile) |
             !paste0("level", i, "name") %in% names(levelfile)){
            errormsgs <- c(errormsgs,
                           paste0("LEVEL", i, "_NAME_DATASET should contain variables level", i, "id and level", i, "name."))
            vcqi_log_comment(VCP, 1, "Error",
                             paste0("LEVEL", i, "_NAME_DATASET should contain variables level", i, "id and level", i, "name."))
            exitflag <- 1
          } else {

            varlist <- c(paste0("level", i, "id"), paste0("level", i, "name"))

            for(v in seq_along(varlist)){

              if(varlist[v] == paste0("level", i, "id")){
                vartype <- "numeric"
                if(is.numeric(get(varlist[v], levelfile)) == FALSE){
                  vartype_error <- TRUE
                } else {vartype_error <- FALSE}
              } else {
                vartype <- "character"
                if(is.character(get(varlist[v], levelfile)) == FALSE){
                  vartype_error <- TRUE
                } else {vartype_error <- FALSE}}

              if(vartype_error == TRUE){
                errormsgs <- c(errormsgs,
                               paste0(varlist[v], " needs to be a ", vartype, " variable in ",
                                      "LEVEL", i, "_NAME_DATASET"))
                vcqi_log_comment(VCP, 1, "Error",
                                 paste0(varlist[v], " needs to be a ", vartype, " variable in ",
                                        "LEVEL", i, "_NAME_DATASET"))
                exitflag <- 1
              }

              # Check for missing values
              if(any(is.na(get(varlist[v], levelfile))) == TRUE){
                errormsgs <- c(errormsgs,
                               paste0(varlist[v], " cannot have a missing value in the LEVEL",
                                      i, "_NAME_DATASET"))
                vcqi_log_comment(VCP, 1, "Error",
                                 paste0(varlist[v], " cannot have a missing value in the LEVEL",
                                        i, "_NAME_DATASET"))
                exitflag <- 1
              }

              if(i == 1){
                if(nrow(levelfile) != 1){
                  errormsgs <- c(errormsgs,
                                 paste0("LEVEL1_NAME_DATASET can only have 1 observation."))
                  vcqi_log_comment(VCP, 1, "Error",
                                   paste0("LEVEL1_NAME_DATASET can only have 1 observation."))
                  exitflag <- 1
                }
              }

            } # end v loop

          } # end else (variables exist)

        } # end else (file not in right format)

      } # end else (file exists)

    } # end else (object exists)

    # Checks on LEVEL_n_ORDER_DATASETs
    if(i %in% c(2,3)){

      # Check that LEVEL_n_ORDER_DATASET value exists
      if(vcqi_object_exists(paste0("LEVEL", i, "_ORDER_DATASET")) == FALSE){
        errormsgs <- c(errormsgs,
                       paste0("Please set LEVEL", i, "_ORDER_DATASET"))
        vcqi_log_comment(VCP, 1, "Error",
                         paste0("Please set LEVEL", i, "_ORDER_DATASET"))
        exitflag <- 1
      } else {
        # Log global
        vcqi_log_global(paste0("LEVEL", i, "_ORDER_DATASET"))

        # Check that LEVEL_n_ORDER_DATASET file exists
        if(file.exists(get(paste0("LEVEL", i, "_ORDER_DATASET"))) == FALSE){
          errormsgs <- c(errormsgs,
                         paste0("LEVEL", i, "_ORDER_DATASET does not exist"))
          vcqi_log_comment(VCP, 1, "Error",
                           paste0("LEVEL", i, "_ORDER_DATASET does not exist"))
          exitflag <- 1
        } else {
          # Read LEVEL_n_ORDER_DATASET file
          levelfile <- vcqi_read(get(paste0("LEVEL", i, "_ORDER_DATASET")))
          if(is.data.frame(levelfile) == FALSE){
            errormsgs <- c(
              errormsgs,
              paste0("The file defined by global macros ", get(paste0("LEVEL", i, "_ORDER_DATASET")), " is not in valid format"))
            exitflag <- 1

            vcqi_log_comment(
              VCP, 1, "Error",
              paste0("Dataset: ",
                     get(paste0("LEVEL", i, "_ORDER_DATASET")),
                     " is not in valid format"))
          } else {
            # Check for required variables
            if(!paste0("level", i, "id") %in% names(levelfile) |
               !paste0("level", i, "order") %in% names(levelfile)){

              errormsgs <- c(
                errormsgs,
                paste0("LEVEL", i, "_ORDER_DATASET should contain variables level", i, "id and level", i, "order."))

              vcqi_log_comment(
                VCP, 1, "Error",
                paste0("LEVEL", i, "_ORDER_DATASET should contain variables level", i, "id and level", i, "order."))

              exitflag <- 1
            } else {

              varlist <- c(paste0("level", i, "id"), paste0("level", i, "order"))

              for(v in seq_along(varlist)){

                if(is.numeric(get(varlist[v], levelfile)) == FALSE){
                  errormsgs <- c(errormsgs,
                                 paste0(varlist[v], " needs to be a numeric variable in ",
                                        "LEVEL", i, "_ORDER_DATASET"))
                  vcqi_log_comment(VCP, 1, "Error",
                                   paste0(varlist[v], " needs to be a numeric variable in ",
                                          "LEVEL", i, "_ORDER_DATASET"))
                  exitflag <- 1
                }

                # Check for missing values
                if(any(is.na(get(varlist[v], levelfile))) == TRUE){
                  errormsgs <- c(errormsgs,
                                 paste0(varlist[v], " cannot have a missing value in the LEVEL",
                                        i, "_ORDER_DATASET"))
                  vcqi_log_comment(VCP, 1, "Error",
                                   paste0(varlist[v], " cannot have a missing value in the LEVEL",
                                          i, "_ORDER_DATASET"))
                  exitflag <- 1
                }

              } # end v loop
            } # end else (variables exist)
          } # end else (file not in right format)
        } # end else (file exists)
      } # end else (object exists)
    } # end if (i = 2 or 3)

  } # end i loop


  # *************************************************************************

  # Confirm contents of level 4 names and order datasets, if VCQI_LEVEL4_STRATIFIER is requested
  # Confirm the level4id level4name level4order are not missing and have the correct variable type.

  vcqi_log_global(VCQI_LEVEL4_SET_VARLIST)
  vcqi_log_global(VCQI_LEVEL4_SET_LAYOUT)

  # If the user provides a level4 set_layout they must supply the set_varlist
  if(vcqi_object_exists("VCQI_LEVEL4_SET_LAYOUT") & !vcqi_object_exists("VCQI_LEVEL4_SET_VARLIST")){
    exitflag <- 1
    errormsgs <- c(errormsgs, "If you specify VCQI_LEVEL4_SET_LAYOUT then you must also specify VCQI_LEVEL4_SET_VARLIST.")
    vcqi_log_comment(VCP, 1, "Error",  "If you specify VCQI_LEVEL4_SET_LAYOUT then you must also specify VCQI_LEVEL4_SET_VARLIST.")
  }

  # Check variables provided in the level 4 varlist
  if(vcqi_object_exists("VCQI_LEVEL4_SET_VARLIST")){

    # Placeholder for default layout dataset
    layout_temp <- NULL

    for(v in seq_along(VCQI_LEVEL4_SET_VARLIST)){

      print(paste0("Looking for ", VCQI_LEVEL4_SET_VARLIST[v]))
      found <- FALSE

      # We don't know which dataset holds each stratifier...most will be
      # in the RI/SIA/TT datasets but some might be in CM or another, so
      # loop over the datasets and keep going until we find it.

      # Note that it may be convenient sometimes to use a stratifier that
      # is constructed DURING the VCQI run and therefore does not exist
      # in the datasets in the VCQI_DATA_FOLDER. To accommodate this option,
      # we include an item in the search list named VCQI_TEMP_STRATHOLDER.
      # This global can point to a file in the VCQI_OUTPUT_FOLDER. Note,
      # for instance, how it is used in the indicator named SIA_COVG_04.

      # Define datasets to look for
      vcqi_datasets <- c("VCQI_RI_DATASET", "VCQI_SIA_DATASET",
                         "VCQI_TT_DATASET", "VCQI_CM_DATASET",
                         "VCQI_RIHC_DATASET", "VCQI_HM_DATASET",
                         "VCQI_HH_DATASET", "LEVEL1_NAME_DATASET",
                         "LEVEL2_NAME_DATASET", "LEVEL3_NAME_DATASET")

      # Restrict to datasets that are defined and exist in the data folder

      # For most datasets, look in VCQI_DATA_FOLDER
      vcqi_datasets1 <- lapply(seq_along(vcqi_datasets), function(x){
        if(vcqi_object_exists(vcqi_datasets[x])){
          if(file.exists(paste0(VCQI_DATA_FOLDER, "/", get(vcqi_datasets[x], envir = globalenv())))){
            vcqi_datasets[x]
          }
        }}) %>% do.call(c, .)

      # For other datasets, like LEVEL<X>_NAME_DATASET, a full path is defined
      vcqi_datasets2 <- lapply(seq_along(vcqi_datasets), function(x){
        if(vcqi_object_exists(vcqi_datasets[x])){
          if(file.exists(get(vcqi_datasets[x], envir = globalenv()))){
            vcqi_datasets[x]
          }
        }}) %>% do.call(c, .)

      vcqi_datasets2 <- vcqi_datasets2[!vcqi_datasets2 %in% vcqi_datasets1]

      vcqi_datasets <- c(vcqi_datasets1, vcqi_datasets2)

      # Add VCQI_TEMP_STRATHOLDER if appropriate
      if(vcqi_object_exists("VCQI_TEMP_STRATHOLDER")){
        if(file.exists(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_TEMP_STRATHOLDER))){
          vcqi_datasets <- c(vcqi_datasets, VCQI_TEMP_STRATHOLDER)
        }} else {VCQI_TEMP_STRATHOLDER <- ""}

      for(d in seq_along(vcqi_datasets)){

        # For datasets other than VCQI_TEMP_STRATHOLDER...
        if (vcqi_datasets[d] != VCQI_TEMP_STRATHOLDER){

          # Read from VCQI_DATA_FOLDER when appropriate
          if (vcqi_datasets[d] %in% vcqi_datasets1){

            temppath <- paste0(VCQI_DATA_FOLDER, "/",
                               get(vcqi_datasets[d], envir = global_env()))

            tempdat <- vcqi_read(temppath)

            # Otherwise read using the full path
          } else {
            temppath <- get(vcqi_datasets[d], envir = global_env())

            tempdat <- vcqi_read(temppath)
          }
          # Read VCQI_TEMP_STRATHOLDER from VCQI_OUTPUT_FOLDER
        } else {

          temppath <- paste0(VCQI_OUTPUT_FOLDER, "/",
                             get(vcqi_datasets[d], envir = global_env()))

          tempdat <- vcqi_read(temppath)
        }

        varhere <- any(names(tempdat) == VCQI_LEVEL4_SET_VARLIST[v])

        if(varhere == TRUE){
          print(paste0("Found ", VCQI_LEVEL4_SET_VARLIST[v], " in ",
                       vcqi_datasets[d], "..."))

          dat <- vcqi_read(temppath)

          dvar <- get(VCQI_LEVEL4_SET_VARLIST[v], dat)

          # If the variable has a label in the input dataset, use that as the
          # variable label, otherwise use the variable name
          variable_label <- ifelse(!is.null(attributes(dvar)$label),
                                   attributes(dvar)$label, VCQI_LEVEL4_SET_VARLIST[v])

          if(is.labelled(dvar)){
            valuesandlabels <- data.frame(
              val = attr(dvar, "labels"),
              lab = names(attr(dvar, "labels"))
            )
          } else {
            valuesandlabels <- data.frame(
              val = sort(unique(dvar)),
              lab = sort(unique(dvar))
            )
          }

          variable_values <- valuesandlabels$val
          variable_value_labels <- valuesandlabels$lab
          nrows <- length(variable_values) + 1

          layout_rows <- data.frame(
            order = rep(NA, nrows),
            label = c(variable_label, rep(NA, nrows-1)),
            condition = rep(NA, nrows),
            rowtype = c("LABEL_ONLY", rep("DATA_ROW", nrows-1))
          )

          if(is.labelled(dvar)){
            layout_rows$label[2:nrows] <- names(attr(dvar, "labels"))
            layout_rows$value_todrop <- c(NA, attr(dvar, "labels"))
          } else {
            layout_rows$label[2:nrows] <- sort(unique(dvar))
            layout_rows$value_todrop <- c(NA, sort(unique(dvar)))
          }

          if(is.numeric(dvar)){

            layout_rows <- layout_rows %>%
              mutate(
                condition = ifelse(
                  rowtype %in% "DATA_ROW",
                  paste0(VCQI_LEVEL4_SET_VARLIST[v], ' == ',  value_todrop), NA),
              ) %>% select(-value_todrop)

          } else {

            layout_rows <- layout_rows %>%
              mutate(
                condition = ifelse(
                  rowtype %in% "DATA_ROW",
                  paste0(VCQI_LEVEL4_SET_VARLIST[v], ' == ', '"', value_todrop, '"'), NA),
              ) %>% select(-value_todrop)
          }

          layout_temp <- bind_rows(layout_temp, layout_rows)

          # Stop loop here once the variable has been found
          found <- TRUE
          break
        } # end varhere = TRUE

      } # End d loop (searching through datasets)

      if(found == FALSE){
        exitflag <- 1

        errormsgs <- c(
          errormsgs,
          paste0(
            "User asked for variable ", VCQI_LEVEL4_SET_VARLIST[v],
            " in VCQI_LEVEL4_SET_VARLIST but that variable doesn't seem to appear in ",
            paste(vcqi_datasets, collapse = ", ")))

        vcqi_log_comment(VCP, 1, "Error", paste0(
          "User asked for variable ", VCQI_LEVEL4_SET_VARLIST[v],
          " in VCQI_LEVEL4_SET_VARLIST but that variable doesn't seem to appear in ", paste(vcqi_datasets, collapse = ", ")))
      }

    } # End v loop (looping through variables)

    # If VCQI_LEVEL4_SET_LAYOUT wasn't provided, save default layout here:
    if(exitflag != 1  & !vcqi_object_exists("VCQI_LEVEL4_SET_LAYOUT")){

      layout_temp <- layout_temp %>% mutate(order = 1:n())

      saveRDS(layout_temp, file = paste0(VCQI_OUTPUT_FOLDER, "/VCQI_LEVEL4_SET_LAYOUT_automatic.rds"))
      vcqi_global(VCQI_LEVEL4_SET_LAYOUT,
                  paste0(VCQI_OUTPUT_FOLDER, "/VCQI_LEVEL4_SET_LAYOUT_automatic.rds"))
    }

  } # end if varlist supplied but not level4 layout

  if(vcqi_object_exists("VCQI_LEVEL4_SET_LAYOUT")){
    if(file.exists(VCQI_LEVEL4_SET_LAYOUT)){

      level4_layout <- vcqi_read(VCQI_LEVEL4_SET_LAYOUT)

      varerror <- FALSE
      if(any(names(level4_layout) == "order") == FALSE){varerror <- TRUE} else if(
        is.numeric(level4_layout$order) == FALSE){varerror <- TRUE}
      if(sum(c("label", "condition", "rowtype") %in% names(level4_layout)) < 3){
        varerror <- TRUE
      } else if(
        is.character(level4_layout$label) == FALSE | is.character(level4_layout$condition) == FALSE | is.character(level4_layout$rowtype) == FALSE
      ){varerror <- TRUE}

      if(varerror == TRUE){
        exitflag <- 1
        errormsgs <- c(errormsgs, "VCQI_LEVEL4_SET_LAYOUT must contain a numeric variable named order and string variables named rowtype, label and condition")
        vcqi_log_comment(VCP, 1, "Error", "VCQI_LEVEL4_SET_LAYOUT must contain a numeric variable named order and string variables named rowtype, label and condition")
      }

      #2023/01/10: add new part to check level4 customized plot related settings

      #check if the all inputs are valid for outlinecolor1_r: valid R color or NA
      if ("outlinecolor1_r" %in% names(level4_layout)) {
        outlinecolor1_r <- get("outlinecolor1_r", level4_layout)
        if (any((!(outlinecolor1_r %in% colors())&!(nchar(outlinecolor1_r) == 7 & substr(outlinecolor1_r, 1, 1) == "#") & !is.na(outlinecolor1_r)& !outlinecolor1_r %in% c(""))) %in% TRUE) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-
            c(errormsgs,"The column outlinecolor1_r only accepts valid R color from colors() or valid hex code or NA")
          vcqi_log_comment(VCP,1,"Error","The column outlinecolor1_r only accepts valid R color from colors() or valid hex code or NA")
        }
      } #end of checking outlinecolor1_r

      #check if the all inputs are valid for outlinecolor2_r: valid R color or NA
      if ("outlinecolor2_r" %in% names(level4_layout)) {
        outlinecolor2_r <- get("outlinecolor2_r", level4_layout)
        if (any((!(outlinecolor2_r %in% colors())&!(nchar(outlinecolor2_r) == 7 & substr(outlinecolor2_r, 1, 1) == "#") & !is.na(outlinecolor2_r)& !outlinecolor2_r %in% c(""))) %in% TRUE) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-
            c(errormsgs,"The column outlinecolor2_r only accepts valid R color from colors() or valid hex code or NA")
          vcqi_log_comment(VCP,1,"Error","The column outlinecolor2_r only accepts valid R color from colors() or valid hex code or NA")
        }
      } #end of checking outlinecolor2_r

      #check if the all inputs are valid for bar_fillcolor1_r: valid R color or NA
      if ("bar_fillcolor1_r" %in% names(level4_layout)) {
        bar_fillcolor1_r <- get("bar_fillcolor1_r", level4_layout)
        if (any((!(bar_fillcolor1_r %in% colors())&!(nchar(bar_fillcolor1_r) == 7 & substr(bar_fillcolor1_r, 1, 1) == "#")& !is.na(bar_fillcolor1_r)& !bar_fillcolor1_r %in% c(""))) %in% TRUE) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-c(errormsgs,"The column bar_fillcolor1_r only accepts valid R color from colors() or valid hex code or NA")
          vcqi_log_comment(VCP,1,"Error","The column bar_fillcolor1_r only accepts valid R color from colors() or valid hex code or NA")
        }
      } #end of checking bar_fillcolor1_r

      #check if the all inputs are valid for bar_fillcolor2_r: valid R color or NA
      if ("bar_fillcolor2_r" %in% names(level4_layout)) {
        bar_fillcolor2_r <- get("bar_fillcolor2_r", level4_layout)
        if (any((!(bar_fillcolor2_r %in% colors())&!(nchar(bar_fillcolor2_r) == 7 & substr(bar_fillcolor2_r, 1, 1) == "#")& !is.na(bar_fillcolor2_r)& !bar_fillcolor2_r %in% c(""))) %in% TRUE) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-c(errormsgs,"The column bar_fillcolor2_r only accepts valid R color from colors() or valid hex code or NA")
          vcqi_log_comment(VCP,1,"Error","The column bar_fillcolor2_r only accepts valid R color from colors() or valid hex code or NA")
        }
      } #end of checking bar_fillcolor2_r

      #check if the all inputs are valid for shadecolor1_r: valid R color or NA
      if ("shadecolor1_r" %in% names(level4_layout)) {
        shadecolor1_r <- get("shadecolor1_r", level4_layout)
        if (any((!(shadecolor1_r %in% colors())&!(nchar(shadecolor1_r) == 7 & substr(shadecolor1_r, 1, 1) == "#")& !is.na(shadecolor1_r)& !shadecolor1_r %in% c(""))) %in% TRUE) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-c( errormsgs,"The column shadecolor1_r only accepts valid R color from colors() or valid hex code or NA")
          vcqi_log_comment(VCP,1,"Error","The column shadecolor1_r only accepts valid R color from colors() or valid hex code or NA")
        }
      } #end of checking shadecolor1_r

      #check if the all inputs are valid for shadecolor2_r: valid R color or NA
      if ("shadecolor2_r" %in% names(level4_layout)) {
        shadecolor2_r <- get("shadecolor2_r", level4_layout)
        if (any((!(shadecolor2_r %in% colors())&!(nchar(shadecolor2_r) == 7 & substr(shadecolor2_r, 1, 1) == "#")& !is.na(shadecolor2_r)& !shadecolor2_r %in% c(""))) %in% TRUE) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-c( errormsgs,"The column shadecolor2_r only accepts valid R color from colors() or valid hex code or NA")
          vcqi_log_comment(VCP,1,"Error","The column shadecolor2_r only accepts valid R color from colors() or valid hex code or NA")
        }
      } #end of checking shadecolor2_r

      #chekc if all inputs are valid for addline
      if ("addline" %in% names(level4_layout)) {
        addline <- get("addline", level4_layout)
        if (any(!addline %in% c("below", "above", "both", "", NA))) {
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-c(errormsgs,"The column addline only accepts four possible values: below, above, both or NA")
          vcqi_log_comment(VCP,1,"Error","The column addline only accepts four possible values: below, above, both or NA")
        }
      }#end of checking addline

      #2023-0207 add: check cell format
      assign("use_basic_fmtids",1, envir = .GlobalEnv)
      # Import the deafult cell styles
      vcqi_basic_fmtids()

      if (vcqi_object_exists("FMTIDS")){
        # First check if the file exists
        if(file.exists(FMTIDS)){
          # Second check if all the styles created are R acceptable
          # Try to source the user-provided file
          sourcetest <- try(source(FMTIDS),silent = TRUE)
          # Check if there were any errors triggered by the user-provided file
          # If there were errors, add a warning message to the VCQI log
          # If no errors, source the user-provided file
          if (inherits(sourcetest, "try-error")){
            exitflag <- 1
            #TO DO: update error message and comment
            errormsgs <-c(errormsgs,paste0("FMTIDS file", FMTIDS, " contains invalid cell style. Please check the file"))
            vcqi_log_comment(VCP,1,"Error",paste0("FMTIDS file", FMTIDS, " contains invalid cell style. Please check the file"))

          } else {
            # Import the customized formats
            assign("use_basic_fmtids",0, envir = .GlobalEnv)
            source(file = FMTIDS)

            # Check if there is repeated id names if columns are defined.
            defaultid <- c("bold_left", "bold_right", "regular_left", "regular_right", "shaded_left", "shaded_right", "italic_left", "italic_right")

            firstin <- 0
            otherin <- 0
            repeat_ids <- NULL
            if ("fmtid_for_first_column_r" %in% names(level4_layout)){
              firstin <- 1
              for (l in 1:nrow(level4_layout)){
                firstid <- level4_layout$fmtid_for_first_column_r[l]
                # Check if there is repeated id
                if (firstid %in% defaultid){
                  repeat_ids <- c(repeat_ids," ", firstid)
                }

                # Check if there is not defined id
                if (firstid != "" & !is.na(firstid) & !is.null(firstid)){
                  if (!exists(firstid, envir = .GlobalEnv)){
                    level4_layout$fmtid_for_first_column_r[l] <- "regular_left"
                    warningmsgs <-c(warningmsgs,paste0("VCQI_LEVEL4_SET_LAYOUT contains the fmtid ", firstid,
                                                       " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_left"))
                    vcqi_log_comment(VCP,2,"Warning",paste0("VCQI_LEVEL4_SET_LAYOUT contains the fmtid ", firstid,
                                                            " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_left"))
                  }
                }
              } #end of l loop
            } #end of if column defined

            if ("fmtid_for_other_columns_r" %in% names(level4_layout)){
              otherin <- 1
              for (l in 1:nrow(level4_layout)){
                otherid <- level4_layout$fmtid_for_other_columns_r[l]
                # Check if there is repeated id
                if (otherid %in% defaultid){
                  repeat_ids <- c(repeat_ids," ", otherid)
                }
                # Check if there is not defined id
                if (otherid != "" & !is.na(otherid) & !is.null(otherid)){
                  if (!exists(otherid, envir = .GlobalEnv)){
                    level4_layout$fmtid_for_other_columns_r[l] <- "regular_right"
                    warningmsgs <-c(warningmsgs,paste0("VCQI_LEVEL4_SET_LAYOUT contains the fmtid ", otherid,
                                                       " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_right"))
                    vcqi_log_comment(VCP,2,"Warning",paste0("VCQI_LEVEL4_SET_LAYOUT contains the fmtid ", otherid,
                                                            " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_right"))
                  }
                }
              } #end of l loop
            }#end of if column defined

            # If there's repeated ids, send a message
            if (!is.null(repeat_ids)){
              repeat_ids <- str_flatten(repeat_ids)
              warningmsgs <-c(warningmsgs,paste0("The .R file provided in global FMTIDS contains the default fmtid name(s):", repeat_ids,
                                                 ". The customized fmtid settings will be used over the default values in vcqi_basic_fmtids function",
                                                 " which means that the foramt(s) might be applied somewehre else too."))
              vcqi_log_comment(VCP,2,"Warning",paste0("The .R file provided in global FMTIDS contains the default fmtid name(s):", repeat_ids,
                                                      ". The customized fmtid settings will be used over the default values in vcqi_basic_fmtids function",
                                                      " which means that the foramt(s) might be applied somewehre else too."))

            }

            # If columns not defined, send a message
            if (firstin == 0 & otherin == 0){
              assign("use_basic_fmtids",1, envir = .GlobalEnv)
              warningmsgs <- c(warningmsgs,
                               "Columns fmtid_for_first_column_r or fmtid_for_other_columns_r was not provided in VCQI_LEVEL4_SET_LAYOUT; VCQI will have access to the default formats from function vcqi_basic_fmtids")
              vcqi_log_comment(VCP,2,"Warning",
                               "Columns fmtid_for_first_column_r or fmtid_for_other_columns_r was not provided in VCQI_LEVEL4_SET_LAYOUT; VCQI will have access to the default formats from function vcqi_basic_fmtids")
            }

          } #end successfully imported customized formats.

        } else {
          #File path is not valid
          warningmsgs <-c(warningmsgs,paste0("File containing the format ids for VCQI_LEVEL4_SET_LAYOUT provided in global FMTIDS does not exist: ", FMTIDS))
          vcqi_log_comment(VCP,2,"Warning",paste0("File containing the format ids for VCQI_LEVEL4_SET_LAYOUT provided in global FMTIDS does not exist: ", FMTIDS))

          #change fmtid_for_first_column_r abd fmtid_for_other_columns_r to empty if exist
          if ("fmtid_for_first_column_r" %in% names(level4_layout)){
            level4_layout <- level4_layout %>% mutate(fmtid_for_first_column_r = NA)
          }

          if ("fmtid_for_other_columns_r" %in% names(level4_layout)){
            level4_layout <- level4_layout %>% mutate(fmtid_for_other_columns_r = NA)
          }

        }
      } else {

        notempty <- 0
        if ("fmtid_for_first_column_r" %in% names(level4_layout)){
          if (!(all(level4_layout$fmtid_for_first_column_r == "" | is.na(level4_layout$fmtid_for_first_column_r) | is.null(level4_layout$fmtid_for_first_column_r)))){
            notempty <- 1
          }
        }

        if ("fmtid_for_other_columns_r" %in% names(level4_layout)){
          if (!(all(level4_layout$fmtid_for_other_columns_r == "" | is.na(level4_layout$fmtid_for_other_columns_r) | is.null(level4_layout$fmtid_for_other_columns_r)))){
            notempty <- 1
          }
        }

        # If the user does not define the file but fmtid_for_first_column_r or fmtid_for_other_columns_r is not empty
        # error out
        if (notempty == 1){
          exitflag <- 1
          #TO DO: update error message and comment
          errormsgs <-c(errormsgs,"FMTIDS file is not defined but customized format is defined for at least one row.")
          vcqi_log_comment(VCP,1,"Error","FMTIDS file is not defined but customized format is defined for at least one row.")
        }

        # If the user does not define the file but fmtid_for_first_column_r or fmtid_for_other_columns_r is not defined or emtpy
        # Give a message.
        if (notempty == 0){
          warningmsgs <- c(warningmsgs,
                           "A separate user-defined file with format ids for VCQI_LEVEL4_SET_LAYOUT was not provided; VCQI will have access to the default formats from function vcqi_basic_fmtids")
          vcqi_log_comment(VCP,2,"Warning",
                           "A separate user-defined file with format ids for VCQI_LEVEL4_SET_LAYOUT was not provided; VCQI will have access to the default formats from function vcqi_basic_fmtids")
        }
      } #end of if FMTIDS not defined


    } else {
      exitflag <- 1
      errormsgs <- c(errormsgs, "VCQI_LEVEL4_SET_LAYOUT dataset does not exist")
      vcqi_global(VCQI_ERROR,1)
      vcqi_log_comment(VCP, 1, "Error", "VCQI_LEVEL4_SET_LAYOUT dataset does not exist")
    }
  }

  # Check settings related to making tables and plots

  if (vcqi_object_exists("EXPORT_TO_EXCEL")){
    if ((EXPORT_TO_EXCEL != 1 & EXPORT_TO_EXCEL != 0) %in% TRUE){
      exitflag <- 1
      errormsgs <- c(errormsgs, "Please set EXPORT_TO_EXCEL to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set EXPORT_TO_EXCEL to 0 or 1.")
    }
  } else {
    exitflag <- 1
    errormsgs <- c(errormsgs, "Please set EXPORT_TO_EXCEL to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set EXPORT_TO_EXCEL to 0 or 1.")
  }

  if (vcqi_object_exists("MAKE_PLOTS")){
    if ((MAKE_PLOTS != 1 & MAKE_PLOTS != 0) %in% TRUE){
      exitflag <- 1
      errormsgs <- c(errormsgs, "Please set MAKE_PLOTS to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set MAKE_PLOTS to 0 or 1.")
    }
  } else {
    exitflag <- 1
    errormsgs <- c(errormsgs, "Please set MAKE_PLOTS to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set MAKE_PLOTS to 0 or 1.")
  }

  if (vcqi_object_exists("DELETE_VCQI_DATABASES_AT_END")){
    if ((DELETE_VCQI_DATABASES_AT_END != 1 & DELETE_VCQI_DATABASES_AT_END != 0) %in% TRUE){
      exitflag <- 1
      errormsgs <- c(errormsgs, "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
    }
  } else {
    exitflag <- 1
    errormsgs <- c(errormsgs, "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
  }

  if (vcqi_object_exists("DELETE_TEMP_VCQI_DATASETS")){
    if ((DELETE_TEMP_VCQI_DATASETS != 1 & DELETE_TEMP_VCQI_DATASETS != 0) %in% TRUE){
      exitflag <- 1
      errormsgs <- c(errormsgs, "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
    }
  } else {
    exitflag <- 1
    errormsgs <- c(errormsgs, "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
  }

  if (!vcqi_object_exists("VCQI_PREPROCESS_DATA")){
    vcqi_global(VCQI_PREPROCESS_DATA,1)
  }
  if (!vcqi_object_exists("VCQI_GENERATE_DVS")){
    vcqi_global(VCQI_GENERATE_DVS,1)
  }
  if (!vcqi_object_exists("VCQI_GENERATE_DATABASES")){
    vcqi_global(VCQI_GENERATE_DATABASES,1)
  }

  # Default is to make all types of plots; user may override
  if (vcqi_object_exists("MAKE_PLOTS")){
    if (MAKE_PLOTS == 1 & !vcqi_object_exists("VCQI_MAKE_UW_PLOTS")){
      vcqi_global(VCQI_MAKE_UW_PLOTS,1)
    }
  }


  if (length(VCQI_LEVEL4_SET_VARLIST) > 1 ){
    if (vcqi_object_exists("VCQI_MAKE_UW_PLOTS")){
      if (VCQI_MAKE_UW_PLOTS == 1){
        vcqi_log_comment(VCP, 2, "Warning",
                         "VCQI does not make unweighted proportion plots when the user asks for 2+ LEVEL4 stratifiers via the LEVEL4_SET syntax.")
        vcqi_global(VCQI_MAKE_UW_PLOTS,0)
      }
    }
  }

  if (exitflag == 1) {
    vcqi_global(VCQI_ERROR, 1)
    miss_vcqi_halt_immediately(halt_message = errormsgs)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
  assign("level4_layout", level4_layout, envir = global_env())
}
