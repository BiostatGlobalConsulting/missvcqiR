
#' Check VCQI analysis metadata for missing information and format problems
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#'
#' @return Default run settings, default level4 layout file if needed, errors and/or warnings if conditions not met
#'
#' @export
#'
#' @examples
#' check_analysis_metadata_MV()
#'
# check_analysis_metadata_MV R version 1.17 - Biostat Global Consulting - 2024-05-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-06-28  1.00      Caitlin Clary   Original R version
# 2022-07-08  1.01      Caitlin Clary   Finished except for sections on LEVEL4 and
#                                       saving GPH files
# 2022-08-02  1.02      Mia Yu          Changed DELETE_TEMP_VCQI_DATABASES to correct name
#                                       Bugs fixed
# 2022-08-05  1.03      Mia Yu          Added the part that gives out error if the level
#                                       dataset is not in the right format
# 2022-08-08  1.04      Mia Yu          Fixed the parts that checks variables in HH and HM
#                                       Added the line to print warningmsgs
#                                       Added the line that check if HH/MM in right format
# 2022-08-11  1.05      Caitlin Clary   Add Level4 checks
# 2022-09-20  1.06      Caitlin Clary   Add default value for VCQI_SAVE_IW_PLOT_DATA
# 2022-10-07  1.07      Caitlin Clary   Add level1/2/3 names to level 4 variable
#                                       search list, fix bug with numeric vars
#                                       when making auto level 4 layout
# 2022-10-10  1.08      Caitlin Clary   Package version
# 2022-10-18  1.09      Caitlin Clary   Added vcqi_halt_immediately call
# 2022-10-24  1.10      Mia Yu          Added vcqi_halt_immediately after checking programlist
# 2023-01-10  1.11      Mia Yu          Added part for customized level4 plot settings
# 2023-02-07  1.12      Mia Yu          Added part for customized level4 Excel cell formats
# 2023-07-29  1.13      Mia Yu          Remove HH14 from missing value check
# 2023-08-28  1.14      Mia Yu          Copied and revised from check_analysis_metadata
# 2023-10-03  1.15      Mia Yu          Set default value of LEVEL2_ID
# 2023-11-15  1.16      Mia Yu          Add format columns with default settings for level4_layout
# 2024-05-20	1.17     	Mia Yu      		Add check for FOOTNOTE_CUTOFF and TITLE_CUTOFF
#										                    Set to default if not set for QUAL_08 and QUAL_09
# *******************************************************************************

# Note: sections of this program that check FMTID are not implemented (2022-10-07)
# Note: see formatID placeholder comment (2022-10-27)

check_analysis_metadata_MV <- function(VCP = "check_analysis_metadata_MV"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  programlist <- c("VCQI_DATA_FOLDER", "VCQI_OUTPUT_FOLDER", "VCQI_ANALYSIS_NAME")

  for(g in seq_along(programlist)){
    if(vcqi_object_exists(programlist[g]) == FALSE){
      errormsgs <- c(errormsgs, paste0("Please set ", programlist[g]))
      vcqi_log_comment(VCP, 1, "Error", paste0("Please set ", programlist[g]))
      exitflag <- 1
    }
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Set VCQI_PASS_THRU_VARLIST to be NULL to run DESC_02 and DESC_03 smoothly
  vcqi_global(VCQI_PASS_THRU_VARLIST, NULL)

  # Provide the variable name that uniquely identifies each facility
  vcqi_global(LEVEL_3_ID, "ID02AIid")

  # At this time, this global is required. Set its default value here.
  if (!vcqi_object_exists("LEVEL_2_ID")){
    vcqi_global(LEVEL_2_ID,"ID02AD")
  }

  # check to see if the TITLE and FOOTNOTE CUTOFF globals are set
  if(!vcqi_object_exists("TITLE_CUTOFF")){
    vcqi_global(TITLE_CUTOFF,40)
  }
  if(!vcqi_object_exists("FOOTNOTE_CUTOFF")){
    vcqi_global(FOOTNOTE_CUTOFF,100)
  }


  # This syntax should be set in Block E, set as default if user failed to specify
  if(vcqi_object_exists("VCQI_SVYDESIGN_SYNTAX") == FALSE){
    vcqi_global(VCQI_SVYDESIGN_SYNTAX, list(ids = ~clusterid, weights = ~psweight, strata = ~stratumid))
  }

  # This will set globals for each output in Miss-VCQI based on the value provided
  # in global OUTPUT_LANGUAGE
  miss_vcqi_multi_lingual_strings()

  # Check for variables if HH and HM datasets are provided

  HHlist <- c("HH01", "HH03")
  HMlist <- c("HM01", "HM03", "HM09", "HM22", "HM29")
  hhhm <- c("HH", "HM")

  if(vcqi_object_exists("VCQI_DATA_FOLDER")){
    for(i in 1:length(hhhm)){
      if(vcqi_object_exists(paste0("VCQI_", hhhm[i], "_DATASET"))){
        filetoget <- paste0(VCQI_DATA_FOLDER, "/", get(paste0("VCQI_", hhhm[i], "_DATASET")))
        if(file.exists(filetoget)){
          temp <- vcqi_read(filetoget)
          if(is.data.frame(temp)){
            vlist <- get(paste0(hhhm[i], "list"))

            for(v in 1:length(vlist)){

              if(vlist[v] %in% names(temp)){

                # If the variable exists, confirm the variable is not missing and has the correct variable type
                if(!vlist[v] %in% c("HH14", "HM09", "HM29")){
                  if(is.numeric(get(vlist[v], temp)) == FALSE){

                    errormsgs <- c(errormsgs, paste0(
                      vlist[v], " needs to be a numeric variable in the ",
                      hhhm[i], " dataset."
                    ))

                    vcqi_log_comment(VCP, 1, "Error",
                                     paste0(
                                       vlist[v],
                                       " needs to be a numeric variable in the ",
                                       hhhm[i], " dataset."
                                     ))

                    exitflag <- 1
                  }
                } # end numeric check

                if(any(is.na(get(vlist[v], temp)))){
                  warningmsgs <- c(warningmsgs,
                                   paste0(vlist[v],
                                          " should not have a missing value in the ",
                                          hhhm[i], " dataset."))

                  vcqi_log_comment(VCP, 2, "Warning",
                                   paste0(vlist[v],
                                          " should not have a missing value in the ",
                                          hhhm[i], " dataset."))

                } # end missing values check

              } else {
                errormsgs <- c(errormsgs, paste0(
                  "Variable ", vlist[v], " does not exist in the ", hhhm[i],
                  " dataset and is required to run VCQI"
                ))

                vcqi_log_comment(VCP, 1, "Error",
                                 paste0(
                                   "Variable ", vlist[v],
                                   " does not exist in the ", hhhm[i],
                                   " dataset and is required to run VCQI."))

                exitflag <- 1
              }

            } # end: v loop
          }# end: dataset file in right format
        } # end: dataset file exists
      } # end: dataset global exists
    } # end: i loop
  } # end: folder exists

  # Check for existence of datasets listing strata names and orders
  #
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
              paste0("The file defined by global macros VCQI_DATA_FOLDER/VCQI_RI_DATASET ", get(paste0("LEVEL", i, "_ORDER_DATASET")), " is not in valid format"))
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

  ## Level 4 checks ----

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

      if (vcqi_object_exists("LEVEL2_NAME_DATASET")){
        # Define datasets to look for
        vcqi_datasets <- c("HW_SURVEY_DATASET", "ES_SURVEY_DATASET",
                           "VCQI_CM_DATASET","LEVEL1_NAME_DATASET",
                           "LEVEL2_NAME_DATASET","LEVEL3_NAME_DATASET")
      } else {
        # Define datasets to look for
        vcqi_datasets <- c("HW_SURVEY_DATASET", "ES_SURVEY_DATASET",
                           "VCQI_CM_DATASET","LEVEL1_NAME_DATASET",
                           "LEVEL3_NAME_DATASET")
      }

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

      #2023-11-16 update: add format columns with default settings
      layout_temp <- layout_temp %>% mutate(order = 1:n(),
                                            fmtid_for_first_column_r = "regular_left",
                                            fmtid_for_other_columns_r = "regular_right",
                                            outlinecolor1_r = "#0000ff",
                                            outlinecolor2_r = "lightgrey",
                                            bar_fillcolor1_r = "#2b92be",
                                            bar_fillcolor2_r = "lightgrey",
                                            shadecolor1_r = NA,
                                            shadecolor2_r = NA,
                                            addline = NA) %>%
        mutate(fmtid_for_other_columns_r = ifelse(!rowtype %in% "DATA_ROW", NA, fmtid_for_other_columns_r),
               outlinecolor1_r = ifelse(!rowtype %in% "DATA_ROW", NA, outlinecolor1_r),
               bar_fillcolor2_r = ifelse(!rowtype %in% "DATA_ROW", NA, bar_fillcolor2_r),
               bar_fillcolor1_r = ifelse(!rowtype %in% "DATA_ROW", NA, bar_fillcolor1_r),
               bar_fillcolor2_r = ifelse(!rowtype %in% "DATA_ROW", NA, bar_fillcolor2_r))

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
          if (!(all(level4_layout$fmtid_for_first_column_r == "" |
                    is.na(level4_layout$fmtid_for_first_column_r) |
                    is.null(level4_layout$fmtid_for_first_column_r) |
                    level4_layout$fmtid_for_first_column_r %in%
                    c("regular_left","regular_right","shaded_left","shaded_right",
                      "italic_left","italic_right","bold_left","bold_right","italic_left_indented3")))){
            notempty <- 1
          }
        }

        if ("fmtid_for_other_columns_r" %in% names(level4_layout)){
          if (!(all(level4_layout$fmtid_for_other_columns_r == "" |
                    is.na(level4_layout$fmtid_for_other_columns_r) |
                    is.null(level4_layout$fmtid_for_other_columns_r) |
                    level4_layout$fmtid_for_other_columns_r %in%
                    c("regular_left","regular_right","shaded_left","shaded_right",
                      "italic_left","italic_right","bold_left","bold_right","italic_left_indented3")))){
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
        if (notempty == 0) {
          warningmsgs <- c(
            warningmsgs,
            "A separate user-defined file with format ids for VCQI_LEVEL4_SET_LAYOUT was not provided; VCQI will have access to the default formats from function vcqi_basic_fmtids"
          )
          vcqi_log_comment(
            VCP,
            2,
            "Warning",
            "A separate user-defined file with format ids for VCQI_LEVEL4_SET_LAYOUT was not provided; VCQI will have access to the default formats from function vcqi_basic_fmtids"
          )

          if ("fmtid_for_other_columns_r" %in% names(level4_layout)) {
            if (!(all(
                level4_layout$fmtid_for_other_columns_r == "" |
                is.na(level4_layout$fmtid_for_other_columns_r) |
                is.null(level4_layout$fmtid_for_other_columns_r) |
                level4_layout$fmtid_for_other_columns_r %in% "regular_left"))) {
              assign("use_basic_fmtids", 0, envir = .GlobalEnv)
            }
          }
          if ("fmtid_for_other_columns_r" %in% names(level4_layout)) {
            if (!(all(
                level4_layout$fmtid_for_other_columns_r == "" |
                is.na(level4_layout$fmtid_for_other_columns_r) |
                is.null(level4_layout$fmtid_for_other_columns_r) |
                level4_layout$fmtid_for_other_columns_r %in% "regular_right"))) {
              assign("use_basic_fmtids", 0, envir = .GlobalEnv)
            }
          }

        }
      } #end of if FMTIDS not defined


    } else {
      exitflag <- 1
      errormsgs <- c(errormsgs, "VCQI_LEVEL4_SET_LAYOUT dataset does not exist")
      vcqi_global(VCQI_ERROR,1)
      vcqi_log_comment(VCP, 1, "Error", "VCQI_LEVEL4_SET_LAYOUT dataset does not exist")
    }
  }

  # Format ID placeholder

  # Check CI method ----

  if(vcqi_object_exists("VCQI_CI_METHOD") == FALSE){
    vcqi_global(VCQI_CI_METHOD, "Wilson")
  } else {
    if(!(str_to_upper(VCQI_CI_METHOD) %in% c(
      "LOGIT", "WILSON",
      "CLOPPER", "CLOPPER-PEARSON",
      "JEFFREYS"
    ))){
      errormsgs <- c(errormsgs,
                     paste0("Please set VCQI_CI_METHOD to either LOGIT, WILSON, CLOPPER-PEARSON, or JEFFREYS. It is currently set to: ", VCQI_CI_METHOD, "."))
      vcqi_log_comment(VCP, 1, "Error",
                       paste0("Please set VCQI_CI_METHOD to either LOGIT, WILSON, CLOPPER-PEARSON, or JEFFREYS. It is currently set to: ", VCQI_CI_METHOD, "."))

      exitflag <- 1
    }
  }

  # Check settings related to making tables and plots

  # EXPORT_TO_EXCEL must exist and equal 0 or 1
  if(vcqi_object_exists("EXPORT_TO_EXCEL") == TRUE){
    if(!EXPORT_TO_EXCEL %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set EXPORT_TO_EXCEL to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set EXPORT_TO_EXCEL to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set EXPORT_TO_EXCEL to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set EXPORT_TO_EXCEL to 0 or 1.")
    exitflag <- 1
  }

  # FORMAT_EXCEL default = 1
  if(vcqi_object_exists("FORMAT_EXCEL") == TRUE){
    if(!FORMAT_EXCEL %in% c(0,1)){vcqi_global(FORMAT_EXCEL, 1)}
  } else {vcqi_global(FORMAT_EXCEL, 1)}

  # MAKE_PLOTS must exist and equal 0 or 1
  if(vcqi_object_exists("MAKE_PLOTS") == TRUE){
    if(!MAKE_PLOTS %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set MAKE_PLOTS to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set MAKE_PLOTS to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set MAKE_PLOTS to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set MAKE_PLOTS to 0 or 1.")
    exitflag <- 1
  }

  # DELETE_VCQI_DATABASES_AT_END must exist and equal 0 or 1
  if(vcqi_object_exists("DELETE_VCQI_DATABASES_AT_END") == TRUE){
    if(!DELETE_VCQI_DATABASES_AT_END %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_VCQI_DATABASES_AT_END to 0 or 1.")
    exitflag <- 1
  }

  # DELETE_TEMP_VCQI_DATASETS must exist and equal 0 or 1
  if(vcqi_object_exists("DELETE_TEMP_VCQI_DATASETS") == TRUE){
    if(!DELETE_TEMP_VCQI_DATASETS %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
      vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
    vcqi_log_comment(VCP, 1, "Error", "Please set DELETE_TEMP_VCQI_DATASETS to 0 or 1.")
    exitflag <- 1
  }

  # Default is to generate databases, although user can turn this off if they already exist
  if(vcqi_object_exists("VCQI_PREPROCESS_DATA") == FALSE){vcqi_global(VCQI_PREPROCESS_DATA, 1)}
  if(vcqi_object_exists("VCQI_GENERATE_DVS") == FALSE){vcqi_global(VCQI_GENERATE_DVS, 1)}
  if(vcqi_object_exists("VCQI_GENERATE_DATABASES") == FALSE){vcqi_global(VCQI_GENERATE_DATABASES, 1)}

  # Default is to make all types of plots; user may override
  if(vcqi_object_exists("MAKE_PLOTS") == TRUE){
    if(MAKE_PLOTS == 1){
      if(vcqi_object_exists("VCQI_MAKE_OP_PLOTS") == FALSE){vcqi_global(VCQI_MAKE_OP_PLOTS, 1)}
      if(vcqi_object_exists("VCQI_MAKE_IW_PLOTS") == FALSE){vcqi_global(VCQI_MAKE_IW_PLOTS, 1)}
      if(vcqi_object_exists("VCQI_MAKE_UW_PLOTS") == FALSE){vcqi_global(VCQI_MAKE_UW_PLOTS, 1)}
      if(vcqi_object_exists("VCQI_SAVE_OP_PLOT_DATA") == FALSE){vcqi_global(VCQI_SAVE_OP_PLOT_DATA, 0)}
      if(vcqi_object_exists("VCQI_SAVE_IW_PLOT_DATA") == FALSE){vcqi_global(VCQI_SAVE_IW_PLOT_DATA, 0)}
      if(vcqi_object_exists("VCQI_SAVE_UW_PLOT_DATA") == FALSE){vcqi_global(VCQI_SAVE_UW_PLOT_DATA, 0)}
    }
  }


  # If user specifies VCQI_NUM_DECIMAL_DIGITS, use it; default to 1
  if(vcqi_object_exists("VCQI_NUM_DECIMAL_DIGITS") == FALSE){
    vcqi_global(VCQI_NUM_DECIMAL_DIGITS, 1)
  }
  # Ensure VCQI_NUM_DECIMAL_DIGITS is an integer
  vcqi_global(VCQI_NUM_DECIMAL_DIGITS, as.integer(VCQI_NUM_DECIMAL_DIGITS))

  # Default IWPLOT_SHOWBARS to 0
  if(vcqi_object_exists("IWPLOT_SHOWBARS") == FALSE){
    vcqi_global(IWPLOT_SHOWBARS, 0)
  } else if(IWPLOT_SHOWBARS != 1){vcqi_global(IWPLOT_SHOWBARS, 0)
  } else if(IWPLOT_SHOWBARS == 1){
    vcqi_log_comment(VCP, 3, "Comment",
                     "The global macro IWPLOT_SHOWBARS is set to 1. So VCQI will make barcharts instead of inchworm plots.")
  }

  if(IWPLOT_SHOWBARS == 1){
    vcqi_global(IWPLOT_TYPE, "Bar chart")
  } else {
    vcqi_global(IWPLOT_TYPE, "Inchworm plot")
  }

  # Global that controls right side text for double inchworm plots

  # Default to point estimates only for both distributions
  if(vcqi_object_exists("VCQI_DOUBLE_IWPLOT_CITEXT") == FALSE){
    vcqi_global(VCQI_DOUBLE_IWPLOT_CITEXT, 1)}

  # If user specified something besides 1 or 2 or 3, reset to 1 and issue warning
  if(!VCQI_DOUBLE_IWPLOT_CITEXT %in% c(1,2,3)){
    vcqi_log_comment(VCP, 2, "Warning",
                     paste0("User specified an invalid value of VCQI_DOUBLE_IWPLOT_CITEXT. Valid values include 1 or 2 or 3. User specified ", VCQI_DOUBLE_IWPLOT_CITEXT, ". Resetting this parameter to default value of 1."))
    vcqi_global(VCQI_DOUBLE_IWPLOT_CITEXT, 1)
  }

  # Aggregate databases unless (programmer's option) the user asks not to by setting VCQI_AGGREGATE_VCQI_DATABASES to something other than 1 (like 0)
  if(vcqi_object_exists("AGGREGATE_VCQI_DATABASES") == FALSE){
    vcqi_global(AGGREGATE_VCQI_DATABASES, 1)}

  if(!is.null(warningmsgs)){
    warning(warningmsgs)
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
  assign("level4_layout", level4_layout, envir = global_env())
}
