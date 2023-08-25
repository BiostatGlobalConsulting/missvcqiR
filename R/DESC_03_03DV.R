#' Calculate derived variables for DESC_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (DESC_03_<ANALYSIS_COUNTER>_<DESC_03_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import haven
#' @importFrom utils stack

# DESC_03_03DV R version 1.00 - Biostat Global Consulting - 2023-06-04
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-06-04  1.00      Mia Yu          Original R package version
# *******************************************************************************

DESC_03_03DV <- function(VCP = "DESC_03_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/DESC_03_",ANALYSIS_COUNTER,"_",DESC_03_COUNTER,".rds"))

  vcounter <- DESC_03_COUNTER

  # generate variables for each of the levels, using one variable per level
  # in the order the variables are listed in DESC_03_VARIABLES

  lcounter <- 1
  for (v in seq_along(DESC_03_VARIABLES)){
    va <- rlang::sym(DESC_03_VARIABLES[v])
    var <- get(DESC_03_VARIABLES[v],dat)
    varlabel <- attr(var,"label")

    var <- zap_labels(var)
    if (class(var) == "character"){
      dat <- dat %>% mutate(!!va := ifelse(!!va == "", NA_character_, !!va))
      dat <- dat %>% mutate(!!va := ifelse(str_trim(!!va) == "", NA_character_, !!va))

      dat <- dat %>% mutate(tempvar1 = ifelse((!!va == "DESC_03_SELECTED_VALUE") %in% TRUE, 1, 0))
      dat <- dat %>% mutate(tempvar1 = ifelse(is.na(!!va) &
                                           str_to_upper(DESC_03_DENOMINATOR) == "RESPONDED", NA,tempvar1))
    }

    if (class(var) == "numeric"){
      dat <- dat %>% mutate(tempvar1 = ifelse((!!va == DESC_03_SELECTED_VALUE) %in% TRUE, 1, 0))
      dat <- dat %>% mutate(tempvar1 = ifelse(is.na(!!va) &
                                                str_to_upper(DESC_03_DENOMINATOR) == "RESPONDED", NA,tempvar1))
    }

    if (class(var) == "logical"){
      dat <- dat %>% mutate(!!va := as.numeric(!!va))

      dat <- dat %>% mutate(tempvar1 = ifelse((!!va == DESC_03_SELECTED_VALUE) %in% TRUE, 1, 0))
      dat <- dat %>% mutate(tempvar1 = ifelse(is.na(!!va) &
                                                str_to_upper(DESC_03_DENOMINATOR) == "RESPONDED", NA,tempvar1))
    }

    dat$tempvar1 <- haven::labelled(dat$tempvar1, label = varlabel) %>% suppressWarnings()

    # If this level is a missing value then allow the user to specify the label via input global macros
    # (Actually you could use this options to overwrite the label for any option...we just call it 'missing'
    if (vcqi_object_exists("DESC_03_N_RELABEL_LEVELS")){
      if (DESC_03_N_RELABEL_LEVELS != 0){
        for (i in 1:DESC_03_N_RELABEL_LEVELS){
          relabelva <- get(paste0("DESC_03_RELABEL_LEVEL_",i), envir = .GlobalEnv)
          relabella <- get(paste0("DESC_03_RELABEL_LABEL_",i), envir = .GlobalEnv)
          #TODO: double check this
          if ((as.character(DESC_03_VARIABLES[v]) == as.character(relabelva)) %in% TRUE |
              (is.na(as.character(DESC_03_VARIABLES[v])) & is.na(as.character(relabelva)))) {
            dat$tempvar1 <- haven::zap_label(dat$tempvar1)
            dat$tempvar1 <- haven::labelled(dat$tempvar1, label = relabella) %>% suppressWarnings()
          }
        } #end of DESC_03_N_RELABEL_LEVELS i loop
      }

    } #end of check DESC_03_N_RELABEL_LEVELS

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("desc03_",vcounter,"_",lcounter)

    dat <- dat %>% relocate(paste0("desc03_",vcounter,"_",lcounter),.after = DESC_03_VARIABLES[v])
    lcounter <- lcounter + 1
  } #end of DESC_03_VARIABLES v loop

  # Do vcqi_global in seperate steps since the name depends on another global
  assign(paste0("DESC_03_LVL_COUNT_",vcounter), lcounter-1, envir = .GlobalEnv)
  vcqi_log_comment(VCP, 3, "Global", paste0("Global value DESC_03_LVL_COUNT_", vcounter, " is ", lcounter-1))

  # Now calculate the subtotal variables...setting the outcome to 1 if any
  # of the subtotal components is 1
  #
  # Subtotal

  if (vcqi_object_exists("DESC_03_N_SUBTOTALS")){
    if (DESC_03_N_SUBTOTALS != 0){
      for (i in 1:DESC_03_N_SUBTOTALS){
        dat <- dat %>% mutate(tempvar2 = 0)
        dat <- dat %>% mutate(tempvar3 = 1)

        sublevel <- get(paste0("DESC_03_SUBTOTAL_LEVELS_",i), envir = .GlobalEnv)
        sublabel <- get(paste0("DESC_03_SUBTOTAL_LABEL_",i), envir = .GlobalEnv)

        for (v in seq_along(sublevel)){
          va <- rlang::sym(sublevel[v])
          var <- get(sublevel[v],dat)
          varlabel <- attr(var,"label")

          var <- zap_labels(var)
          if (class(var) == "character"){

            dat <- dat %>% mutate(tempvar2 = ifelse((!!va == "DESC_03_SELECTED_VALUE") %in% TRUE, 1, tempvar2))
            dat <- dat %>% mutate(tempvar3 = ifelse(is.na(!!va), 0,tempvar3))
          }

          if (class(var) == "numeric"){
            dat <- dat %>% mutate(tempvar2 = ifelse((!!va == DESC_03_SELECTED_VALUE) %in% TRUE, 1, tempvar2))
            dat <- dat %>% mutate(tempvar3 = ifelse(is.na(!!va), 0,tempvar3))
          }

          if (class(var) == "logical"){
            dat <- dat %>% mutate(tempvar2 = ifelse((!!va == DESC_03_SELECTED_VALUE) %in% TRUE, 1, tempvar2))
            dat <- dat %>% mutate(tempvar3 = ifelse(is.na(!!va), 0,tempvar3))
          }
        } #end of sublevel v loop

        dat <- dat %>% mutate(tempvar2 = ifelse(tempvar3 == 1 &
                                                  str_to_upper(DESC_03_DENOMINATOR) == "RESPONDED", NA,tempvar2))
        dat <- dat %>% select(-c(tempvar3))
        dat$tempvar2 <- haven::labelled(dat$tempvar2, label = sublabel) %>% suppressWarnings()

        names(dat)[which(names(dat) == "tempvar2")] <- paste0("desc03_",vcounter,"_st",i)

        if (i == 1){
          dat <- dat %>% relocate(paste0("desc03_",vcounter,"_st",i),.after = paste0("desc03_",vcounter,"_",lcounter-1))
        }
        if (i > 1){
          dat <- dat %>% relocate(paste0("desc03_",vcounter,"_st",i),.after = paste0("desc03_",vcounter,"_st",i-1))
        }

      } #end of DESC_03_N_SUBTOTALS i loop
    }

    # Do vcqi_global in seperate steps since the name depends on another global
    assign(paste0("DESC_03_ST_COUNT_",vcounter), DESC_03_N_SUBTOTALS, envir = .GlobalEnv)
    vcqi_log_comment(VCP, 3, "Global", paste0("Global value DESC_03_ST_COUNT_", vcounter, " is ", DESC_03_N_SUBTOTALS))

  } else {
    rm(list = c(paste0("DESC_03_ST_COUNT_",vcounter)),envir = .GlobalEnv) %>% suppressWarnings()
  }#end of check DESC_03_N_SUBTOTALS

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/DESC_03_",ANALYSIS_COUNTER,"_",DESC_03_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
