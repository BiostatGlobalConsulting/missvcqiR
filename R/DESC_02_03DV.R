#' Calculate derived variables for DESC_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (DESC_02_<ANALYSIS_COUNTER>_<DESC_02_COUNTER>)
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import haven
#' @importFrom utils stack

# DESC_02_03DV R version 1.00 - Biostat Global Consulting - 2023-05-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-05-23  1.00      Mia Yu          Original R package version
# *******************************************************************************

DESC_02_03DV <- function(VCP = "DESC_02_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/DESC_02_",ANALYSIS_COUNTER,"_",DESC_02_COUNTER,".rds"))

  vcounter <- 1

  if (str_to_upper(DESC_02_DENOMINATOR) == "ALL"){
    missing <- "TRUE"
  } else {
    missing <- "FALSE"
  }

  for (v in seq_along(DESC_02_VARIABLES)){
    va <- rlang::sym(DESC_02_VARIABLES[v])
    var <- get(DESC_02_VARIABLES[v],dat)
    var2 <- get(DESC_02_VARIABLES[v],dat)

    var2 <- zap_labels(var2)
    if (class(var2) == "character"){
      if (length(which(var == "" %in% TRUE)) > 0){
        var[which(var == "")] <- NA #replace "" to NA
        dat <- dat %>% mutate(!!va := ifelse(!!va == "", NA_character_, !!va))
      }
      if (length(which(str_trim(var) == "" %in% TRUE)) > 0){
        var[which(str_trim(var) == "")] <- NA #replace "" to NA
        dat <- dat %>% mutate(!!va := ifelse(str_trim(!!va) == "", NA_character_, !!va))
      }
    }

    # * What are the observed values of `v'?
    llist <- sort(unique(var),na.last = TRUE)

    if (missing != "TRUE"){
      if (length(which(is.na(llist))) > 0){
        na <- which(is.na(llist))
        llist <- llist[-na]
      }
    }

    # What values of `v' are listed in its value label (if applicable)?
    if (!is.null(attr(var,"labels"))){
      labels <- unique(stack(attr(var, 'labels')))
      lalist <- as.character(labels$ind)
      valist <- sort(labels$values,na.last = TRUE)
      llist <- unique(c(valist,llist))
    }

    # detach the value labels from the variable
    var <- zap_labels(var)
    # get the variable type
    if (class(var) == "character"){
      vtype <- "string"
    }

    if (class(var) == "numeric"){
      vtype <- "number"
    }

    if (class(var) == "logical"){
      dat <- dat %>% mutate(!!va := as.numeric(!!va))
      vtype <- "number"
    }

    lcounter <- 1
    for (l in seq_along(llist)){

      dat <- dat %>% mutate(tempvar1 = ifelse((((!!va == llist[l]) %in% TRUE) | (is.na(!!va) & is.na(llist[l]))) & psweight > 0 & !is.na(psweight),
                                              1, 0))
      assign(paste0("DESC02_VALUE_LEVEL_", lcounter), llist[l], envir = .GlobalEnv)
      dat <- dat %>% mutate(tempvar1 = ifelse((
          is.na(!!va) & (str_to_upper(DESC_02_DENOMINATOR) == "RESPONDED") & psweight > 0 & !is.na(psweight)),
        NA, tempvar1))

      if (vtype == "string"){
        dat$tempvar1 <- haven::labelled(dat$tempvar1, label = llist[l]) %>% suppressWarnings()
      }

      if (vtype == "number"){
        if (length(which(labels$values %in% llist[l])) > 0){
          m <- which(labels$values %in% llist[l])
          dat$tempvar1 <- haven::labelled(dat$tempvar1, label = lalist[m]) %>% suppressWarnings()
        }
      }

      # Allow the user to specify (or overwrite) the label via input global macros
      if (vcqi_object_exists("DESC_02_N_RELABEL_LEVELS")){
        if (DESC_02_N_RELABEL_LEVELS != 0){
          for (i in 1:DESC_02_N_RELABEL_LEVELS){
            relabelva <- get(paste0("DESC_02_RELABEL_LEVEL_",i), envir = .GlobalEnv)
            relabella <- get(paste0("DESC_02_RELABEL_LABEL_",i), envir = .GlobalEnv)
            if ((as.character(llist[l]) == as.character(relabelva)) %in% TRUE |
                (is.na(as.character(llist[l])) & is.na(as.character(relabelva)))) {
              dat$tempvar1 <- haven::zap_label(dat$tempvar1)
              dat$tempvar1 <- haven::labelled(dat$tempvar1, label = relabella) %>% suppressWarnings()
            }
          } #end of DESC_02_N_RELABEL_LEVELS i loop
        }

      } #end of check DESC_02_N_RELABEL_LEVELS

      names(dat)[which(names(dat) == "tempvar1")] <- paste0("desc02_",vcounter,"_",lcounter)

      if (lcounter == 1){
        dat <- dat %>% relocate(paste0("desc02_",vcounter,"_",lcounter),.after = DESC_02_VARIABLES[v])
      }
      if (lcounter > 1){
        dat <- dat %>% relocate(paste0("desc02_",vcounter,"_",lcounter),.after = paste0("desc02_",vcounter,"_",lcounter-1))
      }

      lcounter = lcounter+1

    } #end of llist l loop

    # Do vcqi_global in seperate steps since the name depends on another global
    assign(paste0("DESC_02_LVL_COUNT_",vcounter), lcounter-1, envir = .GlobalEnv)
    vcqi_log_comment(VCP, 3, "Global", paste0("Global value DESC_02_LVL_COUNT_", vcounter, " is ", lcounter-1))

    if (vcqi_object_exists("DESC_02_N_SUBTOTALS")){
      if (DESC_02_N_SUBTOTALS != 0){
        for(i in 1:DESC_02_N_SUBTOTALS){
          dat <- dat %>% mutate(tempvar2 = 0)
          sublevel <- get(paste0("DESC_02_SUBTOTAL_LEVELS_",i), envir = .GlobalEnv)
          sublabel <- get(paste0("DESC_02_SUBTOTAL_LABEL_",i), envir = .GlobalEnv)

          for (j in seq_along(sublevel)){
            dat <- dat %>% mutate(tempvar2 = ifelse(((!!va == sublevel[j]) %in% TRUE | (is.na(!!va) & is.na(sublevel[j]))) & psweight > 0 & !is.na(psweight),
                                                    1,tempvar2))
          } #end of j loop

          dat <- dat %>% mutate(tempvar2 = ifelse((
            is.na(!!va) & (str_to_upper(DESC_02_DENOMINATOR) == "RESPONDED") & psweight > 0 & !is.na(psweight)),
            NA, tempvar2))
          dat$tempvar2 <- haven::labelled(dat$tempvar2, label = sublabel) %>% suppressWarnings()

          names(dat)[which(names(dat) == "tempvar2")] <- paste0("desc02_",vcounter,"_st",i)

          if (i == 1){
            dat <- dat %>% relocate(paste0("desc02_",vcounter,"_st",i),.after = paste0("desc02_",vcounter,"_",lcounter-1))
          }
          if (i > 1){
            dat <- dat %>% relocate(paste0("desc02_",vcounter,"_st",i),.after = paste0("desc02_",vcounter,"_st",i-1))
          }
        } #end of DESC_02_N_SUBTOTALS i loop
      }

      # Do vcqi_global in seperate steps since the name depends on another global
      assign(paste0("DESC_02_ST_COUNT_",vcounter), DESC_02_N_SUBTOTALS, envir = .GlobalEnv)
      vcqi_log_comment(VCP, 3, "Global", paste0("Global value DESC_02_ST_COUNT_", vcounter, " is ", DESC_02_N_SUBTOTALS))

    } else {
      rm(list = c(paste0("DESC_02_ST_COUNT_",vcounter)),envir = .GlobalEnv) %>% suppressWarnings()
    }#end of check DESC_02_N_SUBTOTALS

    vcounter = vcounter+1

  } #end of v DESC_02_VARIABLES loop

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/DESC_02_",ANALYSIS_COUNTER,"_",DESC_02_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
