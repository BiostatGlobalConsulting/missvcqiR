#' Pre-process dataset to for DESC_02_05TOST/DESC_03_05TOST
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return datasets
#'
#' @import dplyr
#' @import stringr
#' @rawNamespace import(rlang, except = c(local_options,with_options))

# format_desc0203_database R version 1.01 - Biostat Global Consulting - 2023-09-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-03  1.00      Mia Yu          Original R package version
# 2023-09-29  1.01      Mia Yu          Added multi lingual globals
# *******************************************************************************


format_desc0203_database <- function(VCP = "format_desc0203_database", i, db, wtd, indicator, datname, vid) {

  dat2 <- vcqi_read(datname)

  # If the analysis is not weighted, include a column with n at the far right
  dat2 <- dat2 %>% mutate(n = round(n))

  # Create variables that are made in make_tables_from_DESC_0203
  pct <- rlang::sym(paste0("pct",i))
  DESC_labels <- get(paste0(indicator,"_labels_",vid), envir = .GlobalEnv)
  pctl <- DESC_labels$label[which(DESC_labels$var == paste0("pct",i))]

  if (vcqi_object_exists(paste0(indicator,"_LIST_N_BEFORE_PCT"))){
    nbp <- get(paste0(indicator,"_LIST_N_BEFORE_PCT"), envir = .GlobalEnv)

    if (str_to_upper(nbp) == "YES"){

      dat2 <- dat2 %>% mutate(tempvar1 = round(!!pct*n))

      dat2$tempvar1 <- haven::labelled(dat2$tempvar1,
                                       label = paste0(pctl, " ",language_string(language_use = language_use, str = "OS_2")))
                                       #(N)
      dat2 <- dat2 %>% relocate(tempvar1, .before = !!pct)
      names(dat2)[which(names(dat2) == "tempvar1")] <- paste0("n",i)
      assign(paste0("n",i),paste0(pctl, " ", language_string(language_use = language_use, str = "OS_2")),
             envir = .GlobalEnv)
             #(N)
    }
  }

  if (vcqi_object_exists(paste0(indicator,"_LIST_NWTD_BEFORE_PCT"))){
    nwbp <- get(paste0(indicator,"_LIST_NWTD_BEFORE_PCT"), envir = .GlobalEnv)

    if (wtd == 1 & str_to_upper(nwbp) == "YES"){

      dat2 <- dat2 %>% mutate(tempvar1 = round(!!pct*nwtd, digits = 1))

      dat2$tempvar1 <- haven::labelled(dat2$tempvar1,
                                       label = paste0(pctl, " ",language_string(language_use = language_use, str = "OS_3")))
                                                      #(Weighted N)
      dat2 <- dat2 %>% relocate(tempvar1, .before = !!pct)
      names(dat2)[which(names(dat2) == "tempvar1")] <- paste0("nwtd",i)
      assign(paste0("nwtd",i),paste0(pctl, " ", language_string(language_use = language_use, str = "OS_3")),
             envir = .GlobalEnv) #(Weighted N)

    }
  }

  if (vcqi_object_exists(paste0(indicator,"_SUPPRESS_CI_OUTPUT"))){
    supci <- get(paste0(indicator,"_SUPPRESS_CI_OUTPUT"), envir = .GlobalEnv)
  } else {
    supci <- 0
  }

  if (str_to_upper(supci) == "YES" | supci == 1){
    assign("suppress_cis",1, envir = .GlobalEnv)
  } else {
    assign("suppress_cis",0, envir = .GlobalEnv)
  }

  assign(paste0("pct",i), pctl, envir = .GlobalEnv)

  # Only keep the variables that are necessary
  vartokeep <- grep(glob2rx(paste0("*",i)), names(dat2), value=TRUE)
  dat2 <- dat2 %>% select(c(level,name,level4id,level4name, all_of(vartokeep)))

  # Now rename variables for formatting purposes in make_stable_column
  names(dat2)[which(names(dat2) == paste0("pct",i))] <- "estimate"
  if (paste0("n",i) %in% names(dat2)){
    names(dat2)[which(names(dat2) == paste0("n",i))] <- "n"
  }

  if (paste0("nwtd",i) %in% names(dat2)){
    names(dat2)[which(names(dat2) == paste0("nwtd",i))] <- "nwtd"
  }

  if (paste0("ciul",i) %in% names(dat2)){
    names(dat2)[which(names(dat2) == paste0("ciul",i))] <- "ciul"
  }

  if (paste0("cill",i) %in% names(dat2)){
    names(dat2)[which(names(dat2) == paste0("cill",i))] <- "cill"
  }

  # Now rename variables for the purposes of
  saveRDS(dat2,file = paste0(VCQI_OUTPUT_FOLDER,"/",db,"_",i,".rds"))

  }
