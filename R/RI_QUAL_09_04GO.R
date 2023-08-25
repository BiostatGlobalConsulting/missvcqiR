#' Generate output databases for RI_QUAL_09
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import haven

# RI_QUAL_09_04GO R version 1.04 - Biostat Global Consulting - 2023-07-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-30  1.00      Mia Yu          Original R version
# 2022-10-07  1.01      Mia Yu          Fix label dataset
# 2022-10-13  1.02      Mia Yu          Package version
# 2022-10-18  1.03      Mia Yu          Add variable labels
# 2023-07-21  1.04      Caitlin Clary   Update labels (MOV -> MOSV)
# *******************************************************************************

RI_QUAL_09_04GO <- function(VCP = "RI_QUAL_09_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(RI_QUAL_09_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)

  dat <- vcqi_read(file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,".rds"))

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){

    print(MOV_OUTPUT_DOSE_LIST[d])

    go <- NULL

    filename <- paste0("RI_QUAL_09_", ANALYSIS_COUNTER, "_",
                       MOV_OUTPUT_DOSE_LIST[d], "_database.rds")

    if (!vcqi_object_exists("VCQI_DATABASES")){VCQI_DATABASES <- NULL}
    vcqi_global(VCQI_DATABASES, c(VCQI_DATABASES, filename))

    l <- 4
    for (j in 1:nrow(level4_layout)){
      l4name <- level4_layout$label[j]
      rowtype <- level4_layout$rowtype[j]

      if (rowtype == "DATA_ROW"){
        condition <- level4_layout$condition[j]

        # Count respondents meeting the level4 condition(s)
        subdat <- subset(dat, eval(rlang::parse_expr(condition)))

        hadmov <- get(paste0("child_had_mov_", MOV_OUTPUT_DOSE_LIST[d], "_", vc), subdat)
        n1 = length(hadmov[which(!is.na(hadmov))])
        n2 = length(hadmov[which(hadmov %in% 1)])

        uncormov <- get(paste0("child_had_uncor_mov_", MOV_OUTPUT_DOSE_LIST[d], "_", vc), subdat)
        n3 = length(uncormov[which(uncormov %in% 1)])

        cormov <- get(paste0("child_had_cor_mov_", MOV_OUTPUT_DOSE_LIST[d], "_", vc), subdat)
        n4 = length(cormov[which(cormov %in% 1)])

        gotemp <- data.frame(
          level = l,
          level4id = j,
          level4name = l4name,
          outcome = paste0("Percent of respondents with MOSV for ",
                           str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
          dose = str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
          n_eligible = n1,
          n_mov = n2,
          n_uncor_mov = n3,
          n_cor_mov = n4
        )
        go <- rbind(go, gotemp)

      } # end of data_row

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(
          level = l,
          level4id = j,
          level4name = "BLANK_ROW",
          outcome = paste0("Percent of respondents with MOSV for ",
                           str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
          dose = str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
          n_eligible = NA,
          n_mov = NA,
          n_uncor_mov = NA,
          n_cor_mov = NA
        )
        go <- rbind(go,gotemp)
      } # end of blank_row

      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(
          level = l,
          level4id = j,
          level4name = l4name,
          outcome = paste0("Percent of respondents with MOSV for ",
                           str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
          dose = str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
          n_eligible = NA,
          n_mov = NA,
          n_uncor_mov = NA,
          n_cor_mov = NA
        )
        go <- rbind(go,gotemp)
      } # end of label_only

    } # end of j loop

    saveRDS(go, file = paste0(VCQI_OUTPUT_FOLDER,"/",filename))

    go <- go %>%
      mutate(name = NA,
             level4name = as.character(level4name),
             # Append the name to the front of the level4name if we have a single
             # stratifier; otherwise leave it off.
             name = ifelse(!is.na(level4name), level4name, name),
             name = ifelse(level4name == "BLANK_ROW", NA, name))

    go <- go %>%
      relocate(c(name, level4id, level4name), .after = level) %>%
      select(-level4name) %>%
      arrange(level, level4id) # Arranging by level4id no longer matches Stata output

    go$level <- haven::labelled(go$level, label = "Stratum geographic level") %>% suppressWarnings()
    go$level4id <- haven::labelled(go$level4id, label = "Sub-stratum ID") %>% suppressWarnings()
    go$name <- haven::labelled(go$name, label = "Stratum name for table output") %>% suppressWarnings()
    go$outcome <- haven::labelled(go$outcome, label = "Outcome") %>% suppressWarnings()
    go$dose <- haven::labelled(go$dose, label = "Dose") %>% suppressWarnings()
    go$n_eligible <- haven::labelled(
      go$n_eligible,
      label = "Number of respondents with vx dates when age-eligible for the dose") %>% suppressWarnings()
    go$n_mov <- haven::labelled(go$n_mov, label = "Number of missed opportunities (MOSVs)") %>% suppressWarnings()
    go$n_uncor_mov <- haven::labelled(go$n_uncor_mov, label = "Number of uncorrected MOSVs") %>% suppressWarnings()
    go$n_cor_mov <- haven::labelled(go$n_cor_mov, label = "Number of corrected MOSVs") %>% suppressWarnings()

    saveRDS(go, file = paste0(VCQI_OUTPUT_FOLDER,"/",filename))

  } #end of d loop

  print("Totals...")

  go <- NULL

  if (!vcqi_object_exists("VCQI_DATABASES")){VCQI_DATABASES <- NULL}
  vcqi_global(
    VCQI_DATABASES,
    c(VCQI_DATABASES, paste0("RI_QUAL_09_", ANALYSIS_COUNTER, "_anydose_database.rds")))

  l <- 4
  for (j in 1:nrow(level4_layout)){
    l4name <- level4_layout$label[j]
    rowtype <- level4_layout$rowtype[j]

    if (rowtype == "DATA_ROW"){
      condition <- level4_layout$condition[j]

      # Count respondents meeting the level4 condition(s)
      subdat <- subset(dat, eval(rlang::parse_expr(condition)))

      hadmov <- get(paste0("child_had_mov_",vc),subdat)
      n1 = length(hadmov[which(!is.na(hadmov))])
      n2 = length(hadmov[which(hadmov %in% 1)])

      uncormov <- get(paste0("child_had_only_uncor_mov_",vc),subdat)
      n3 = length(uncormov[which(uncormov %in% 1)])

      cormov <- get(paste0("child_had_only_cor_mov_",vc),subdat)
      n4 = length(cormov[which(cormov %in% 1)])

      gotemp <- data.frame(
        level = l, level4id = j, level4name = l4name, outcome = "Percent of respondents with MOSV for any dose",
        dose = "ALLDOSES", n_eligible = n1, n_mov = n2, n_uncor_mov = n3, n_cor_mov = n4
      )
      go <- rbind(go,gotemp)

    } #end of data_row

    if (rowtype == "BLANK_ROW"){
      gotemp <- data.frame(
        level = l, level4id = j, level4name = "BLANK_ROW", outcome = "Percent of respondents with MOSV for any dose",
        dose = "ALLDOSES", n_eligible = NA, n_mov = NA, n_uncor_mov = NA, n_cor_mov = NA
      )
      go <- rbind(go,gotemp)
    } #end of blank_row

    if (rowtype == "LABEL_ONLY"){
      gotemp <- data.frame(
        level = l, level4id = j, level4name = l4name, outcome = "Percent of respondents with MOSV for any dose",
        dose = "ALLDOSES", n_eligible = NA, n_mov = NA, n_uncor_mov = NA, n_cor_mov = NA
      )
      go <- rbind(go,gotemp)
    } #end of label_only

  } #end of j loop

  saveRDS(go, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_database.rds"))

  go <- go %>%
    mutate(name = NA,
           level4name = as.character(level4name),
           # Append the name to the front of the level4name if we have a single
           # stratifier; otherwise leave it off.
           name = ifelse(!is.na(level4name), level4name, name),
           name = ifelse(level4name == "BLANK_ROW", NA, name))

  go <- go %>%
    relocate(c(name, level4id, level4name), .after = level) %>%
    select(-level4name) %>%
    arrange(level, level4id) # Arranging by level4id no longer matches Stata output

  go$level <- haven::labelled(go$level, label = "Stratum level") %>% suppressWarnings()
  go$level4id <- haven::labelled(go$level4id, label = "Sub-stratum ID") %>% suppressWarnings()
  go$name <- haven::labelled(go$name, label = "Stratum name for table output") %>% suppressWarnings()
  go$outcome <- haven::labelled(go$outcome, label = "Outcome") %>% suppressWarnings()
  go$dose <- haven::labelled(go$dose, label = "Dose") %>% suppressWarnings()
  go$n_eligible <- haven::labelled(go$n_eligible,
                                   label = "Number of respondents with vx dates when age-eligible for the dose") %>% suppressWarnings()
  go$n_mov <- haven::labelled(go$n_mov, label = "Number of missed opportunities (MOSVs)") %>% suppressWarnings()
  go$n_uncor_mov <- haven::labelled(go$n_uncor_mov, label = "Number of uncorrected MOSVs") %>% suppressWarnings()
  go$n_cor_mov <- haven::labelled(go$n_cor_mov, label = "Number of corrected MOSVs") %>% suppressWarnings()

  saveRDS(go, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_", ANALYSIS_COUNTER, "_anydose_database.rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

