#' Export datasets to Excel for ES_STUD_03 with long table
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr
#' @importFrom utils glob2rx
#' @import dplyr

# ES_STUD_03_05TOST R version 1.00 - Biostat Global Consulting - 2023-09-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-11  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_03_05TOST <- function(VCP = "ES_STUD_03_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_ES_STUD_03_T", "TO_ES_STUD_03_T_columnlabel", "TO_ES_STUD_03_T_formatnum","TO_ES_STUD_03_T_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(list = c("TO_ES_STUD_03_A", "TO_ES_STUD_03_A_columnlabel", "TO_ES_STUD_03_A_formatnum","TO_ES_STUD_03_A_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(list = c("TO_ES_STUD_03_AL", "TO_ES_STUD_03_AL_columnlabel", "TO_ES_STUD_03_AL_formatnum","TO_ES_STUD_03_AL_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()

  vc <- str_to_lower(ES_STUD_03_VALID_OR_CRUDE)
  if (vc == "crude"){
    pvc <- language_string(language_use = language_use, str = "OS_14") # Crude
  } else {
    pvc <- language_string(language_use = language_use, str = "OS_80") # Valid
  }
  vcf <- substr(vc,1,1)

  bb <- language_string(language_use = language_use, str = "OS_373") # BSD
  ba <- language_string(language_use = language_use, str = "OS_374") # OSD

  string1 <- c("b","a")
  string2 <- c("t","a")

  for (i in seq_along(string1)){
    for (j in seq_along(string2)){
      # Do vcqi_global in seperate steps since the name depends on another global
      assign(paste0("ES_STUD_03_",string1[i],"_",string2[j],"_TO_TITLE"),
             ES_STUD_03_TO_TITLE, envir = .GlobalEnv)
      vcqi_log_comment(VCP, 3, "Global",
                       paste0("Global value ES_STUD_03_",string1[i],"_",string2[j],"_TO_TITLE is ",
                              ES_STUD_03_TO_TITLE))

      for (k in 1:7){
        # Do vcqi_global in seperate steps since the name depends on another global
        footnote <- get(paste0("ES_STUD_03_TO_FOOTNOTE_",k), envir = .GlobalEnv)
        assign(paste0("ES_STUD_03_",string1[i],"_",string2[j],"_TO_FOOTNOTE_",k),
               footnote, envir = .GlobalEnv)
        vcqi_log_comment(VCP, 3, "Global",
                         paste0("Global value ES_STUD_03_",string1[i],"_",string2[j],"_TO_FOOTNOTE_",k," is ",
                                footnote))
      } #end of k 1-7 loop for footnote 1-7

      # Do vcqi_global in seperate steps since the name depends on another global
      #BSD - Before Study Day
      foonote8 <- paste0(language_string(language_use = language_use, str = "OS_373"),
                         " - ",
                         language_string(language_use = language_use, str = "OS_11"))
      assign(paste0("ES_STUD_03_",string1[i],"_",string2[j],"_TO_FOOTNOTE_8"),
             foonote8, envir = .GlobalEnv)
      vcqi_log_comment(VCP, 3, "Global",
                       paste0("Global value ES_STUD_03_",string1[i],"_",string2[j],"_TO_FOOTNOTE_8 is ",
                              foonote8))

      # Do vcqi_global in seperate steps since the name depends on another global
      #OSD - On Study Day
      foonote9 <- paste0(language_string(language_use = language_use, str = "OS_374"),
                         " - ",
                         language_string(language_use = language_use, str = "OS_55"))
      assign(paste0("ES_STUD_03_",string1[i],"_",string2[j],"_TO_FOOTNOTE_9"),
             foonote9, envir = .GlobalEnv)
      vcqi_log_comment(VCP, 3, "Global",
                       paste0("Global value ES_STUD_03_",string1[i],"_",string2[j],"_TO_FOOTNOTE_9 is ",
                              foonote9))

    } #end of t a loop

    string3 <- get(paste0("b",string1[i]))

    print(paste0(string3,": ",language_string(language_use = language_use, str = "OS_230"),"...")) #Invalid...

    if (!vcqi_object_exists("ES_STUD_03_TO_SUBTITLE_FOR_ALL")){
      #Across all Antigens
      vcqi_global(ES_STUD_03_TO_SUBTITLE_FOR_ALL,language_string(language_use = language_use, str = "OS_138"))
    }

    # Do vcqi_global in seperate steps since the name depends on another global
    assign(paste0("ES_STUD_03_",string1[i],"_t_TO_SUBTITLE"),
           ES_STUD_03_TO_SUBTITLE_FOR_ALL, envir = .GlobalEnv)
    vcqi_log_comment(VCP, 3, "Global",
                     paste0("Global value ES_STUD_03_",string1[i],"_t_TO_SUBTITLE is ",
                            ES_STUD_03_TO_SUBTITLE_FOR_ALL))

    # Invalid
    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_invalid_",vcf,"_database.rds"),
      variable = "numerator", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_58"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Received Invalid Dose  - `pvc' (N)

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_invalid_",vcf,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_58"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_1"))) #`b`b'': Received Invalid Dose  - `pvc' (%)

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_invalid_",vcf,"_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_78"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Total Number of Doses for Which Children Were Not Eligible - `pvc' (N))

    # Valid doses and MOVs
    print(paste0(string3,": ",language_string(language_use = language_use, str = "OS_231"),"...")) #Valid and MOV...

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_valid_",vcf,"_database.rds"),
      variable = "numerator", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_60"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Received Valid Dose  - `pvc' (N))

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_valid_",vcf,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_60"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_1"))) #`b`b'': Received Valid Dose  - `pvc' (%)

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_mov_",vcf,"_database.rds"),
      variable = "numerator", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_22"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Experienced MOV  - `pvc' (N)

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_mov_",vcf,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_22"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_1"))) #`b`b'': Experienced MOV  - `pvc' (%)

    make_table_column(
      tablename = paste0("TO_ES_STUD_03_",string1[i],"_t"),
      dbfilename = paste0("ES_STUD_03_",string1[i],"_t_",ANALYSIS_COUNTER,"_total_mov_",vcf,"_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE,
      label = paste0(string3,": ",
                     language_string(language_use = language_use, str = "OS_76"),
                     ": ", pvc, " ",
                     language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Total Number of Doses for Which Children Were Eligible - `pvc' (N)

  } #end of b a loop

  # Merge together the before and on study day total tables before exporting

  dat <- TO_ES_STUD_03_B_T

  varlist <- grep(glob2rx("c*"), names(dat), value=TRUE)
  for (v in seq_along(varlist)){
    names(dat)[which(names(dat) == varlist[v])] <- paste0("before_",varlist[v])
  } #end of varlist v loop

  dat2 <- TO_ES_STUD_03_A_T
  dat <- full_join(dat,dat2, by = c("level4id","name"))

  assign("TO_ES_STUD_03_T",dat,envir = .GlobalEnv)
  assign("TO_ES_STUD_03_T_columnlabel", c(TO_ES_STUD_03_B_T_columnlabel,TO_ES_STUD_03_A_T_columnlabel),envir = .GlobalEnv)
  assign("TO_ES_STUD_03_T_formatnum", c(TO_ES_STUD_03_B_T_formatnum,TO_ES_STUD_03_A_T_formatnum),envir = .GlobalEnv)
  assign("TO_ES_STUD_03_T_colformat", c(TO_ES_STUD_03_B_T_colformat,TO_ES_STUD_03_A_T_colformat),envir = .GlobalEnv)

  # Now export to excel
  export_table_to_excel(indicator = "ES_STUD_03_b_t",
                        sheet = paste0("ES_STUD_03 ",ANALYSIS_COUNTER," - ",
                                       language_string(language_use = language_use, str = "OS_70"),"6a"), #Table6a
                        tablename = "TO_ES_STUD_03_T",
                        brief = FALSE)

  # Create tables for Antigens
  # Create a local with the list of doses so we can append the datasets together
  vloop1 <- str_to_lower(RI_SINGLE_DOSE_LIST)
  vloop1 <- vloop1[-which(vloop1 == "visit")]
  vloop2 <- NULL
  for (i in 2:9) {
    if (vcqi_object_exists(paste0("RI_MULTI_", i, "_DOSE_LIST"))) {
      dl <- get(paste0("RI_MULTI_", i, "_DOSE_LIST"))
      if (!is.null(dl) & length(dl > 0)) {
        vloop2 <- c(vloop2, dl)
      }
    }
  } #end of i loop
  vloop2 <- str_to_lower(vloop2)
  vloop <- c(vloop1, vloop2)
  doselist <- NULL

  for (v in seq_along(vloop)){
    # restrict attention to antigens in the MOV_OUTPUT_DOSE_LIST
    if(str_to_upper(vloop[v]) %in% str_to_upper(MOV_OUTPUT_DOSE_LIST) |
       paste0(str_to_upper(vloop[v]),2) %in% str_to_upper(MOV_OUTPUT_DOSE_LIST)){
      doselist <- c(doselist, str_to_upper(vloop[v]))

      #  Create tables for the both before and after
      for (i in seq_along(string1)){

        if (!vcqi_object_exists("ES_STUD_03_TO_SUBTITLE_ANTIGEN")){
          vcqi_global(ES_STUD_03_TO_SUBTITLE_ANTIGEN,language_string(language_use = language_use, str = "OS_136"))
        }

        # Do vcqi_global in seperate steps since the name depends on another global
        assign(paste0("ES_STUD_03_",string1[i],"_a_TO_SUBTITLE"),
               ES_STUD_03_TO_SUBTITLE_ANTIGEN, envir = .GlobalEnv)
        vcqi_log_comment(VCP, 3, "Global",
                         paste0("Global value ES_STUD_03_",string1[i],"_a_TO_SUBTITLE is ",
                                ES_STUD_03_TO_SUBTITLE_ANTIGEN))

        string3 <- get(paste0("b",string1[i]))

        # Invalid
        print(paste0(string3, ": ",
                     language_string(language_use = language_use, str = "OS_230"), " ",
                     str_to_upper(vloop[v]), "...")) #Invalid `=upper("`v'")'..."

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_invalid_",vcf,"_database.rds"),
          variable = "numerator", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_59"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Received Invalid Dose for `=upper("`v'")' (N)

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_invalid_",vcf,"_database.rds"),
          variable = "estimate", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_59"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_1"))) #`b`b'': Received Invalid Dose for `=upper("`v'")' (%)

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_invalid_",vcf,"_database.rds"),
          variable = "n", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_79"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Total Number of Doses for Which Children Were Not Eligible for `=upper("`v'")' (N))

        # Valid doses and MOVs
        print(paste0(string3, ": ",
                     language_string(language_use = language_use, str = "OS_231"), " ",
                     str_to_upper(vloop[v]), "...")) #Valid doses and MOVs `=upper("`v'")'..."

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_valid_",vcf,"_database.rds"),
          variable = "numerator", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_61"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Received Valid Dose for `=upper("`v'")' (N)

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_valid_",vcf,"_database.rds"),
          variable = "estimate", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_61"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_1"))) #`b`b'': Received Valid Dose for `=upper("`v'")' (%)

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_mov_",vcf,"_database.rds"),
          variable = "numerator", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_23"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Experienced MOV for `=upper("`v'")' (N)

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_mov_",vcf,"_database.rds"),
          variable = "estimate", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_23"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_1"))) #`b`b'': Experienced MOV for `=upper("`v'")' (%)

        make_table_column(
          tablename = paste0("TO_ES_STUD_03_a_",vloop[v]),
          dbfilename = paste0("ES_STUD_03_",string1[i],"_a_",ANALYSIS_COUNTER,"_",vloop[v],"_mov_",vcf,"_database.rds"),
          variable = "n", replacevar = NA, noannotate = TRUE,
          label = paste0(string3,": ",
                         language_string(language_use = language_use, str = "OS_77"),
                         " ", str_to_upper(vloop[v]), " ",
                         language_string(language_use = language_use, str = "OS_2"))) #`b`b'': Total Number of Doses for Which Children Were Eligible for `=upper("`v'")' (N)

      } #end of string1 i loop

    }
  } #end of vloop v loop

  start <- doselist[1]
  print(start)
  doselist <- doselist[-1]
  print(doselist)

  # Frist create AL since it does not require changing var names
  dat_long <- get(paste0("TO_ES_STUD_03_A_",start))
  dat_long <- dat_long %>% mutate(Dose = start) %>% arrange(level4id)
  dat_long [nrow(dat_long)+1,] <- NA #Add an empty row for better looking

  label_long <- get(paste0("TO_ES_STUD_03_A_",start,"_columnlabel"), envir = .GlobalEnv)
  format_long <- get(paste0("TO_ES_STUD_03_A_",start,"_formatnum"), envir = .GlobalEnv)
  colf_long <- get(paste0("TO_ES_STUD_03_A_",start,"_colformat"), envir = .GlobalEnv)

  for (d in seq_along(doselist)){
    dat2_long <- get(paste0("TO_ES_STUD_03_A_",doselist[d]))
    dat2_long <- dat2_long %>% mutate(Dose = doselist[d]) %>% arrange(level4id)
    dat2_long [nrow(dat2_long)+1,] <- NA #Add an empty row for better looking

    dat_long <- rbind(dat_long,dat2_long)

  } #end of doselist d loop

  dat_long <- dat_long %>% relocate(Dose, .after = name) %>% mutate(level4id = row_number(),
                                                                    Dose = ifelse(is.na(name),NA,Dose))

  # replace "BCG" with "the dose: for label
  for (l in seq_along(label_long)){
    label_long[l] <- gsub("BCG", "the dose", label_long[l], fixed = TRUE)
  }

  assign("TO_ES_STUD_03_AL",dat_long,envir = .GlobalEnv)
  assign("TO_ES_STUD_03_AL_columnlabel",label_long,envir = .GlobalEnv)
  assign("TO_ES_STUD_03_AL_formatnum",format_long,envir = .GlobalEnv)
  assign("TO_ES_STUD_03_AL_colformat",colf_long,envir = .GlobalEnv)

  # Now export to excel
  export_ALT_to_excel(indicator = "ES_STUD_03_b_al",
                        sheet = paste0("ES_STUD_03 ",ANALYSIS_COUNTER," - ",
                                       language_string(language_use = language_use, str = "OS_70"),"6b long"), #Table6b long
                        tablename = "TO_ES_STUD_03_AL")

  # Open the first dataset
  dat <- get(paste0("TO_ES_STUD_03_A_",start))

  label <- get(paste0("TO_ES_STUD_03_A_",start,"_columnlabel"), envir = .GlobalEnv)
  format <- get(paste0("TO_ES_STUD_03_A_",start,"_formatnum"), envir = .GlobalEnv)
  colf <- get(paste0("TO_ES_STUD_03_A_",start,"_colformat"), envir = .GlobalEnv)

  varlist <- grep(glob2rx("c*"), names(dat), value=TRUE)
  for (v in seq_along(varlist)){
    names(dat)[which(names(dat) == varlist[v])] <- paste0(start,"_",varlist[v])
  } #end of varlist v loop

  for (d in seq_along(doselist)){
    dat2 <- get(paste0("TO_ES_STUD_03_A_",doselist[d]))

    varlist <- grep(glob2rx("c*"), names(dat2), value=TRUE)
    for (v in seq_along(varlist)){
      names(dat2)[which(names(dat2) == varlist[v])] <- paste0(doselist[d],"_",varlist[v])
    } #end of varlist v loop

    dat <- full_join(dat,dat2,by = c("level4id","name"))

    doselabel <- get(paste0("TO_ES_STUD_03_A_",doselist[d],"_columnlabel"), envir = .GlobalEnv)
    doseformat <- get(paste0("TO_ES_STUD_03_A_",doselist[d],"_formatnum"), envir = .GlobalEnv)
    dosecolf <- get(paste0("TO_ES_STUD_03_A_",doselist[d],"_colformat"), envir = .GlobalEnv)

    label <- c(label,doselabel)
    format <- c(format, doseformat)
    colf <- c(colf,dosecolf)

    assign("TO_ES_STUD_03_A",dat,envir = .GlobalEnv)
    assign("TO_ES_STUD_03_A_columnlabel",label,envir = .GlobalEnv)
    assign("TO_ES_STUD_03_A_formatnum",format,envir = .GlobalEnv)
    assign("TO_ES_STUD_03_A_colformat",colf,envir = .GlobalEnv)
  } #end of doselist d loop

  # Now export to excel
  export_table_to_excel(indicator = "ES_STUD_03_b_a",
                        sheet = paste0("ES_STUD_03 ",ANALYSIS_COUNTER," - ",
                                       language_string(language_use = language_use, str = "OS_70"),"6b"), #Table6b
                        tablename = "TO_ES_STUD_03_A",
                        brief = FALSE)

  rm(list = c("TO_ES_STUD_03_T", "TO_ES_STUD_03_T_columnlabel", "TO_ES_STUD_03_T_formatnum","TO_ES_STUD_03_T_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(list = c("TO_ES_STUD_03_A", "TO_ES_STUD_03_A_columnlabel", "TO_ES_STUD_03_A_formatnum","TO_ES_STUD_03_A_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()
  rm(list = c("TO_ES_STUD_03_AL", "TO_ES_STUD_03_AL_columnlabel", "TO_ES_STUD_03_AL_formatnum","TO_ES_STUD_03_AL_colformat"),
     envir = .GlobalEnv) %>% suppressWarnings()

  rm(TO_ES_STUD_03_T_CN, envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_ES_STUD_03_A_CN, envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_ES_STUD_03_AL_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
