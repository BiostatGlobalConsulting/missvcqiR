#' Generate output databases for ES_STUD_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr


# ES_STUD_03_04GO R version 1.00 - Biostat Global Consulting - 2023-09-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-11 1.00      Mia Yu          Original R package version
# *******************************************************************************


ES_STUD_03_04GO <- function(VCP = "ES_STUD_03_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(ES_STUD_03_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)
  vcf <- substr(vc,1,1)

  # Change the locals previously just before and on to include the study day for multi lingual purposes
  bb <- language_string(language_use = language_use, str = "OS_11") # Before the Study Day
  ba <- language_string(language_use = language_use, str = "OS_55") # On the Study Day

  # Create tables for the Totals
  # bb
  # Make the first part for invalid doses
  print(paste0(str_to_upper(bb),": Invalid..."))
  make_count_output_database(numerator = paste0("total_invalid_",vc),
                                denominator = paste0("total_non_elig_doses_",vc),
                                estlabel = paste0(str_to_title(bb)," : ",
                                                  language_string(language_use = language_use, str = "OS_58"),
                                                  " : ",pvc, " (%)"),
                                vid = paste0("total_invalid_",vcf),
                                measureid = "ES_STUD_03_b_t",keepnumerator = TRUE)
  # Valid doses and MOVs
  print(paste0(str_to_upper(bb),": Valid and MOV..."))
  make_count_output_database(numerator = paste0("total_correct_validdose_",vc),
                                denominator = paste0("total_elig_doses_",vc),
                                estlabel = paste0(str_to_title(bb)," : ",
                                                  language_string(language_use = language_use, str = "OS_60"),
                                                  " : ",pvc, " (%)"),
                                vid = paste0("total_valid_",vcf),
                                measureid = "ES_STUD_03_b_t",keepnumerator = TRUE)
  make_count_output_database(numerator = paste0("total_mov_",vc),
                                denominator = paste0("total_elig_doses_",vc),
                                estlabel = paste0(str_to_title(bb)," : ",
                                                  language_string(language_use = language_use, str = "OS_22"),
                                                  " : ",pvc, " (%)"),
                                vid = paste0("total_mov_",vcf),
                                measureid = "ES_STUD_03_b_t",keepnumerator = TRUE)
  # ba
  # Make the first part for invalid doses
  print(paste0(str_to_upper(ba),": Invalid..."))
  make_count_output_database(numerator = paste0("total_invalid_",vc),
                                denominator = paste0("total_non_elig_doses_",vc),
                                estlabel = paste0(str_to_title(ba)," : ",
                                                  language_string(language_use = language_use, str = "OS_58"),
                                                  " : ",pvc, " (%)"),
                                vid = paste0("total_invalid_",vcf),
                                measureid = "ES_STUD_03_a_t",keepnumerator = TRUE)
  # Valid doses and MOVs
  print(paste0(str_to_upper(ba),": Valid and MOV..."))
  make_count_output_database(numerator = paste0("total_correct_validdose_",vc),
                                denominator = paste0("total_elig_doses_",vc),
                                estlabel = paste0(str_to_title(ba)," : ",
                                                  language_string(language_use = language_use, str = "OS_60"),
                                                  " : ",pvc, " (%)"),
                                vid = paste0("total_valid_",vcf),
                                measureid = "ES_STUD_03_a_t",keepnumerator = TRUE)
  make_count_output_database(numerator = paste0("total_mov_",vc),
                                denominator = paste0("total_elig_doses_",vc),
                                estlabel = paste0(str_to_title(ba)," : ",
                                                  language_string(language_use = language_use, str = "OS_22"),
                                                  " : ",pvc, " (%)"),
                                vid = paste0("total_mov_",vcf),
                                measureid = "ES_STUD_03_a_t",keepnumerator = TRUE)

  # Create tables for Antigens
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
  for (v in seq_along(vloop)){
    # restrict attention to antigens in the MOV_OUTPUT_DOSE_LIST
    if(str_to_upper(vloop[v]) %in% str_to_upper(MOV_OUTPUT_DOSE_LIST) |
       paste0(str_to_upper(vloop[v]),2) %in% str_to_upper(MOV_OUTPUT_DOSE_LIST)){
      # Create tables for the bot before and after

      # bb
      # Make the first part for invalid doses
      print(paste0(str_to_upper(bb),": Invalid..."))
      make_count_output_database(numerator = paste0("invalid_",vloop[v],"_",vc),
                                    denominator = paste0(vloop[v],"_non_elig_doses_",vc),
                                    estlabel = paste0(str_to_title(bb)," : ",
                                                      language_string(language_use = language_use, str = "OS_59"),
                                                      " ", str_to_upper(vloop[v])," : ",pvc, " (%)"),
                                    vid = paste0(vloop[v],"_invalid_",vcf),
                                    measureid = "ES_STUD_03_b_a",keepnumerator = TRUE)
      # Valid doses and MOVs
      print(paste0(str_to_upper(bb),": Valid and MOV..."))
      make_count_output_database(numerator = paste0("correct_validdose_",vloop[v],"_",vc),
                                    denominator = paste0(vloop[v],"_elig_doses_",vc),
                                    estlabel = paste0(str_to_title(bb)," : ",
                                                      language_string(language_use = language_use, str = "OS_61"),
                                                      " ", str_to_upper(vloop[v])," : ",pvc, " (%)"),
                                    vid = paste0(vloop[v],"_valid_",vcf),
                                    measureid = "ES_STUD_03_b_a",keepnumerator = TRUE)
      make_count_output_database(numerator = paste0("mov_",vloop[v],"_",vc),
                                    denominator = paste0(vloop[v],"_elig_doses_",vc),
                                    estlabel = paste0(str_to_title(bb)," : ",
                                                      language_string(language_use = language_use, str = "OS_23"),
                                                      " ", str_to_upper(vloop[v])," : ",pvc, " (%)"),
                                    vid = paste0(vloop[v],"_mov_",vcf),
                                    measureid = "ES_STUD_03_b_a",keepnumerator = TRUE)

      # ba
      # Make the first part for invalid doses
      print(paste0(str_to_upper(ba),": Invalid..."))
      make_count_output_database(numerator = paste0("invalid_",vloop[v],"_",vc),
                                    denominator = paste0(vloop[v],"_non_elig_doses_",vc),
                                    estlabel = paste0(str_to_title(ba)," : ",
                                                      language_string(language_use = language_use, str = "OS_59"),
                                                      " ", str_to_upper(vloop[v])," : ",pvc, " (%)"),
                                    vid = paste0(vloop[v],"_invalid_",vcf),
                                    measureid = "ES_STUD_03_a_a",keepnumerator = TRUE)
      # Valid doses and MOVs
      print(paste0(str_to_upper(ba),": Valid and MOV..."))
      make_count_output_database(numerator = paste0("correct_validdose_",vloop[v],"_",vc),
                                    denominator = paste0(vloop[v],"_elig_doses_",vc),
                                    estlabel = paste0(str_to_title(ba)," : ",
                                                      language_string(language_use = language_use, str = "OS_61"),
                                                      " ", str_to_upper(vloop[v])," : ",pvc, " (%)"),
                                    vid = paste0(vloop[v],"_valid_",vcf),
                                    measureid = "ES_STUD_03_a_a",keepnumerator = TRUE)
      make_count_output_database(numerator = paste0("mov_",vloop[v],"_",vc),
                                    denominator = paste0(vloop[v],"_elig_doses_",vc),
                                    estlabel = paste0(str_to_title(ba)," : ",
                                                      language_string(language_use = language_use, str = "OS_23"),
                                                      " ", str_to_upper(vloop[v])," : ",pvc, " (%)"),
                                    vid = paste0(vloop[v],"_mov_",vcf),
                                    measureid = "ES_STUD_03_a_a",keepnumerator = TRUE)

    }
  } #end of vloop v loop

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
