#' Generate output databases for ES_STUD_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr


# ES_STUD_02_04GO R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08 1.00      Mia Yu          Original R package version
# *******************************************************************************


ES_STUD_02_04GO <- function(VCP = "ES_STUD_02_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(ES_STUD_02_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)
  vcf <- substr(vc,1,1)

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])

    make_unwtd_output_database(variable = paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_valid_",vc),
                                  estlabel = paste0("Received valid dose for ",
                                                    str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
                                                    " - ", pvc, " (%)"),
                                  vid = paste0(MOV_OUTPUT_DOSE_LIST[d],"_valid_",vcf),
                                  measureid = "ES_STUD_02", keepnumerator = FALSE)
    make_unwtd_output_database(variable = paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_mov_",vc),
                                  estlabel = paste0("Experienced MOV for ",
                                                    str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
                                                    " - ", pvc, " (%)"),
                                  vid = paste0(MOV_OUTPUT_DOSE_LIST[d],"_mov_",vcf),
                                  measureid = "ES_STUD_02", keepnumerator = FALSE)
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  print("Totals...")

  make_unwtd_output_database(variable = paste0("table5_any_valid_",vc),
                                estlabel = paste0("Received 1+ valid doses - ",
                                                  pvc, " (%)"),
                                vid = paste0("any_valid_",vcf),
                                measureid = "ES_STUD_02",keepnumerator = TRUE)
  make_unwtd_output_database(variable = paste0("table5_any_mov_",vc),
                                estlabel = paste0("Experienced 1+ MOVs doses - ",
                                                  pvc, " (%)"),
                                vid = paste0("any_mov_",vcf),
                                measureid = "ES_STUD_02",keepnumerator = TRUE)
  make_count_output_database(numerator = paste0("table5_mov_count_",vc),
                             denominator = paste0("table5_elig_count_",vc),
                                estlabel = paste0("Eligible doses not administered -",
                                                  pvc, " (%)"),
                                vid = paste0("total_movs_",vcf),
                                measureid = "ES_STUD_02",keepnumerator = TRUE)

  # To create database for Table5 endcap N
  make_unwtd_output_database(variable = "table5_dv_had_card_endcap",
                                estlabel = "Had card with dob and 1+ doses",
                                vid = "had_card",
                                measureid = "ES_STUD_02",keepnumerator = TRUE)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
