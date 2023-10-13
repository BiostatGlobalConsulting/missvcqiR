#' Generate output databases for ES_STUD_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr


# ES_STUD_01_04GO R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08 1.00      Mia Yu          Original R package version
# *******************************************************************************


ES_STUD_01_04GO <- function(VCP = "ES_STUD_01_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(ES_STUD_01_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)
  vcf <- substr(vc,1,1)

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])

    make_unwtd_output_database(variable = paste0("table4_dv_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
                                  estlabel = paste0("Received invalid dose for ",
                                                    str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
                                                    " - ", pvc, " (%)"),
                                  vid = paste0(MOV_OUTPUT_DOSE_LIST[d],"_invalid_",vcf),
                                  measureid = "ES_STUD_01", keepnumerator = FALSE)
  } #end of MOV_OUTPUT_DOSE_LIST d loop

  print("Totals...")

  make_unwtd_output_database(variable = paste0("table4_dv_not_elig_",vc),
                                estlabel = paste0("Received 1+ invalid doses - ",
                                                  pvc, " (%)"),
                                vid = paste0("any_invalid_",vcf),
                                measureid = "ES_STUD_01",keepnumerator = TRUE)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
