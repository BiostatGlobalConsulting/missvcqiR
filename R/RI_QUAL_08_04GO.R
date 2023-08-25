#' Generate output databases for RI_QUAL_08
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_08_04GO R version 1.02 - Biostat Global Consulting - 2023-07-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Mia Yu          Original R version
# 2022-10-12  1.01      Mia Yu          Package version
# 2023-07-21  1.02      Caitlin Clary   Update labels (MOV -> MOSV)
# *******************************************************************************

RI_QUAL_08_04GO <- function(VCP = "RI_QUAL_08_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vc <- str_to_lower(RI_QUAL_08_VALID_OR_CRUDE)
  pvc <- str_to_title(vc)

  for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])

    make_count_output_database(numerator = paste0("total_mov_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
                               denominator = paste0("total_elig_",MOV_OUTPUT_DOSE_LIST[d],"_",vc),
                               estlabel = paste0("Visits with MOSV for ", str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," (", pvc,")(%)"),
                               vid = MOV_OUTPUT_DOSE_LIST[d],
                               measureid = "RI_QUAL_08")
  } #end of d loop

  print("Totals...")

  make_count_output_database(numerator = paste0("total_mov_visits_",vc),
                             denominator = paste0("total_elig_visits_",vc),
                             estlabel = paste0("Visits with MOSV for any dose "," (", pvc,")(%)"),
                             vid = "any",
                             measureid = "RI_QUAL_08")

  make_count_output_database(numerator = paste0("total_movs_",vc),
                             denominator = paste0("total_elig_visits_",vc),
                             estlabel = paste0("Rate of MOSVs per eligible visit "," (", pvc,")(%)"),
                             vid = "rate",
                             measureid = "RI_QUAL_08")

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

