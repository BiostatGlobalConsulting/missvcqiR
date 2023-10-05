#' Generate output databases for HW_PRAC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER


# HW_PRAC_01_04GO R version 1.00 - Biostat Global Consulting - 2023-08-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# *******************************************************************************


HW_PRAC_01_04GO <- function(VCP = "HW_PRAC_01_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print("No improper Vx practices")
  make_unwtd_output_database(variable = "improper_practices_none",
                             estlabel = "No Improper Vx Practices",
                             vid = 0,
                             measureid = "HW_PRAC_01",keepnumerator = TRUE)
  print("Improper Vx practices - Part 1")
  make_unwtd_output_database(variable = "improper_practices_1",
                             estlabel = "Improper Vx Practices - Part 1",
                             vid = 1,
                             measureid = "HW_PRAC_01",keepnumerator = TRUE)
  print("Improper Vx practices - Part 2")
  make_unwtd_output_database(variable = "improper_practices_2",
                             estlabel = "Improper Vx Practices - Part 2",
                             vid = 2,
                             measureid = "HW_PRAC_01",keepnumerator = TRUE)
  print("Improper Vx practices - Any")
  make_unwtd_output_database(variable = "improper_practices_any",
                             estlabel = "Any Improper Vx Practices",
                             vid = 3,
                             measureid = "HW_PRAC_01",keepnumerator = TRUE)
  print("Improper Vx practices - Both")
  make_unwtd_output_database(variable = "improper_practices_both",
                             estlabel = "Both Improper Vx Practices",
                             vid = 4,
                             measureid = "HW_PRAC_01",keepnumerator = TRUE)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
