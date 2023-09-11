#' This program runs runs RI_COVG_01, 02, 03 and 04 without exporting
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#'
#' @return a dataset

# RI_COVG_01_02_03_04 R version 1.00 - Biostat Global Consulting - 2023-09-07
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-07  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_COVG_01_02_03_04 <- function(VCP = "RI_COVG_01_02_03_04"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Run for RI_COVG_01
  oldvcp <- VCP
  VCP <- "RI_COVG_01"
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ",VCP," ..."))

  if (VCQI_PREPROCESS_DATA == 1){
    print("Pre-processing dataset")
    RI_COVG_01_01PP()
    print("Checking data quality")
    RI_COVG_01_02DQ()
  }
  if (VCQI_GENERATE_DVS == 1){
    print("Calculating derived variables")
    RI_COVG_01_03DV()
  }

  # Run for RI_COVG_02
  VCP <- "RI_COVG_02"
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ",VCP," ..."))

  if (VCQI_PREPROCESS_DATA == 1){
    print("Pre-processing dataset")
    RI_COVG_02_01PP()
  }
  if (VCQI_GENERATE_DVS == 1){
    print("Calculating derived variables")
    RI_COVG_02_03DV()
  }

  # Run for RI_COVG_03
  VCP <- "RI_COVG_03"
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ",VCP," ..."))
  print("Checking global macros")
  RI_COVG_03_00GC()
  if (VCQI_PREPROCESS_DATA == 1){
    print("Pre-processing dataset")
    RI_COVG_03_01PP()
  }
  if (VCQI_GENERATE_DVS == 1){
    print("Calculating derived variables")
    RI_COVG_03_03DV()
  }

  # Run for RI_COVG_04
  VCP <- "RI_COVG_04"
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ",VCP," ..."))
  # The global checks for RI_COVG_03 and _04 are the same, so this program
  if (VCQI_PREPROCESS_DATA == 1){
    print("Pre-processing dataset")
    RI_COVG_04_01PP()
  }
  if (VCQI_GENERATE_DVS == 1){
    print("Calculating derived variables")
    RI_COVG_04_03DV()
  }

  VCP <- VCP
  vcqi_log_comment(VCP, 5, "Flow", "Starting")
}
