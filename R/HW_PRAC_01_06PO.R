#' Make plots for HW_PRAC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# HW_PRAC_01_06PO R version 1.00 - Biostat Global Consulting - 2023-08-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-23  1.00      Mia Yu          Original R package version
# *******************************************************************************

HW_PRAC_01_06PO <- function(VCP = "HW_PRAC_01_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_MAKE_UW_PLOTS == 1){
    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    print("No improper Vx practices")

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_0")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    # run program to create title breaking apart by 5 words in each line
    # title(Health Worker No Improper Vx Practices)
    title0 <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_37"))

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,"_0_database.rds"),
                   title = title0,
                   name = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_0"),
                   savedata = savedata)

    print("Improper Vx practices - Part 1")

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_1")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    # run program to create title breaking apart by 5 words in each line
    # title(Health Worker Improper Vx Practices - Part 1)
    title1 <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_33"))

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,"_1_database.rds"),
                   title = title1,
                   name = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_1"),
                   savedata = savedata)

    print("Improper Vx practices - Part 2")

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_2")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    # run program to create title breaking apart by 5 words in each line
    # title(Health Worker Improper Vx Practices - Part 2)
    title2 <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_34"))

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,"_2_database.rds"),
                   title = title2,
                   name = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_2"),
                   savedata = savedata)

    print("Improper Vx practices - Any")

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_3")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    # run program to create title breaking apart by 5 words in each line
    # title(Health Worker Improper Vx Practices - Any)
    title3 <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_31"))

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,"_3_database.rds"),
                   title = title3,
                   name = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_3"),
                   savedata = savedata)

    print("Improper Vx practices - Both")

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_4")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    # run program to create title breaking apart by 5 words in each line
    # title(Health Worker Improper Vx Practices - Both)
    title4 <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_32"))

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/HW_PRAC_01_",ANALYSIS_COUNTER,"_4_database.rds"),
                   title = title4,
                   name = paste0("HW_PRAC_01_",ANALYSIS_COUNTER,"_uwplot_4"),
                   savedata = savedata)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
