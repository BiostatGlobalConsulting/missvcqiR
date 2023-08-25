#' Make plots for RI_QUAL_09
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @import stringr

# RI_QUAL_09_06PO_MV R version 1.02 - Biostat Global Consulting - 2022-10-13
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-10-01  1.00      Mia Yu          Original R version
# 2022-10-07  1.01      Mia Yu          Add parts that record filename and label
# 2022-10-13  1.02      Mia Yu          Package version
# *******************************************************************************

RI_QUAL_09_06PO_MV <- function(VCP = "RI_QUAL_09_06PO_MV"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_MAKE_UW_PLOTS == 1){

    print("Unweighted proportion plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      print(MOV_OUTPUT_DOSE_LIST[d])

      dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_database.rds"))

      #plot_database
      saveRDS(dat,
              file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_plot_database.rds"))

      vcqi_global(VCQI_DATABASES,
                  c(VCQI_DATABASES,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_plot_database.rds")))

      dat <- dat %>% mutate(estimate = ifelse((n_eligible == 0) %in% TRUE, 0 , n_mov / n_eligible),
                            estimate_cor = ifelse((n_mov == 0) %in% TRUE, 0 , n_cor_mov / n_mov),
                            n = n_eligible)
      saveRDS(dat,
              file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_plot_database.rds"))

      if (!vcqi_object_exists("RI_QUAL_09_TEMP_DATASETS")){
        RI_QUAL_09_TEMP_DATASETS <- NULL
      }
      vcqi_global(RI_QUAL_09_TEMP_DATASETS,
                  c(RI_QUAL_09_TEMP_DATASETS,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_plot_database.rds")))

      #corplot_database
      saveRDS(dat,
              file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_corplot_database.rds"))

      vcqi_global(VCQI_DATABASES,
                  c(VCQI_DATABASES,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_corplot_database.rds")))

      dat <- dat %>% mutate(estimate = estimate_cor,
                            n = n_mov) %>% select(-c(n_eligible,n_mov))

      saveRDS(dat,
              file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_corplot_database.rds"))

      if (!vcqi_object_exists("RI_QUAL_09_TEMP_DATASETS")){
        RI_QUAL_09_TEMP_DATASETS <- NULL
      }
      vcqi_global(RI_QUAL_09_TEMP_DATASETS,
                  c(RI_QUAL_09_TEMP_DATASETS,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_corplot_database.rds")))

      vcqi_global(SORT_PLOT_LOW_TO_HIGH, 0)

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d])
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot_MV(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_plot_database.rds"),
                     title = paste0("RI - Respondents with MOV for ", str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
                     name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d]),
                     savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment", paste0("Unweighted proportion plot for ", MOV_OUTPUT_DOSE_LIST[d], " was created and exported."))

      vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1)

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d],"_cor")
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot_MV(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_corplot_database.rds"),
                     title = paste0("RI - Proportion of MOVs for ", str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," that were Later Corrected"),
                     name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d],"_cor"),
                     savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment", paste0("Unweighted proportion plot for Corrected ", MOV_OUTPUT_DOSE_LIST[d], " was created and exported."))

    } #end of d loop

    print("Totals...")

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_database.rds"))

    #plot_database
    saveRDS(dat,
            file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_plot_database.rds"))

    vcqi_global(VCQI_DATABASES,
                c(VCQI_DATABASES,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_plot_database.rds")))

    dat <- dat %>% mutate(estimate = ifelse((n_eligible == 0) %in% TRUE, 0 , n_mov / n_eligible),
                          estimate_cor = ifelse((n_mov == 0) %in% TRUE, 0 , n_cor_mov / n_mov),
                          n = n_eligible)
    saveRDS(dat,
            file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_plot_database.rds"))

    if (!vcqi_object_exists("RI_QUAL_09_TEMP_DATASETS")){
      RI_QUAL_09_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_QUAL_09_TEMP_DATASETS,
                c(RI_QUAL_09_TEMP_DATASETS,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_plot_database.rds")))

    #corplot_database
    saveRDS(dat,
            file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_corplot_database.rds"))

    vcqi_global(VCQI_DATABASES,
                c(VCQI_DATABASES,paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_corplot_database.rds")))

    dat <- dat %>% mutate(estimate = estimate_cor,
                          n = n_mov) %>% select(-c(n_eligible,n_mov))

    saveRDS(dat,
            file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_corplot_database.rds"))

    if (!vcqi_object_exists("RI_QUAL_09_TEMP_DATASETS")){
      RI_QUAL_09_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_QUAL_09_TEMP_DATASETS,
                c(RI_QUAL_09_TEMP_DATASETS,
                  paste0("RI_QUAL_09_", ANALYSIS_COUNTER, "_anydose_corplot_database.rds")))

    vcqi_global(SORT_PLOT_LOW_TO_HIGH, 0)

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    vcqi_to_uwplot_MV(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_plot_database.rds"),
                   title = "RI - Respondents with MOV for Any Dose",
                   name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose"),
                   savedata = savedata)

    vcqi_log_comment(VCP, 3, "Comment","Unweighted proportion plot for any dose was created and exported.")

    vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1)

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose_cor")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    vcqi_to_uwplot_MV(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_corplot_database.rds"),
                   title = paste0("RI - Proportion of MOVs that had All MOVs Later Corrected"),
                   name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose_cor"),
                   savedata = savedata)

    vcqi_log_comment(VCP, 3, "Comment", "Unweighted proportion plot for any dose corrected was created and exported.")

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

