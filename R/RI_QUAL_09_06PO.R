#' Make plots for RI_QUAL_09
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @import stringr

# RI_QUAL_09_06PO R version 1.03 - Biostat Global Consulting - 2023-09-12
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-10-01  1.00      Mia Yu          Original R version
# 2022-10-07  1.01      Mia Yu          Add parts that record filename and label
# 2022-10-13  1.02      Mia Yu          Package version
# 2023-09-12  1.03      Mia Yu          Update with multi-language
# *******************************************************************************

RI_QUAL_09_06PO <- function(VCP = "RI_QUAL_09_06PO"){

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

      #RI - Respondents with MOSV for `=upper("`d'")'
      vcf <- paste0(language_string(language_use = language_use, str = "OS_64"),
                          " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d]))
      title <- create_multi_lingual_plot_title(title_string = vcf)

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d])
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_plot_database.rds"),
                     title = title,
                     name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d]),
                     savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment", paste0("Unweighted proportion plot for ", MOV_OUTPUT_DOSE_LIST[d], " was created and exported."))

      vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1)

      #RI - Proportion of MOSVs for `=upper("`d'")'" "that were Later Corrected
      vcf <- paste0(language_string(language_use = language_use, str = "OS_63"),
                    " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])," ",
                    language_string(language_use = language_use, str = "OS_73"))
      title <- create_multi_lingual_plot_title(title_string = vcf)

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d],"_cor")
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_corplot_database.rds"),
                     title = title,
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

    #RI: Respondents with MOSV for Any Dose
    title <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_65"))

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_plot_database.rds"),
                   title = title,
                   name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose"),
                   savedata = savedata)

    vcqi_log_comment(VCP, 3, "Comment","Unweighted proportion plot for any dose was created and exported.")

    vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1)

    vcf <- paste0(language_string(language_use = language_use, str = "OS_62"),
                  " ",
                  language_string(language_use = language_use, str = "OS_72"))
    title <- create_multi_lingual_plot_title(title_string = vcf)

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose_cor")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_09_",ANALYSIS_COUNTER,"_anydose_corplot_database.rds"),
                   title = title,
                   name = paste0("RI_QUAL_09_",ANALYSIS_COUNTER,"_uwplot_anydose_cor"),
                   savedata = savedata)

    vcqi_log_comment(VCP, 3, "Comment", "Unweighted proportion plot for any dose corrected was created and exported.")

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

