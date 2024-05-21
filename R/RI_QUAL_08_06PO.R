#' Make plots for RI_QUAL_08
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_08_06PO R version 1.02 - Biostat Global Consulting - 2024-05-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Mia Yu          Original R version
# 2023-09-12  1.01      Mia Yu          Update to multi-language for MISS VCQI
# 2024-05-21	1.02	    Mia Yu      		Added multi lignual globals
#										                    Added call to split_text for title
# *******************************************************************************

RI_QUAL_08_06PO <- function(VCP = "RI_QUAL_08_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_MAKE_UW_PLOTS == 1){
    print("Unweighted proportion plots")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      print(MOV_OUTPUT_DOSE_LIST[d])

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("RI_QUAL_08_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d])
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcf <- paste0(language_string(language_use = language_use, str = "OS_66"),
                          " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])) #RI - Visits with MOSV for `=upper("`d'")'
      title_string <- split_text(text_string = vcf, text_cutoff = TITLE_CUTOFF)
      #title <- create_multi_lingual_plot_title(title_string = vcf)

      vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_08_",ANALYSIS_COUNTER,"_",MOV_OUTPUT_DOSE_LIST[d],"_database.rds"),
                     title = title_string,
                     name = paste0("RI_QUAL_08_",ANALYSIS_COUNTER,"_uwplot_",MOV_OUTPUT_DOSE_LIST[d]),
                     savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment",
                       paste0("Unweighted proportion plot for ",MOV_OUTPUT_DOSE_LIST[d]," was created and exported."))
    } #end of d loop

    print("Totals...")

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("RI_QUAL_08_",ANALYSIS_COUNTER,"_uwplot_any")
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    #RI - Visits with MOSV for Any Dose
    title_string <- split_text(text_string = language_string(language_use = language_use, str = "OS_67"),
                               text_cutoff = TITLE_CUTOFF)
    #title <- create_multi_lingual_plot_title(title_string = language_string(language_use = language_use, str = "OS_67"))

    vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_08_",ANALYSIS_COUNTER,"_any_database.rds"),
                   title = title_string,
                   name = paste0("RI_QUAL_08_",ANALYSIS_COUNTER,"_uwplot_any"),
                   savedata = savedata) #title = "RI - Visits with MOV for Any Dose"

    vcqi_log_comment(VCP, 3, "Comment",
                     "Unweighted proportion plot for any dose was created and exported.")
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

