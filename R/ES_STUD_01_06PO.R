#' Make plots for ES_STUD_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# ES_STUD_01_06PO R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_01_06PO <- function(VCP = "ES_STUD_01_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_MAKE_UW_PLOTS == 1){
    print("Unweighted proportion plots...(1 per dose)")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    vc <- str_to_lower(ES_STUD_01_VALID_OR_CRUDE)
    if (vc == "crude"){
      pvc <- language_string(language_use = language_use, str = "OS_14") #Crude
    } else {
      pvc <- language_string(language_use = language_use, str = "OS_80") #Valid
    }
    vcf <- substr(vc,1,1)

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      print(MOV_OUTPUT_DOSE_LIST[d])

      # Create locals for titles
      # Table 4
      # Invalid doses administered on visit day - `=upper("`d'")'
      invalid_vcf <- paste0(language_string(language_use = language_use, str = "OS_84"),
                            ": ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d]))
      v <- paste0(MOV_OUTPUT_DOSE_LIST[d],"_invalid_",vcf)
      title <- create_multi_lingual_plot_title(title_string = invalid_vcf)

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_uwplot_",v)
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot_MV(database = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_01_",ANALYSIS_COUNTER,"_",v,"_database.rds"),
                        title = title,
                        name = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_uwplot_",v),
                        savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment",
                       paste0("Unweighted proportion plot for ",invalid_vcf," was created and exported."))

    } #end of MOV_OUTPUT_DOSE_LIST d loop

    print("Totals...(5 plots)")
    v <- paste0("any_invalid_",vcf)
    # run program to create title breaking apart by 5 words in each line

    invalid_vcf <- paste0(language_string(language_use = language_use, str = "OS_56"),
                          ": ",pvc)
    title <- create_multi_lingual_plot_title(title_string = invalid_vcf)

    if (VCQI_SAVE_UW_PLOT_DATA == 1){
      filestub <- paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_uwplot_",v)
      savedata <- paste0(newpath,"/",filestub)
    } else{
      savedata <- NA
    }

    vcqi_to_uwplot_MV(database = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_01_",ANALYSIS_COUNTER,"_",v,"_database.rds"),
                      title = title,
                      name = paste0("ES_STUD_01_",ANALYSIS_COUNTER,"_uwplot_",v),
                      savedata = savedata)

    vcqi_log_comment(VCP, 3, "Comment",
                     paste0("Unweighted proportion plot for ",invalid_vcf," was created and exported."))

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
