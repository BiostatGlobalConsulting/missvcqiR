#' Make plots for ES_STUD_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# ES_STUD_02_06PO R version 1.00 - Biostat Global Consulting - 2023-09-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-09-08  1.00      Mia Yu          Original R package version
# *******************************************************************************

ES_STUD_02_06PO <- function(VCP = "ES_STUD_02_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_MAKE_UW_PLOTS == 1){
    print("Unweighted proportion plots...(2 per dose)")

    newpath <- paste0(VCQI_OUTPUT_FOLDER,"/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    vc <- str_to_lower(ES_STUD_02_VALID_OR_CRUDE)
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
      # MOV and valid doses administered on visit day : `=upper("`d'")'
      dose_vcf1 <- paste0(language_string(language_use = language_use, str = "OS_44"),
                            ": ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d]))
      # Experienced MOV for `=upper("`d'")' : `pvc'
      dose_vcf2 <- paste0(language_string(language_use = language_use, str = "OS_23"),
                        " ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),": ",pvc)
      v1 <- paste0(MOV_OUTPUT_DOSE_LIST[d],"_valid_",vcf)
      v2 <- paste0(MOV_OUTPUT_DOSE_LIST[d],"_mov_",vcf)
      title1 <- create_multi_lingual_plot_title(title_string = dose_vcf1)
      title2 <- create_multi_lingual_plot_title(title_string = dose_vcf2)

      for (i in 1:2){

        v <- get(paste0("v",i))
        dose_vcf <- get(paste0("dose_vcf",i))
        title <- get(paste0("title",i))

        if (VCQI_SAVE_UW_PLOT_DATA == 1){
          filestub <- paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_uwplot_",v)
          savedata <- paste0(newpath,"/",filestub)
        } else{
          savedata <- NA
        }

        vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_02_",ANALYSIS_COUNTER,"_",v,"_database.rds"),
                          title = title,
                          name = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_uwplot_",v),
                          savedata = savedata)

        vcqi_log_comment(VCP, 3, "Comment",
                         paste0("Unweighted proportion plot for ",dose_vcf," was created and exported."))
      } #end of i loop
    } #end of MOV_OUTPUT_DOSE_LIST d loop

    print("Totals...(3 plots)")

    #Received 1+ valid doses - `pvc'
    total_vcf1 <- paste0(language_string(language_use = language_use, str = "OS_57"),
                        ": ",pvc)
    #Experienced 1+ MOVs doses - `pvc'
    total_vcf2 <- paste0(language_string(language_use = language_use, str = "OS_21"),
                        ": ",pvc)
    #Eligible doses not administered - `pvc'
    total_vcf3 <- paste0(language_string(language_use = language_use, str = "OS_17"),
                         ": ",pvc)
    v1 <- paste0("any_valid_",vcf)
    v2 <- paste0("any_mov_",vcf)
    v3 <- paste0("total_movs_",vcf)
    title1 <- create_multi_lingual_plot_title(title_string = total_vcf1)
    title2 <- create_multi_lingual_plot_title(title_string = total_vcf2)
    title3 <- create_multi_lingual_plot_title(title_string = total_vcf3)

    for (i in 1:3){

      v <- get(paste0("v",i))
      total_vcf <- get(paste0("total_vcf",i))
      title <- get(paste0("title",i))

      if (VCQI_SAVE_UW_PLOT_DATA == 1){
        filestub <- paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_uwplot_",v)
        savedata <- paste0(newpath,"/",filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_uwplot(database = paste0(VCQI_OUTPUT_FOLDER,"/ES_STUD_02_",ANALYSIS_COUNTER,"_",v,"_database.rds"),
                        title = title,
                        name = paste0("ES_STUD_02_",ANALYSIS_COUNTER,"_uwplot_",v),
                        savedata = savedata)
    } #end of i loop


    vcqi_log_comment(VCP, 3, "Comment",
                     paste0("Unweighted proportion plot for ",total_vcf," was created and exported."))

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
