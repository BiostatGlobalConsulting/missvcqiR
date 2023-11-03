#' This program pulls in the Health Worker (HW) specified data and runs assertlist checks to identify potential errors prior to running the program
#'
#' @param ... Other arguments
#'
#' @return An Excel file that documents the results of assertlist checks
#' @export
#'
#' @rawNamespace import(tools, except = makevars_user)
#'
#' @examples
#' dq_health_worker()

# dq_health_worker R version 1.00 - Biostat Global Consulting - 2023-10-30
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-10-30  1.00      Mia Yu          Original R package version
# *******************************************************************************

dq_health_worker <- function(...){

  rm(assertlist_output, envir = .GlobalEnv) %>% suppressWarnings()
  rm(assertlist_summary, envir = .GlobalEnv) %>% suppressWarnings()

  dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",HW_SURVEY_DATASET))

  idvars <- c("HW01AA", "ID02AB", "ID02AD", "ID02AF", "ID02AH", "ID02AIid")

  # Check Questionnaire number, Facility svc type, facility number
  assertlist(dat = dat, var = c("HW01AA"), f = "!is.na(dat$HW01AA)",
             tag = "Missing HW01AA - Questionnaire number",idlist = idvars,checklist = c("HW01AA"),fix = TRUE)

  # Confirm questionnaire number is unique
  dat2 <- dat %>% group_by(HW01AA) %>% mutate(HW01AA_check = row_number()) %>% ungroup()

  assertlist(dat = dat2, var = c("HW01AA"), f = "dat$HW01AA_check %in% 1",
             tag = "HW01AA - Questionnaire number should be unique",idlist = idvars,checklist = c("HW01AA"),fix = TRUE)


  # Check Facility svc type, facility number
  assertlist(dat = dat, var = c("HW01AB"), f = "dat$HW01AB %in% c(1,2,3,4)",
             tag = "Missing HW01AB - Type of Service",idlist = idvars,checklist = c("HW01AB"),fix = TRUE)

  assertlist(dat = dat, var = c("HW01AB_4OS"), f = "is.na(dat$HW01AB_4OS) | dat$HW01AB_4OS == ''",
             condition = "dat$HW01AB %in% c(1,2,3)",
             tag = "Should be missing HW01AB_4OS as 4 was not selected for HW01AB",
             idlist = idvars,checklist = c("HW01AB_4OS", "HW01AB"),fix = TRUE)
  assertlist(dat = dat, var = c("HW01AB_4OS"), f = "!is.na(dat$HW01AB_4OS) & dat$HW01AB_4OS != ''",
             condition = "dat$HW01AB %in% 4",
             tag = "HW01AB_4OS should be populated as 4 was selected for HW01AB",
             idlist = idvars,checklist = c("HW01AB_4OS", "HW01AB"),fix = TRUE)

  assertlist(dat = dat, var = c("HW01AC"), f = "dat$HW01AC %in% c(1,2)",
             tag = "Missing Health Facility type",idlist = idvars,checklist = c("HW01AC"),fix = TRUE)

  # Check ID variables
  idcheck <- c("ID02AA", "ID02AB", "ID02AC", "ID02AD", "ID02AE",
               "ID02AF", "ID02AG", "ID02AH", "ID02AIname", "ID02AIid")
  for (v in seq_along(idcheck)){
    vartype = get(idcheck[v],dat)
    if(any(class(vartype) == "numeric")){
      assertlist(dat = dat, var = idcheck[v], f = paste0("!is.na(dat$",idcheck[v],")"),
                 tag = paste0(idcheck[v], " cannot be missing as required variable"),
                 idlist = idvars,checklist = idcheck[v],fix = TRUE)
    } else if (any(class(vartype) == "character")){
      assertlist(dat = dat, var = idcheck[v], f = paste0("!is.na(dat$",idcheck[v],") & dat$",idcheck[v]," != ''"),
                 tag = paste0(idcheck[v], " cannot be missing as required variable"),
                 idlist = idvars,checklist = idcheck[v],fix = TRUE)
    }

  } #end of idcheck v loop

  numvar <- c("ID02AB", "ID02AD", "ID02AF", "ID02AH", "ID02AIid")
  for (v in seq_along(numvar)){
    assertlist(dat = dat, var = numvar[v], f = paste0("is.numeric(dat$",numvar[v],")"),
               tag = paste0("Variable type for ",numvar[v], " cannot be string"),
               idlist = idvars,checklist = numvar[v],fix = TRUE)
  } #end of numvar v loop

  assertlist(dat = dat, var = c("ID02AJm"), f = "dat$ID02AJm %in% c(1:12)",
             tag = "Interview Month invalid",idlist = idvars,checklist = c("ID02AJm"),fix = TRUE)

  assertlist(dat = dat, var = c("ID02AJd"), f = "dat$ID02AJd <= 28",
             condition = "dat$ID02AJm %in% 2",
             tag = "Interview Day Invalid",
             idlist = idvars,checklist = c("ID02AJm", "ID02AJd"),fix = TRUE)
  assertlist(dat = dat, var = c("ID02AJd"), f = "dat$ID02AJd <= 31",
             condition = "dat$ID02AJm %in% c(1,3,5,7,8,10,12)",
             tag = "Interview Day Invalid",
             idlist = idvars,checklist = c("ID02AJm", "ID02AJd"),fix = TRUE)
  assertlist(dat = dat, var = c("ID02AJd"), f = "dat$ID02AJd <= 30",
             condition = "dat$ID02AJm %in% c(2,4,6,9,11)",
             tag = "Interview Day Invalid",
             idlist = idvars,checklist = c("ID02AJm", "ID02AJd"),fix = TRUE)

  assertlist(dat = dat, var = c("ID02AJy"), f = "!is.na(dat$ID02AJy)",
             tag = "Interview Year Invalid",idlist = idvars,checklist = c("ID02AJy"),fix = TRUE)

  # Check HW survey specific variables
  assertlist(dat = dat, var = c("HW03AA"), f = "dat$HW03AA %in% c(1,2)",
             tag = "HW03AA - Sex invalid",idlist = idvars,checklist = c("HW03AA"),fix = TRUE)
  assertlist(dat = dat, var = c("HW03AB"), f = "!is.na(dat$HW03AB)",
             tag = "Missing Age in years",idlist = idvars,checklist = c("HW03AB"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AC"), f = "dat$HW03AC %in% c(1:5) | is.na(dat$HW03AC)",
             tag = "HW03AC - Invalid Professional Training",idlist = idvars,checklist = c("HW03AC"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AC_5OS"), f = "!is.na(dat$HW03AC_5OS) & dat$HW03AC_5OS != ''",
             condition = "dat$HW03AC %in% 5",
             tag = "HW03AC_05OS should be populated as HW03AC was set to 5",
             idlist = idvars,checklist = c("HW03AC_5OS", "HW03AC"),fix = TRUE)
  assertlist(dat = dat, var = c("HW03AC_5OS"), f = "is.na(dat$HW03AC_5OS) | dat$HW03AC_5OS == ''",
             condition = "dat$HW03AC %in% c(1:4)",
             tag = "HW03AC_05OS should be missing as HW03AC was not set to 5",
             idlist = idvars,checklist = c("HW03AC_5OS", "HW03AC"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AD_1"), f = "dat$HW03AD_1 %in% c(1,2) | is.na(dat$HW03AD_1)",
             tag = "HW03AD_1 - Invalid response",idlist = idvars,checklist = c("HW03AD_1"),fix = TRUE)
  assertlist(dat = dat, var = c("HW03AE_1"), f = "is.numeric(dat$HW03AE_1)",
             tag = "HW03AE_1 must be numeric if not missing",idlist = idvars,checklist = c("HW03AE_1"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AD_2"), f = "dat$HW03AD_2 %in% c(1,2) | is.na(dat$HW03AD_2)",
             tag = "HW03AD_2 - Invalid response",idlist = idvars,checklist = c("HW03AD_2"),fix = TRUE)
  assertlist(dat = dat, var = c("HW03AE_2"), f = "is.numeric(dat$HW03AE_2)",
             tag = "HW03AE_2 must be numeric if not missing",idlist = idvars,checklist = c("HW03AE_2"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AE_2"), f = "dat$HW03AE_2 <= 11",
             condition = "!is.na(dat$HW03AE_2)",
             tag = "HW03AE_2 should be between 1 and 11",
             idlist = idvars,checklist = c("HW03AE_2"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AF"), f = "dat$HW03AF %in% c(1,2) | is.na(dat$HW03AF)",
             tag = "HW03AF - Invalid value",idlist = idvars,checklist = c("HW03AF"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AG"), f = "dat$HW03AG %in% c(1:4) | is.na(dat$HW03AG)",
             condition = "dat$HW03AF %in% 1",
             tag = "HW03AG - Invalid Value",
             idlist = idvars,checklist = c("HW03AG","HW03AF"),fix = TRUE)
  assertlist(dat = dat, var = c("HW03AG"), f = "is.na(dat$HW03AG)",
             condition = "dat$HW03AF %in% 2",
             tag = "HW03AG should not be populated if HW03AF is set to No",
             idlist = idvars,checklist = c("HW03AG","HW03AF"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AH"), f = "dat$HW03AH %in% c(1,2) | is.na(dat$HW03AH)",
             tag = "HW03AH - Invalid value",idlist = idvars,checklist = c("HW03AH"),fix = TRUE)

  assertlist(dat = dat, var = c("HW03AI"), f = "dat$HW03AI %in% c(1,2) | is.na(dat$HW03AI)",
             condition = "dat$HW03AH %in% 1",
             tag = "HW03AI - Invalid Value",
             idlist = idvars,checklist = c("HW03AI","HW03AH"),fix = TRUE)
  assertlist(dat = dat, var = c("HW03AI"), f = "is.na(dat$HW03AI)",
             condition = "dat$HW03AH %in% 2",
             tag = "HW03AI should be missing if HW03AH is set to No",
             idlist = idvars,checklist = c("HW03AI","HW03AH"),fix = TRUE)

  varlist <- c("AA", "AB", "AC", "AD")
  for (v in seq_along(varlist)){
    for (i in 1:5){
      assertlist(dat = dat, var = paste0("HW04",varlist[v],"_",i),
                 f = paste0("dat$HW04",varlist[v],"_",i," %in% c(1,2) | is.na(dat$HW04",varlist[v],"_",i,")"),
                 tag = paste0("HW04",varlist[v],"_",i," - Invalid Value"),
                 idlist = idvars,checklist = paste0("HW04",varlist[v],"_",i),fix = TRUE)
    } #end of i loop
  } #end of varlist v loop

  for (i in 1:5){
    assertlist(dat = dat, var = paste0("HW04AE_",i),
               f = paste0("dat$HW04AE_",i," %in% c(1:5) | is.na(dat$HW04AE_",i,")"),
               tag = paste0("HW04AE_",i," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW04AE_",i),fix = TRUE)
  } #end of i loop

  varlist <- c("AF", "AG", "AH")
  for (v in seq_along(varlist)){
    assertlist(dat = dat, var = paste0("HW04",varlist[v]),
               f = paste0("dat$HW04",varlist[v]," %in% c(1:5) | is.na(dat$HW04",varlist[v],")"),
               tag = paste0("HW04",varlist[v]," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW04",varlist[v]),fix = TRUE)
  }#end of varlist v loop

  assertlist(dat = dat, var = c("HW04AI_1"), f = "dat$HW04AI_1 %in% c(1,2) | is.na(dat$HW04AI_1)",
             tag = "HW04AI_1 - Invalid value",idlist = idvars,checklist = c("HW04AI_1"),fix = TRUE)

  assertlist(dat = dat, var = c("HW04AI_2"), f = "!is.na(dat$HW04AI_2) & dat$HW04AI_2 != ''",
             condition = "dat$HW04AI_1 %in% 2",
             tag = "HW04AI_2 should be populated if HW04AI_1 was set to No",
             idlist = idvars,checklist = c("HW04AI_2","HW04AI_1"),fix = TRUE)
  assertlist(dat = dat, var = c("HW04AI_2"), f = "is.na(dat$HW04AI_2) | dat$HW04AI_2 == ''",
             condition = "!(dat$HW04AI_1 %in% 2)",
             tag = "HW04AI_2 should be missing if HW04AI_1 was not set to No",
             idlist = idvars,checklist = c("HW04AI_2","HW04AI_1"),fix = TRUE)

  for (i in 1:7){
    assertlist(dat = dat, var = paste0("HW04AJ_",i),
               f = paste0("dat$HW04AJ_",i," %in% c(1,2) | is.na(dat$HW04AJ_",i,")"),
               tag = paste0("HW04AJ_",i," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW04AJ_",i),fix = TRUE)
  } #end of i loop

  varlist <- c("AA", "AB")
  for (v in seq_along(varlist)){
    assertlist(dat = dat, var = paste0("HW05",varlist[v]),
               f = paste0("dat$HW05",varlist[v]," %in% c(1:5)"),
               tag = paste0("HW05",varlist[v]," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW05",varlist[v]),fix = TRUE)
  }#end of varlist v loop

  assertlist(dat = dat, var = c("HW05AC"), f = "dat$HW05AC %in% c(1:7) | is.na(dat$HW05AC)",
             tag = "HW05AC - Invalid value",idlist = idvars,checklist = c("HW05AC"),fix = TRUE)

  assertlist(dat = dat, var = c("HW05AD"), f = "dat$HW05AD %in% c(1,2,99) | is.na(dat$HW05AD)",
             tag = "HW05AD - Invalid value",idlist = idvars,checklist = c("HW05AD"),fix = TRUE)

  assertlist(dat = dat, var = c("HW05AE"), f = "is.na(dat$HW05AE) | dat$HW05AE == ''",
             condition = "dat$HW05AD %in% 99",
             tag = "HW05AE should not be populated if 'Don't Know' was selected in HW05AD",
             idlist = idvars,checklist = c("HW05AE","HW05AD"),fix = TRUE)
  assertlist(dat = dat, var = c("HW05AE"), f = "!is.na(dat$HW05AE) & dat$HW05AE != ''",
             condition = "!(dat$HW05AD %in% 99)",
             tag = "HW05AE should be populated",
             idlist = idvars,checklist = c("HW05AE","HW05AD"),fix = TRUE)

  varlist <- c("AF", "AG", "AH", "AI")
  for (v in seq_along(varlist)){
    assertlist(dat = dat, var = paste0("HW05",varlist[v]),
               f = paste0("dat$HW05",varlist[v]," %in% c(1,2) | is.na(dat$HW05",varlist[v],")"),
               tag = paste0("HW05",varlist[v]," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW05",varlist[v]),fix = TRUE)
  }#end of varlist v loop

  varlist <- c("AA", "AB", "AC")
  for (v in seq_along(varlist)){
    assertlist(dat = dat, var = paste0("HW06",varlist[v]),
               f = paste0("dat$HW06",varlist[v]," %in% c(1:5) | is.na(dat$HW06",varlist[v],")"),
               tag = paste0("HW06",varlist[v]," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW06",varlist[v]),fix = TRUE)
  }#end of varlist v loop

  for (i in 1:6){
    assertlist(dat = dat, var = paste0("HW06AD_",i),
               f = paste0("dat$HW06AD_",i," %in% c(1,2) | is.na(dat$HW06AD_",i,")"),
               tag = paste0("HW06AD_",i," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW06AD_",i),fix = TRUE)
  } #end of i loop

  varlist <- c("AE", "AF", "AG", "AH", "AI")
  for (v in seq_along(varlist)){
    assertlist(dat = dat, var = paste0("HW06",varlist[v]),
               f = paste0("dat$HW06",varlist[v]," %in% c(1:5) | is.na(dat$HW06",varlist[v],")"),
               tag = paste0("HW06",varlist[v]," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW06",varlist[v]),fix = TRUE)
  }#end of varlist v loop

  assertlist(dat = dat, var = c("HW06AJ"), f = "dat$HW06AJ %in% c(1:7) | is.na(dat$HW06AJ)",
             tag = "HW06AJ - Invalid value",idlist = idvars,checklist = c("HW06AJ"),fix = TRUE)

  for (i in 1:4){
    assertlist(dat = dat, var = paste0("HW06AK_",i),
               f = paste0("dat$HW06AK_",i," %in% c(1:3)"),
               tag = paste0("HW06AK_",i," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW06AK_",i),fix = TRUE)
  } #end of i loop

  varlist <- c("AL", "AM", "AN", "AO")
  for (v in seq_along(varlist)){
    assertlist(dat = dat, var = paste0("HW06",varlist[v]),
               f = paste0("dat$HW06",varlist[v]," %in% c(1,2) | is.na(dat$HW06",varlist[v],")"),
               tag = paste0("HW06",varlist[v]," - Invalid Value"),
               idlist = idvars,checklist = paste0("HW06",varlist[v]),fix = TRUE)
  }#end of varlist v loop

  asserlist_cleanup(summary_dataframe = "assertlist_summary", output_dataframe = "assertlist_output",
                   filename = paste0(VCQI_OUTPUT_FOLDER,"/",
                                     tools::file_path_sans_ext(HW_SURVEY_DATASET),"_assertlist_checks.xlsx"),
                   sheetname = "assertlist_checks",fix = TRUE)

  rm(assertlist_output, envir = .GlobalEnv) %>% suppressWarnings()
  rm(assertlist_summary, envir = .GlobalEnv) %>% suppressWarnings()
}
