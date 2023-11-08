#' This program checks the data to make sure everything aligns with the expected value specified in the protocol document
#'
#' @param ... Other arguments
#'
#' @return An Excel file that documents the results of assertlist checks
#' @export
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#'
#' @examples
#' dq_exit_survey()

# dq_exit_survey R version 1.00 - Biostat Global Consulting - 2023-11-03
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-11-03  1.00      Mia Yu          Original R package version
# *******************************************************************************

dq_exit_survey <- function(...){

  rm(assertlist_output, envir = .GlobalEnv) %>% suppressWarnings()
  rm(assertlist_summary, envir = .GlobalEnv) %>% suppressWarnings()

  dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",ES_SURVEY_DATASET))

  # Confirm that the globals used in this program are set
  if (!vcqi_object_exists("RI_DOSE_LIST")){
    warning("Since no dose list global has been populated, no dose variables will be checked")
  }

  if (!vcqi_object_exists("LATEST_SVY_VACC_DATE_Y")){
    warning(paste0("Since global LATEST_SVY_VACC_DATE_Y is not populated all assertlist checks ",
                   "looking at the year (doses and survey dates) will not be completed"))
  }


  # Make sure Questionnaire is not missing and unique

  # Check Questionnaire number, Facility svc type, facility number
  assertlist(dat = dat, var = c("ES01AA"), f = "!is.na(dat$ES01AA)",
             tag = "Questionnaire number is missing",idlist = "ES01AA",checklist = c("ES01AA"),fix = TRUE)

  # Confirm questionnaire number is unique
  dat2 <- dat %>% group_by(ES01AA) %>% mutate(n = row_number()) %>% ungroup()

  assertlist(dat = dat2, var = c("ES01AA"), f = "dat$n %in% 1",
             tag = "Questionnaire number is not unique",idlist = "ES01AA",
             checklist = c("ES01AA"),fix = TRUE)

  # Confirm the following identifying variables are numeric
  numvar <- c("ES01AA", "ES01AB", "ES01AE", "ID02AB", "ID02AD", "ID02AF", "ID02AH", "ID02AIid")
  for (v in seq_along(numvar)){
    assertlist(dat = dat, var = numvar[v], f = paste0("!('character' %in% class(dat$",numvar[v],"))"),
               tag = paste0("Variable type for ",numvar[v], " cannot be string"),
               idlist = "ES01AA",checklist = numvar[v],fix = TRUE)
  } #end of numvar v loop

  # Confirm all other identifying variables are populated and take on appropriate values
  assertlist(dat = dat, var = c("ES01AB"), f = "dat$ES01AB %in% c(1:4)",
             tag = "ES01AB invalid value",idlist = "ES01AA",
             checklist = c("ES01AB"),fix = TRUE)

  assertlist(dat = dat, var = c("ES01AE"), f = "dat$ES01AE %in% c(1:8)",
             condition = "dat$ES01AD %in% 1",
             tag = "ES01AE - Invalid Value",
             idlist = "ES01AA",checklist = c("ES01AE", "ES01AD"),fix = TRUE)

  varlist <- c("AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AIname", "AIid")
  for (v in seq_along(varlist)){
    vartype = get(paste0("ID02",varlist[v]),dat)
    if(any("numeric" %in% class(vartype)) | any("double" %in% class(vartype))){
      assertlist(dat = dat, var = paste0("ID02",varlist[v]), f = paste0("!is.na(dat$ID02",varlist[v],")"),
                 tag = paste0("ID02",varlist[v], " cannot be missing"),
                 idlist = "ES01AA",checklist = paste0("ID02",varlist[v]),fix = TRUE)
    } else if (any("character" %in% class(vartype))){
      assertlist(dat = dat, var = paste0("ID02",varlist[v]),
                 f = paste0("!is.na(dat$ID02",varlist[v],") & dat$ID02",varlist[v]," != ''"),
                 tag = paste0("ID02",varlist[v], " cannot be missing"),
                 idlist = "ES01AA",checklist = paste0("ID02",varlist[v]),fix = TRUE)
    }

  } #end of varlist v loop

  # Check that all non identifying vars are missing if not eligible or survey not completed.

  for (v in seq_along(names(dat))){
    # Check that all non identifying vars are missing if child was not eligible for survey
    # or did not complete it

    # Check that all non identifying vars are missing if survey not completed
    idvars <- c("ES01AA", "ES01AB","ES01AB_4OS", "ES01AC","ES01AE","ES01AE_4OS",
                "ID02AA", "ID02AB", "ID02AC", "ID02AD", "ID02AE", "ID02AF", "ID02AG",
                "ID02AH", "ID02AIid","ID02AIname", "ID02AJ",
                "ID02AK", "ID02AL", "ID02AM", "ID02AN", "ID02AO", "ID02AP", "ID02AQ", "ID11AA", "ID11AB",
                "ID11AC","ID11AD","ID11AE")

    if (!names(dat)[v] %in% idvars){
      vartype = get(names(dat)[v],dat)
      if(any("numeric" %in% class(vartype)) | any("double" %in% class(vartype))){
        assertlist(dat = dat, var = names(dat)[v], f = paste0("is.na(dat$",names(dat)[v],")"),
                   condition = "!(dat$ES01AE %in% 1)",
                   tag = paste0(names(dat)[v], " should not be populated since ES01AE indicates survey was not completed."),
                   idlist = "ES01AA",checklist = c("ES01AE",names(dat)[v]),fix = TRUE)
      } else if (any("character" %in% class(vartype))){
        assertlist(dat = dat, var = names(dat)[v],
                   f = paste0("is.na(dat$",names(dat)[v],") | dat$",names(dat)[v]," == ''"),
                   condition = "!(dat$ES01AE %in% 1)",
                   tag = paste0(names(dat)[v], " should not be populated since ES01AE indicates survey was not completed."),
                   idlist = "ES01AA",checklist = c("ES01AE",names(dat)[v]),fix = TRUE)
      }
    }
  } #end of v loop

  # Next check each date variable to ensure it is a valid date
  if(!vcqi_object_exists("RI_DOSE_LIST")){
    RI_DOSE_LIST_MINUS_VISIT <- NULL
  } else{
    RI_DOSE_LIST_MINUS_VISIT <- RI_DOSE_LIST[-which(RI_DOSE_LIST == "visit")]
  }

  prefix <- c(RI_DOSE_LIST_MINUS_VISIT,"ES03AA", "ID02AJ")
  for (p in seq_along(prefix)){
    v <- paste0(prefix[p],"_date_card_")
    c <- "ES06AA"

    # The nondose date variables will have different locals

    if (prefix[p] %in% c("ES03AA", "ID02AJ")){
      v <- prefix[p]
      c <- "ES01AE"
    }

    dat3 <- dat %>% mutate(tempvar = 0)

    suffix <- c("m","d","y")
    for (m in seq_along(suffix)){
      date <- rlang::sym(paste0(v,suffix[m]))
      dat3 <- dat3 %>% mutate(tempvar = ifelse(!is.na(!!date),tempvar + 1, tempvar))
    }

    names(dat3)[which(names(dat3) == "tempvar")] <- paste0(prefix[p],"_count")

    # If 1 or 2 date components, send for review
    assertlist(dat = dat3, var = paste0(prefix[p],"_count"), f = paste0("dat$",prefix[p],"_count %in% c(0,3)"),
               tag = paste0(v," has partial date components"),
               idlist = "ES01AA",
               checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y")),fix = TRUE)

    # Since the month can be missing for all dates except the Interview date
    # will need to do two separate assertlist codes depending on the variable

    if (prefix[p] != "ID02AJ"){
      assertlist(dat = dat, var = paste0(v,"m"), f = paste0("dat$",v,"m %in% c(1:12) | is.na(dat$",v,"m)"),
                 condition = paste0("dat$",c," %in% 1"),
                 tag = paste0(prefix[p]," Invalid Month"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
    }
    if (prefix[p] == "ID02AJ"){
      assertlist(dat = dat, var = paste0(v,"m"), f = paste0("dat$",v,"m %in% c(1:12)"),
                 condition = paste0("dat$",c," %in% 1"),
                 tag = "ID02AJ - Survey Month Invalid",
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
    }

    if (prefix[p] != "ID02AJ"){
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=31) %in% TRUE | is.na(dat$",v,"d)"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% c(1,3,5,7,8,10,12)"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=30) %in% TRUE | is.na(dat$",v,"d)"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% c(4,6,9,11)"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=28) %in% TRUE | is.na(dat$",v,"d)"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% 2 & !(dat$",
                                    v,"y %in% c(2012,2016,2020,2024,2028,2032,2036,2040,2044,2048))"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=29) %in% TRUE | is.na(dat$",v,"d)"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% 2 & dat$",
                                    v,"y %in% c(2012,2016,2020,2024,2028,2032,2036,2040,2044,2048)"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)

      # Capture invalid days that do not have a month date component
      # but have day so they can be reviewed if not within 1 to 31

      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=31) %in% TRUE | is.na(dat$",v,"d)"),
                 condition = paste0("dat$",c," %in% 1 & is.na(dat$",v,"m)"),
                 tag = paste0(prefix[p]," Invalid Day - No month present"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)

    }

    if (prefix[p] == "ID02AJ"){
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=31) %in% TRUE"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% c(1,3,5,7,8,10,12)"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=30) %in% TRUE"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% c(4,6,9,11)"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=28) %in% TRUE"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% 2 & !(dat$",
                                    v,"y %in% c(2012,2016,2020,2024,2028,2032,2036,2040,2044,2048))"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=29) %in% TRUE"),
                 condition = paste0("dat$",c," %in% 1 & dat$",v,"m %in% 2 & dat$",
                                    v,"y %in% c(2012,2016,2020,2024,2028,2032,2036,2040,2044,2048)"),
                 tag = paste0(prefix[p]," Invalid Day"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)

      # Capture invalid days that do not have a month date component
      # but have day so they can be reviewed if not within 1 to 31

      assertlist(dat = dat, var = paste0(v,"d"),
                 f = paste0("(dat$",v,"d >= 1 & dat$",v,"d <=31) %in% TRUE"),
                 condition = paste0("dat$",c," %in% 1 & is.na(dat$",v,"m)"),
                 tag = paste0(prefix[p]," Invalid Day - No month present"),
                 idlist = "ES01AA",
                 checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)

    }

    # The same for the year, it can be the latest_svy_vacc_date_y minus 5
    # if a dose date But must be the latest_svy_vacc_date_y if the interview date

    if (vcqi_object_exists("LATEST_SVY_VACC_DATE_Y")){
      if (prefix[p] != "ID02AJ"){
        assertlist(dat = dat, var = paste0(v,"y"),
                   f = paste0("dat$",v,"y %in% c((LATEST_SVY_VACC_DATE_Y-5):LATEST_SVY_VACC_DATE_Y) | is.na(dat$",v,"y)"),
                   condition = paste0("dat$",c," %in% 1"),
                   tag = paste0(prefix[p]," Year outside expected range"),
                   idlist = "ES01AA",
                   checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      }
      if (prefix[p] == "ID02AJ"){
        assertlist(dat = dat, var = paste0(v,"y"), f = paste0("dat$",v,"y %in% LATEST_SVY_VACC_DATE_Y"),
                   condition = paste0("dat$",c," %in% 1"),
                   tag = paste0(prefix[p]," Year outside expected range"),
                   idlist = "ES01AA",
                   checklist = c(paste0(v,"m"),paste0(v,"d"),paste0(v,"y"),c),fix = TRUE)
      }
    }

    if (!(prefix[p] %in% c("ES03AA", "ID02AJ"))){
      assertlist(dat = dat, var = paste0(prefix[p],"_tick_card"),
                 f = paste0("dat$",prefix[p],"_tick_card %in% c(1,2) | is.na(dat$",prefix[p],"_tick_card)"),
                 tag = paste0(prefix[p]," tick invalid value"),
                 idlist = "ES01AA",
                 checklist = c("ES06AA",paste0(prefix[p],"_tick_card")),fix = TRUE)
    }

  } #end of prefix p loop

  # Make sure age is between 0 and 5 years of age
  assertlist(dat = dat, var = c("ES03AA_1_1"), f = "dat$ES03AA_1_1 %in% c(0,1,2,3,4,5,99)",
             condition = "dat$ES01AE %in% 1",
             tag = "ES03AA_1_1 - Child's age in years is outside expected range",
             idlist = "ES01AA",checklist = c("ES03AA_1_1", "ES01AE"),fix = TRUE)

  # Child sex cannot be missing
  assertlist(dat = dat, var = c("ES03AB"), f = "dat$ES03AB %in% c(1,2)",
             condition = "dat$ES01AE %in% 1",
             tag = "Child's sex - ES03AB Invalid Value",
             idlist = "ES01AA",checklist = c("ES03AB", "ES01AE"),fix = TRUE)

  # Caregivers age cannot be missing
  # Should be in years...make sure greater than 14 and less than 99
  assertlist(dat = dat, var = c("ES04AA"), f = "!is.na(dat$ES04AA)",
             condition = "dat$ES01AE %in% 1",
             tag = "ES04AA - Missing age of Caregiver",
             idlist = "ES01AA",checklist = c("ES04AA", "ES01AE"),fix = TRUE)
  assertlist(dat = dat, var = c("ES04AA"), f = "dat$ES04AA >= 14 & dat$ES04AA <= 99",
             condition = "dat$ES01AE %in% 1",
             tag = "ES04AA - Age of Caregiver is outside expected range",
             idlist = "ES01AA",checklist = c("ES04AA", "ES01AE"),fix = TRUE)

  # Caregivers sex cannot be missing
  assertlist(dat = dat, var = c("ES04AB"), f = "dat$ES04AB %in% c(1,2)",
             condition = "dat$ES01AE %in% 1",
             tag = "ES04AB - Caregiver's sex invalid value",
             idlist = "ES01AA",checklist = c("ES04AB", "ES01AE"),fix = TRUE)

  # Caregivers schooling
  assertlist(dat = dat, var = c("ES04AG"), f = "dat$ES04AG %in% c(1:6)",
             condition = "dat$ES01AE %in% 1",
             tag = "ES04AG - Caregiver's schooling invalid value",
             idlist = "ES01AA",checklist = c("ES04AG", "ES01AE"),fix = TRUE)

  # Caregivers occupation
  assertlist(dat = dat, var = c("ES04AH"), f = "dat$ES04AH %in% c(1:5)",
             condition = "dat$ES01AE %in% 1",
             tag = "ES04AH - Caregiver's occupation invalid value",
             idlist = "ES01AA",checklist = c("ES04AH", "ES01AE"),fix = TRUE)

  # Does the child live in the same municipality as the health center
  assertlist(dat = dat, var = c("ES04AK"), f = "dat$ES04AK %in% c(1,2,99)",
             condition = "dat$ES01AE %in% 1",
             tag = "ES04AK - Invalid Value",
             idlist = "ES01AA",checklist = c("ES04AK", "ES01AE"),fix = TRUE)

  # Do you have a vaccination card
  assertlist(dat = dat, var = c("ES06AA"), f = "dat$ES06AA %in% c(1,2,3)",
             condition = "dat$ES01AE %in% 1 & dat$ES05AH %in% 1",
             tag = "ES06AA invalid value",
             idlist = "ES01AA",checklist = c("ES06AA", "ES01AE"),fix = TRUE)

  # What was the source
  # NOTE need to allow for facility records on this question
  assertlist(dat = dat, var = c("ES06ABSRC"), f = "dat$ES06ABSRC %in% c(1,2) | is.na(dat$ES06ABSRC)",
             condition = "dat$ES06AA %in% 1",
             tag = "ES06ABSRC invalid value",
             idlist = "ES01AA",checklist = c("ES06ABSRC", "ES06AA"),fix = TRUE)

  # Reasons not vaccinated
  assertlist(dat = dat, var = c("ES08AA"), f = "dat$ES08AA %in% c(1,2)",
             condition = "dat$ES01AE %in% 1 & dat$ES05AH %in% 1",
             tag = "ES08AA invalid value",
             idlist = "ES01AA",checklist = c("ES08AA", "ES01AE"),fix = TRUE)

  assertlist(dat = dat, var = c("ES08AA_1A_01"), f = "dat$ES08AA_1A_01 %in% c(1,2)",
             condition = "dat$ES08AA %in% 2",
             tag = "ES08AA_1A_01 invalid value",
             idlist = "ES01AA",checklist = c("ES08AA_1A_01", "ES08AA"),fix = TRUE)
  assertlist(dat = dat, var = c("ES08AA_1A_02"), f = "dat$ES08AA_1A_02 %in% c(1,2)",
             condition = "dat$ES08AA %in% 2",
             tag = "ES08AA_1A_02 invalid value",
             idlist = "ES01AA",checklist = c("ES08AA_1A_02", "ES08AA"),fix = TRUE)
  assertlist(dat = dat, var = c("ES08AA_1A_03"), f = "dat$ES08AA_1A_03 %in% c(1,2)",
             condition = "dat$ES08AA %in% 2",
             tag = "ES08AA_1A_03 invalid value",
             idlist = "ES01AA",checklist = c("ES08AA_1A_03", "ES08AA"),fix = TRUE)

  # Reasons not vaccinated because sick
  for (i in 1:9){
    assertlist(dat = dat, var = paste0("ES08AA_1A_03_0",i), f = paste0("dat$ES08AA_1A_03_0",i," %in% c(1,2)"),
               condition = "dat$ES08AA_1A_03 %in% 1",
               tag = paste0("ES08AA_1A_03_0",i," invalid value"),
               idlist = "ES01AA",checklist = c(paste0("ES08AA_1A_03_0",i), "ES08AA_1A_03"),fix = TRUE)

    assertlist(dat = dat, var = paste0("ES08AA_1A_03_0",i), f = paste0("is.na(dat$ES08AA_1A_03_0",i,")"),
               condition = "dat$ES08AA_1A_03 %in% 2",
               tag = paste0("ES08AA_1A_03_0",i," should not be populated if ES08AA_1A_03 == 2"),
               idlist = "ES01AA",checklist = c(paste0("ES08AA_1A_03_0",i), "ES08AA_1A_03"),fix = TRUE)
  } #end of i loop

  assertlist(dat = dat, var = c("ES08AA_1A_03_09DN"), f = "!is.na(dat$ES08AA_1A_03_09DN) & dat$ES08AA_1A_03_09DN != ''",
             condition = "dat$ES08AA_1A_03_09 %in% 1",
             tag = "ES08AA_1A_03_09DN should be populated if ES08AA_1A_03_09==1",
             idlist = "ES01AA",checklist = c("ES08AA_1A_03_09DN", "ES08AA_1A_03_09"),fix = TRUE)
  assertlist(dat = dat, var = c("ES08AA_1A_03_09DN"), f = "is.na(dat$ES08AA_1A_03_09DN)| dat$ES08AA_1A_03_09DN == ''",
             condition = "!(dat$ES08AA_1A_03_09 %in% 1)",
             tag = "ES08AA_1A_03_09DN should not be populated if ES08AA_1A_03_09!=1",
             idlist = "ES01AA",checklist = c("ES08AA_1A_03_09DN", "ES08AA_1A_03_09"),fix = TRUE)

  for (i in 10:11){
    assertlist(dat = dat, var = paste0("ES08AA_1A_03_",i), f = paste0("dat$ES08AA_1A_03_",i," %in% c(1,2)"),
               condition = "dat$ES08AA_1A_03 %in% 1",
               tag = paste0("ES08AA_1A_03_",i," invalid value"),
               idlist = "ES01AA",checklist = c(paste0("ES08AA_1A_03_",i), "ES08AA_1A_03"),fix = TRUE)

    assertlist(dat = dat, var = paste0("ES08AA_1A_03_",i), f = paste0("is.na(dat$ES08AA_1A_03_",i,")"),
               condition = "dat$ES08AA_1A_03 %in% 2",
               tag = paste0("ES08AA_1A_03_",i," should not be populated if ES08AA_1A_03 == 2"),
               idlist = "ES01AA",checklist = c(paste0("ES08AA_1A_03_",i), "ES08AA_1A_03"),fix = TRUE)
  } #end of i loop

  assertlist(dat = dat, var = c("ES08AA_1A_03_11OS"), f = "!is.na(dat$ES08AA_1A_03_11OS) & dat$ES08AA_1A_03_11OS != ''",
             condition = "dat$ES08AA_1A_03_11 %in% 1",
             tag = "ES08AA_1A_0_11OS should be populated if ES08AA_1A_03_11==1",
             idlist = "ES01AA",checklist = c("ES08AA_1A_03_11OS", "ES08AA_1A_03_11"),fix = TRUE)
  assertlist(dat = dat, var = c("ES08AA_1A_03_11OS"), f = "is.na(dat$ES08AA_1A_03_11OS)| dat$ES08AA_1A_03_11OS == ''",
             condition = "!(dat$ES08AA_1A_03_11 %in% 1)",
             tag = "ES08AA_1A_0_11OS should not be populated if ES08AA_1A_03_11!=1",
             idlist = "ES01AA",checklist = c("ES08AA_1A_03_11OS", "ES08AA_1A_03_11"),fix = TRUE)

  for (i in 1:12){

    if (i < 10){
      assertlist(dat = dat, var = paste0("ES08AA_1B_0",i), f = paste0("dat$ES08AA_1B_0",i," %in% c(1,2)"),
                 condition = "dat$ES08AA %in% 2",
                 tag = paste0("ES08AA_1B_0",i," invalid value"),
                 idlist = "ES01AA",checklist = c(paste0("ES08AA_1B_0",i), "ES08AA"),fix = TRUE)
    } else {
      assertlist(dat = dat, var = paste0("ES08AA_1B_",i), f = paste0("dat$ES08AA_1B_",i," %in% c(1,2)"),
                 condition = "dat$ES08AA %in% 2",
                 tag = paste0("ES08AA_1B_",i," invalid value"),
                 idlist = "ES01AA",checklist = c(paste0("ES08AA_1B_",i), "ES08AA"),fix = TRUE)
    }

  } #end of i loop

  assertlist(dat = dat, var = c("ES08AA_1B_12OS"), f = "!is.na(dat$ES08AA_1B_12OS) & dat$ES08AA_1B_12OS != ''",
             condition = "dat$ES08AA_1B_12 %in% 1",
             tag = "ES08AA_1B_12OS should be populated if ES08AA_1B_12==1",
             idlist = "ES01AA",checklist = c("ES08AA_1B_12OS", "ES08AA_1B_12"),fix = TRUE)
  assertlist(dat = dat, var = c("ES08AA_1B_12OS"), f = "is.na(dat$ES08AA_1B_12OS)| dat$ES08AA_1B_12OS == ''",
             condition = "!(dat$ES08AA_1B_12 %in% 1)",
             tag = "ES08AA_1B_12OS should not be populated if ES08AA_1B_12!=1",
             idlist = "ES01AA",checklist = c("ES08AA_1B_12OS", "ES08AA_1B_12"),fix = TRUE)

  for (i in 1:9){
    assertlist(dat = dat, var = paste0("ES08AA_1C_0",i), f = paste0("dat$ES08AA_1C_0",i," %in% c(1,2)"),
               condition = "dat$ES08AA %in% 2",
               tag = paste0("ES08AA_1C_0",i," invalid value"),
               idlist = "ES01AA",checklist = c(paste0("ES08AA_1C_0",i), "ES08AA"),fix = TRUE)

  } #end of i loop

  assertlist(dat = dat, var = c("ES08AA_1C_09OS"), f = "!is.na(dat$ES08AA_1C_09OS) & dat$ES08AA_1C_09OS != ''",
             condition = "dat$ES08AA_1C_09 %in% 1",
             tag = "ES08AA_1C_09OS should be populated if ES08AA_1C_09==1",
             idlist = "ES01AA",checklist = c("ES08AA_1C_09OS", "ES08AA_1C_09"),fix = TRUE)
  assertlist(dat = dat, var = c("ES08AA_1C_09OS"), f = "is.na(dat$ES08AA_1C_09OS)| dat$ES08AA_1C_09OS == ''",
             condition = "!(dat$ES08AA_1C_09 %in% 1)",
             tag = "ES08AA_1C_09OS should not be populated if ES08AA_1C_09!=1",
             idlist = "ES01AA",checklist = c("ES08AA_1C_09OS", "ES08AA_1C_09"),fix = TRUE)

  # Check some skip patterns
  # Confirm the below variables are missing if exist and ES05AH was set to 2
  # ES05AH==2 SKIP TO ES10AA

  if ("ES05AH" %in% names(dat)){
    varlist <- c("ES05AH_1", "ES05AH_2", "ES05AH_2_10OS", "ES05AI_1", "ES05AI_2", "ES05AI_3", "ES05AI_4", "ES05AI_5", "ES05AI_5OS",
                 "ES06AA", "ES06AB", "ES06ABSRC", "ES07AA", "ES07AA_6OS", "ES07AB", "ES07AC", "ES07AD", "ES07AD_2OS", "ES07AE",
                 "ES08AA", "ES08AA_1A_01", "ES08AA_1A_02", "ES08AA_1A_03", "ES08AA_1A_03_01", "ES08AA_1A_03_02", "ES08AA_1A_03_03",
                 "ES08AA_1A_03_04","ES08AA_1A_03_05", "ES08AA_1A_03_06", "ES08AA_1A_03_07", "ES08AA_1A_03_08", "ES08AA_1A_03_09",
                 "ES08AA_1A_03_09DN", "ES08AA_1A_03_10" ,"ES08AA_1A_03_11", "ES08AA_1A_03_11OS",
                 "ES08AA_1B_01", "ES08AA_1B_02", "ES08AA_1B_03", "ES08AA_1B_04", "ES08AA_1B_05", "ES08AA_1B_06",
                 "ES08AA_1B_07", "ES08AA_1B_08", "ES08AA_1B_09", "ES08AA_1B_10", "ES08AA_1B_11", "ES08AA_1B_12",
                 "ES08AA_1B_12OS", "ES08AA_1C_01", "ES08AA_1C_02", "ES08AA_1C_03", "ES08AA_1C_04", "ES08AA_1C_05",
                 "ES08AA_1C_06", "ES08AA_1C_07", "ES08AA_1C_08", "ES08AA_1C_09", "ES08AA_1C_09OS", "ES08AB_1",
                 "ES08AB_2", "ES09AA", "ES09AB", "ES09AC", "ES09AD", "ES09AE", "ES09AF", "ES09AG", "ES09AH", "ES09AH_4OS", "ES09AI",
                 "ES09AI_5OS", "ES09AJ", "ES09AK")

    for (v in seq_along(varlist)){
      if (varlist[v] %in% names(dat)){
        vartype = get(varlist[v],dat)
        if(any("numeric" %in% class(vartype)) | any("double" %in% class(vartype))){
          assertlist(dat = dat, var = varlist[v], f = paste0("is.na(dat$",varlist[v],")"),
                     condition = "dat$ES05AH %in% 2",
                     tag = paste0(varlist[v], " should be missing since ES05AH was set to 2 - Never vaccinated child"),
                     idlist = "ES01AA",checklist = c("ES05AH",varlist[v]),fix = TRUE)
        } else if (any("character" %in% class(vartype))){
          assertlist(dat = dat, var = varlist[v],
                     f = paste0("is.na(dat$",varlist[v],") | dat$",varlist[v]," == ''"),
                     condition = "dat$ES05AH %in% 2",
                     tag = paste0(varlist[v], " should be missing since ES05AH was set to 2 - Never vaccinated child"),
                     idlist = "ES01AA",checklist = c("ES05AH",varlist[v]),fix = TRUE)
        }
      }
    } #end of varlist v loop
  }

  # ES06AA=2 OR 3 SKIP TO ES07AA
  varlist <- c("ES06AB", "ES06ABSRC")
  for (v in seq_along(varlist)){
    if (varlist[v] %in% names(dat)){
      vartype = get(varlist[v],dat)
      if(any("numeric" %in% class(vartype)) | any("double" %in% class(vartype))){
        assertlist(dat = dat, var = varlist[v], f = paste0("is.na(dat$",varlist[v],")"),
                   condition = "dat$ES06AA %in% c(2,3)",
                   tag = paste0(varlist[v], " should be missing since ES06AA was set to 2 or 3- Do not have vaccination card with them"),
                   idlist = "ES01AA",checklist = c("ES06AA",varlist[v]),fix = TRUE)
      } else if (any("character" %in% class(vartype))){
        assertlist(dat = dat, var = varlist[v],
                   f = paste0("is.na(dat$",varlist[v],") | dat$",varlist[v]," == ''"),
                   condition = "dat$ES06AA %in% c(2,3)",
                   tag = paste0(varlist[v], " should be missing since ES06AA was set to 2 or 3- Do not have vaccination card with them"),
                   idlist = "ES01AA",checklist = c("ES06AA",varlist[v]),fix = TRUE)
      }
    }
  } #end of varlist v loop

  # ES08AA==1 SKIP TO ES08AB_1
  varlist <- c("ES08AA_1A_01", "ES08AA_1A_02", "ES08AA_1A_03", "ES08AA_1A_03_01", "ES08AA_1A_03_02",
               "ES08AA_1A_03_03", "ES08AA_1A_03_04", "ES08AA_1A_03_05", "ES08AA_1A_03_06", "ES08AA_1A_03_07",
               "ES08AA_1A_03_08", "ES08AA_1A_03_09", "ES08AA_1A_03_09DN", "ES08AA_1A_03_10", "ES08AA_1A_03_11",
               "ES08AA_1A_03_11OS", "ES08AA_1B_01", "ES08AA_1B_02", "ES08AA_1B_03", "ES08AA_1B_04", "ES08AA_1B_05",
               "ES08AA_1B_06", "ES08AA_1B_07", "ES08AA_1B_08", "ES08AA_1B_09", "ES08AA_1B_10", "ES08AA_1B_11",
               "ES08AA_1B_12", "ES08AA_1B_12OS", "ES08AA_1C_01", "ES08AA_1C_02", "ES08AA_1C_03", "ES08AA_1C_04",
               "ES08AA_1C_05", "ES08AA_1C_06", "ES08AA_1C_07", "ES08AA_1C_08", "ES08AA_1C_09", "ES08AA_1C_09OS")
  for (v in seq_along(varlist)){
    if (varlist[v] %in% names(dat)){
      vartype = get(varlist[v],dat)
      if(any("numeric" %in% class(vartype)) | any("double" %in% class(vartype))){
        assertlist(dat = dat, var = varlist[v], f = paste0("is.na(dat$",varlist[v],")"),
                   condition = "dat$ES08AA %in% 1",
                   tag = paste0(varlist[v], " should be missing since ES08AA was set to 1 - Child vaccinated during visit"),
                   idlist = "ES01AA",checklist = c("ES08AA",varlist[v]),fix = TRUE)
      } else if (any("character" %in% class(vartype))){
        assertlist(dat = dat, var = varlist[v],
                   f = paste0("is.na(dat$",varlist[v],") | dat$",varlist[v]," == ''"),
                   condition = "dat$ES08AA %in% 1",
                   tag = paste0(varlist[v], " should be missing since ES08AA was set to 1 - Child vaccinated during visit"),
                   idlist = "ES01AA",checklist = c("ES08AA",varlist[v]),fix = TRUE)
      }
    }
  } #end of varlist v loop

  varlist <- c("ES08AA_1A_03_01", "ES08AA_1A_03_02", "ES08AA_1A_03_03", "ES08AA_1A_03_04", "ES08AA_1A_03_05",
               "ES08AA_1A_03_06", "ES08AA_1A_03_07", "ES08AA_1A_03_08", "ES08AA_1A_03_09", "ES08AA_1A_03_09DN",
               "ES08AA_1A_03_10", "ES08AA_1A_03_11", "ES08AA_1A_03_11OS")
  for (v in seq_along(varlist)){
    if (varlist[v] %in% names(dat)){
      vartype = get(varlist[v],dat)
      if(any("numeric" %in% class(vartype)) | any("double" %in% class(vartype))){
        assertlist(dat = dat, var = varlist[v], f = paste0("is.na(dat$",varlist[v],")"),
                   condition = "dat$ES08AA_1A_03 %in% 2",
                   tag = paste0(varlist[v], "  should be missing since ES08AA_1A_03 was set to 2 - Did not mention doctor said child was sick"),
                   idlist = "ES01AA",checklist = c("ES08AA_1A_03",varlist[v]),fix = TRUE)
      } else if (any("character" %in% class(vartype))){
        assertlist(dat = dat, var = varlist[v],
                   f = paste0("is.na(dat$",varlist[v],") | dat$",varlist[v]," == ''"),
                   condition = "dat$ES08AA_1A_03 %in% 2",
                   tag = paste0(varlist[v], "  should be missing since ES08AA_1A_03 was set to 2 - Did not mention doctor said child was sick"),
                   idlist = "ES01AA",checklist = c("ES08AA_1A_03",varlist[v]),fix = TRUE)
      }
    }
  } #end of varlist v loop

  asserlist_cleanup(summary_dataframe = "assertlist_summary", output_dataframe = "assertlist_output",
                    filename = paste0(VCQI_OUTPUT_FOLDER,"/",
                                      VCQI_ANALYSIS_NAME,"_DQ.xlsx"),
                    sheetname = "dq_check_results",fix = TRUE)

  rm(assertlist_output, envir = .GlobalEnv) %>% suppressWarnings()
  rm(assertlist_summary, envir = .GlobalEnv) %>% suppressWarnings()
}
