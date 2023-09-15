#' This program makes derived variables needed to make study date MOV tables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @rawNamespace import(tools, except = makevars_user)
#' @importFrom utils glob2rx
#'
#' @return a dataset
#'
#' @export
#'
#' @examples
#' gen_MOV_dvs()

# gen_MOV_dvs R version 1.00 - Biostat Global Consulting - 2023-08-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-29  1.00      Mia Yu          Original R package version
# *******************************************************************************

gen_MOV_dvs <- function(VCP = "gen_MOV_dvs"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # *** Load dataset ***
  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_merged_vars.rds"))

  stype <- c("valid", "crude")
  for (vc in seq_along(stype)){
    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      if (d == 1){
        #not_elig_`vc'_list
        assign(paste0("not_elig_",stype[vc],"_list"),
               paste0("elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #elig_`vc'_list
        assign(paste0("elig_",stype[vc],"_list"),
               paste0("elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #got_none_`vc'_list
        assign(paste0("got_none_",stype[vc],"_list"),
               paste0("got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #got_at_least_one_`vc'_list
        assign(paste0("got_at_least_one_",stype[vc],"_list"),
               paste0("got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #got_all_elig_no_more_`vc'_list
        assign(paste0("got_all_elig_no_more_",stype[vc],"_list"),
               paste0("(correct_nodose_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1 | correct_validdose_",
                      MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1)"))

        #correct_validdose_`vc'_list
        assign(paste0("correct_validdose_",stype[vc],"_list"),
               paste0("correct_validdose_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #mov_`vc'_list
        assign(paste0("mov_",stype[vc],"_list"),
               paste0("mov_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #no_invalid_`vc'_list
        assign(paste0("no_invalid_",stype[vc],"_list"),
               paste0("invalid_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #at_least_one_invalid_`vc'_list
        assign(paste0("at_least_one_invalid_",stype[vc],"_list"),
               paste0("invalid_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #no_correct_validdose_`vc'_list
        assign(paste0("no_correct_validdose_",stype[vc],"_list"),
               paste0("correct_validdose_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #got_all_elig_`vc'_list
        assign(paste0("got_all_elig_",stype[vc],"_list"),
               paste0("((elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1 & got_",
                      MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1) | elig_",
                      MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0)"))
      } else {
        #not_elig_`vc'_list
        no_elig <- get(paste0("not_elig_",stype[vc],"_list"))
        assign(paste0("not_elig_",stype[vc],"_list"),
               paste0(no_elig," & elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #elig_`vc'_list
        elig <- get(paste0("elig_",stype[vc],"_list"))
        assign(paste0("elig_",stype[vc],"_list"),
               paste0(elig," | elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #got_none_`vc'_list
        got_none <- get(paste0("got_none_",stype[vc],"_list"))
        assign(paste0("got_none_",stype[vc],"_list"),
               paste0(got_none," & got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #got_at_least_one_`vc'_list
        got_one <- get(paste0("got_at_least_one_",stype[vc],"_list"))
        assign(paste0("got_at_least_one_",stype[vc],"_list"),
               paste0(got_one," | got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #got_all_elig_no_more_`vc'_list
        no_more <- get(paste0("got_all_elig_no_more_",stype[vc],"_list"))
        assign(paste0("got_all_elig_no_more_",stype[vc],"_list"),
               paste0(no_more," & (correct_nodose_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1 | correct_validdose_",
                      MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1)"))

        #correct_validdose_`vc'_list
        correct <- get(paste0("correct_validdose_",stype[vc],"_list"))
        assign(paste0("correct_validdose_",stype[vc],"_list"),
               paste0(correct," | correct_validdose_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #mov_`vc'_list
        mov <- get(paste0("mov_",stype[vc],"_list"))
        assign(paste0("mov_",stype[vc],"_list"),
               paste0(mov," | mov_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #no_invalid_`vc'_list
        no_invalid <- get(paste0("no_invalid_",stype[vc],"_list"))
        assign(paste0("no_invalid_",stype[vc],"_list"),
               paste0(no_invalid," & invalid_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #at_least_one_invalid_`vc'_list
        one_invalid <- get(paste0("at_least_one_invalid_",stype[vc],"_list"))
        assign(paste0("at_least_one_invalid_",stype[vc],"_list"),
               paste0(one_invalid," | invalid_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1"))

        #no_correct_validdose_`vc'_list
        no_correct <- get(paste0("no_correct_validdose_",stype[vc],"_list"))
        assign(paste0("no_correct_validdose_",stype[vc],"_list"),
               paste0(no_correct," & correct_validdose_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0"))

        #got_all_elig_`vc'_list
        got_all <- get(paste0("got_all_elig_",stype[vc],"_list"))
        assign(paste0("got_all_elig_",stype[vc],"_list"),
               paste0(got_all," & ((elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1 & got_",
                      MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 1) | elig_",
                      MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]," == 0)"))
      }

    } #end of MOV_OUTPUT_DOSE_LIST d loop

    # Add parentheses around the entire list

    #not_elig_`vc'_list
    no_elig <- get(paste0("not_elig_",stype[vc],"_list"))
    assign(paste0("not_elig_",stype[vc],"_list"),
           paste0("(",no_elig,")"))

    #elig_`vc'_list
    elig <- get(paste0("elig_",stype[vc],"_list"))
    assign(paste0("elig_",stype[vc],"_list"),
           paste0("(",elig,")"))

    #got_none_`vc'_list
    got_none <- get(paste0("got_none_",stype[vc],"_list"))
    assign(paste0("got_none_",stype[vc],"_list"),
           paste0("(",got_none,")"))

    #got_at_least_one_`vc'_list
    got_one <- get(paste0("got_at_least_one_",stype[vc],"_list"))
    assign(paste0("got_at_least_one_",stype[vc],"_list"),
           paste0("(",got_one,")"))

    #got_all_elig_no_more_`vc'_list
    no_more <- get(paste0("got_all_elig_no_more_",stype[vc],"_list"))
    assign(paste0("got_all_elig_no_more_",stype[vc],"_list"),
           paste0("(",no_more,")"))

    #correct_validdose_`vc'_list
    correct <- get(paste0("correct_validdose_",stype[vc],"_list"))
    assign(paste0("correct_validdose_",stype[vc],"_list"),
           paste0("(",correct,")"))

    #mov_`vc'_list
    mov <- get(paste0("mov_",stype[vc],"_list"))
    assign(paste0("mov_",stype[vc],"_list"),
           paste0("(",mov,")"))

    #no_invalid_`vc'_list
    no_invalid <- get(paste0("no_invalid_",stype[vc],"_list"))
    assign(paste0("no_invalid_",stype[vc],"_list"),
           paste0("(",no_invalid,")"))

    #at_least_one_invalid_`vc'_list
    one_invalid <- get(paste0("at_least_one_invalid_",stype[vc],"_list"))
    assign(paste0("at_least_one_invalid_",stype[vc],"_list"),
           paste0("(",one_invalid,")"))

    #no_correct_validdose_`vc'_list
    no_correct <- get(paste0("no_correct_validdose_",stype[vc],"_list"))
    assign(paste0("no_correct_validdose_",stype[vc],"_list"),
           paste0("(",no_correct,")"))

    #got_all_elig_`vc'_list
    got_all <- get(paste0("got_all_elig_",stype[vc],"_list"))
    assign(paste0("got_all_elig_",stype[vc],"_list"),
           paste0("(",got_all,")"))

  } #end of stype vc loop

  for (vc in seq_along(stype)){

    dat <- dat %>% mutate(tempvar1 = NA) %>% mutate(tempvar1 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar1))
    no_elig <- get(paste0("not_elig_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", no_elig, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar1 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar1)) %>%
      mutate(tempvar1 = as.numeric(tempvar1))

    dat <- dat %>% mutate(tempvar2 = NA) %>% mutate(tempvar2 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar2))
    elig <- get(paste0("elig_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", elig, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar2 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar2)) %>%
      mutate(tempvar2 = as.numeric(tempvar2))

    dat <- dat %>% mutate(tempvar3 = NA) %>% mutate(tempvar3 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar3))
    got_none <- get(paste0("got_none_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", got_none, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar3 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar3)) %>%
      mutate(tempvar3 = as.numeric(tempvar3))

    dat <- dat %>% mutate(tempvar4 = NA) %>% mutate(tempvar4 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar4))
    got_one <- get(paste0("got_at_least_one_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", got_one, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar4 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar4)) %>%
      mutate(tempvar4 = as.numeric(tempvar4))

    dat <- dat %>% mutate(tempvar5 = NA) %>% mutate(tempvar5 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar5))
    no_more <- get(paste0("got_all_elig_no_more_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", no_more, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar5 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar5)) %>%
      mutate(tempvar5 = as.numeric(tempvar5))

    dat <- dat %>% mutate(tempvar6 = NA) %>% mutate(tempvar6 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar6))
    correct <- get(paste0("correct_validdose_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", correct, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar6 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar6)) %>%
      mutate(tempvar6 = as.numeric(tempvar6))

    dat <- dat %>% mutate(tempvar7 = NA) %>% mutate(tempvar7 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar7))
    mov <- get(paste0("mov_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", mov, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar7 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar7)) %>%
      mutate(tempvar7 = as.numeric(tempvar7))

    dat <- dat %>% mutate(tempvar8 = NA) %>% mutate(tempvar8 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar8))
    no_invalid <- get(paste0("no_invalid_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", no_invalid, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar8 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar8)) %>%
      mutate(tempvar8 = as.numeric(tempvar8))

    dat <- dat %>% mutate(tempvar9 = NA) %>% mutate(tempvar9 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar9))
    one_invalid <- get(paste0("at_least_one_invalid_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", one_invalid, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar9 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar9)) %>%
      mutate(tempvar9 = as.numeric(tempvar9))

    dat <- dat %>% mutate(tempvar10 = NA) %>% mutate(tempvar10 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar10))
    no_correct <- get(paste0("no_correct_validdose_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", no_correct, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar10 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar10)) %>%
      mutate(tempvar10 = as.numeric(tempvar10))

    dat <- dat %>% mutate(tempvar11 = NA) %>% mutate(tempvar11 = ifelse(has_card_with_dob_and_dose %in% 1,0,tempvar11))
    got_all <- get(paste0("got_all_elig_",stype[vc],"_list"))
    conditions <- paste0("(has_card_with_dob_and_dose==1 & ", got_all, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar11 = ifelse(eval(rlang::parse_expr(conditions)),1,tempvar11)) %>%
      mutate(tempvar11 = as.numeric(tempvar11))


    dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("Not eligible for any vaccine (",stype[vc],")"))
    dat$tempvar2 <- haven::labelled(dat$tempvar2, label = paste0("Eligible for 1+ vaccines (",stype[vc],")"))
    dat$tempvar3 <- haven::labelled(dat$tempvar3, label = paste0("Rec'd no doses (",stype[vc],")"))
    dat$tempvar4 <- haven::labelled(dat$tempvar4, label = paste0("Rec'd 1+ doses (",stype[vc],")"))
    dat$tempvar5 <- haven::labelled(dat$tempvar5, label = paste0("Rec'd all doses elig for & no others (no errors) (",stype[vc],")"))
    dat$tempvar6 <- haven::labelled(dat$tempvar6, label = paste0("Rec'd 1+ correct valid doses (elig for & got dose) (",stype[vc],")"))
    dat$tempvar7 <- haven::labelled(dat$tempvar7, label = paste0("Had at least one MOV (elig for & didn't get dose) (",stype[vc],")"))
    dat$tempvar8 <- haven::labelled(dat$tempvar8, label = paste0("Had no invalid doses (if not elig, didn't receive it) (",stype[vc],")"))
    dat$tempvar9 <- haven::labelled(dat$tempvar9, label = paste0("Had at least 1 invalid dose (not elig but rec'd it) (",stype[vc],")"))
    dat$tempvar10 <- haven::labelled(dat$tempvar10, label = paste0("Had no correct valid doses (did not receive any doses elig for) (",stype[vc],")"))
    dat$tempvar11 <- haven::labelled(dat$tempvar11, label = paste0("Got all doses elig for (may or may not have also rec'd invalid dose) (",stype[vc],")"))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("not_elig_",stype[vc])
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("elig_",stype[vc])
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("got_none_",stype[vc])
    names(dat)[which(names(dat) == "tempvar4")] <- paste0("got_at_least_one_",stype[vc])
    names(dat)[which(names(dat) == "tempvar5")] <- paste0("got_all_elig_no_more_",stype[vc])
    names(dat)[which(names(dat) == "tempvar6")] <- paste0("correct_validdose_",stype[vc])
    names(dat)[which(names(dat) == "tempvar7")] <- paste0("mov_",stype[vc])
    names(dat)[which(names(dat) == "tempvar8")] <- paste0("no_invalid_",stype[vc])
    names(dat)[which(names(dat) == "tempvar9")] <- paste0("at_least_one_invalid_",stype[vc])
    names(dat)[which(names(dat) == "tempvar10")] <- paste0("no_correct_validdose_",stype[vc])
    names(dat)[which(names(dat) == "tempvar11")] <- paste0("got_all_elig_",stype[vc])

  } #end of stype vc loop

  # **********************************************
  # Table 1: Study day child eligibility summary
  #    Need: 1 variable with 3 levels
  # Analyze: DESC_02 with 1 subtotal
  # **********************************************

  # Value labels
  # table1_dv definitions:
  #  0 - Not eligible for any dose means child has a valid dob & at least one dose date, but not eligible for any other dose
  #  1 - Eligible for 1+ doses means child has a valid dob & at least one dose date, and is eligible for at least 1 dose
  #  . - No card information means missing or invalid dob and/or no dose dates on card  */
  #  label define table1_dv 0 "Children not eligible for any dose" 1 "Children eligible for 1+ doses" .a "Children with no card information"

  label0 <- language_string(language_use = language_use, str = "OS_202", replaceq = TRUE)
  label1 <- language_string(language_use = language_use, str = "OS_203", replaceq = TRUE)
  label2 <- language_string(language_use = language_use, str = "OS_204", replaceq = TRUE)

  for (vc in seq_along(stype)){
    if (stype[vc] == "crude"){
      vc2 <- language_string(language_use = language_use, str = "OS_14") # Crude
    } else {
      vc2 <- language_string(language_use = language_use, str = "OS_80") # Valid
    }

    dat <- dat %>% mutate(tempvar1 = tagged_na("a")) #NOTE: this is like .a in Stata but will still show as NA
    not_elig <- get(paste0("not_elig_",stype[vc],"_list"))
    conditions1 <- paste0("(has_card_with_dob_and_dose==1 & ", not_elig, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar1 = ifelse(eval(rlang::parse_expr(conditions1)),0,tempvar1))
    elig <- get(paste0("elig_",stype[vc],"_list"))
    conditions2 <- paste0("(has_card_with_dob_and_dose==1 & ", elig, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar1 = ifelse(eval(rlang::parse_expr(conditions2)),1,tempvar1))

    varlabel <- paste0(language_string(language_use = language_use, str = "OS_113", replaceq = TRUE),
                       " (",vc2,")")
    f <- paste0("dat$tempvar1 <- haven::labelled(dat$tempvar1, label = '",varlabel,"', labels = c('",label0,
                "' = 0, '",label1,"' = 1, '",label2,"' = tagged_na('a')))")
    eval(rlang::parse_expr(f))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("table1_dv_",stype[vc])
    # NOTE: use DESC_02 to Re-label missing values .a as 3 & summarize
  } #end of stype vc loop

  # **********************************************
  # Table 2: Study day performance details
  #    Need: 1 variable with 8 levels
  # Analyze: DESC_02 with 2 subtotals
  # **********************************************

  # Value labels
  # table1_dv definitions:
  # 1 - Not eligible for any dose (defined in table1) & received 0 doses
  # 2 - Not eligible for any dose (defined in table1) & received 1+ doses
  # 3 - Eligible for 1+ dose (as defined in table1) & received 0 doses
  # 4 - Eligible for 1+ dose (as defined in table1) & (received all doses eligible for & no other doses)
  # 5 - Eligible for 1+ dose (as defined in table1) & (received some doses eligible for & no other doses)
  # 6 - Eligible for 1+ dose (as defined in table1) & (received all doses eligible for & some other doses not eligible for)
  # 7 - Eligible for 1+ dose (as defined in table1) & (received some doses eligible for & some other doses not eligible for)
  # 8 - Eligible for 1+ dose (as defined in table1) & (received 0 doses eligible for & some other doses not eligible for)
  #
  #  **************************************************************************************
  # This code is for the new table2 that breaks up by if eligible for 0,1, and 2+ doses
  # Create variable to show how many doses child was eligible for

  # label define table2_dv 1 "Not eligible for any dose & rec'd 0 doses[1][5]", replace
  # label define table2_dv 2 "Not eligible for any dose and rec'd 1+ doses[3]", modify
  # label define table2_dv 3 "Eligible for 1 dose & rec'd 0 doses[4][5]", modify
  # label define table2_dv 4 "Eligible for 1 dose & recd that 1 dose & no others[1][2]", modify
  # label define table2_dv 5 "Eligible for 1 dose & recd that 1 dose & some others[2][3]", modify
  # label define table2_dv 6 "Eligible for 1 dose & did not rec that 1 dose & recd some others[3][4][6]", modify
  # label define table2_dv 7 "Eligible for 2+ doses & recd 0 doses[4][5]", modify
  # label define table2_dv 8 "Eligible for 2+ doses & recd all doses elig & no others[1][2]", modify
  # label define table2_dv 9 "Eligible for 2+ doses & recd some doses elig & no others[2][4][6]", modify
  # label define table2_dv 10 "Eligible for 2+ doses & recd all doses elig & some others[2][3]", modify
  # label define table2_dv 11 "Eligible for 2+ doses & recd some doses elig & some others[2][3][4][6]", modify
  # label define table2_dv 12 "Eligible for 2+ doses & recd no doses elig & some others[3][4][6]", modify

  label1 <- paste0(language_string(language_use = language_use, str = "OS_205", replaceq = TRUE),"[1][5]")
  label2 <- paste0(language_string(language_use = language_use, str = "OS_206", replaceq = TRUE),"[3]")
  label3 <- paste0(language_string(language_use = language_use, str = "OS_207", replaceq = TRUE),"[4][5]")
  label4 <- paste0(language_string(language_use = language_use, str = "OS_208", replaceq = TRUE),"[1][2]")
  label5 <- paste0(language_string(language_use = language_use, str = "OS_209", replaceq = TRUE),"[2][3]")
  label6 <- paste0(language_string(language_use = language_use, str = "OS_210", replaceq = TRUE),"[3][4][6]")
  label7 <- paste0(language_string(language_use = language_use, str = "OS_211", replaceq = TRUE),"[4][5]")
  label8 <- paste0(language_string(language_use = language_use, str = "OS_212", replaceq = TRUE),"[1][2]")
  label9 <- paste0(language_string(language_use = language_use, str = "OS_213", replaceq = TRUE),"[2][4][6]")
  label10 <- paste0(language_string(language_use = language_use, str = "OS_214", replaceq = TRUE),"[2][3]")
  label11 <- paste0(language_string(language_use = language_use, str = "OS_215", replaceq = TRUE),"[2][3][4][6]")
  label12 <- paste0(language_string(language_use = language_use, str = "OS_216", replaceq = TRUE),"[3][4][6]")

  for (vc in seq_along(stype)){
    if (stype[vc] == "crude"){
      vc2 <- language_string(language_use = language_use, str = "OS_14") # Crude
    } else {
      vc2 <- language_string(language_use = language_use, str = "OS_80") # Valid
    }

    dat <- dat %>% mutate(tempvar1 = 0) #table2_elig_count_vc

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      delig <- rlang::sym(paste0("elig_",MOV_OUTPUT_DOSE_LIST[d],'_',stype[vc]))
      dat <- dat %>% mutate(tempvar1 = ifelse(!!delig %in% 1, tempvar1 + 1, tempvar1))
    } #end of MOV_OUTPUT_DOSE_LIST d loop

    # Children with cards category (N) & (%)
    dat <- dat %>% mutate(tempvar2 = NA)

    table1 <- rlang::sym(paste0("table1_dv_",stype[vc]))
    got_none <- rlang::sym(paste0("got_none_",stype[vc]))
    got_one <- rlang::sym(paste0("got_at_least_one_",stype[vc]))
    got_all <- rlang::sym(paste0("got_all_elig_no_more_",stype[vc]))
    one_invalid <- rlang::sym(paste0("at_least_one_invalid_",stype[vc]))
    correct <- rlang::sym(paste0("correct_validdose_",stype[vc]))
    no_correct <- rlang::sym(paste0("no_correct_validdose_",stype[vc]))
    mov <- rlang::sym(paste0("mov_",stype[vc]))
    no_invalid <- rlang::sym(paste0("no_invalid_",stype[vc]))

    # eligible for 0
    # not elig; rec'd 0
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 0 & !!got_none ==1) %in% TRUE, 1, tempvar2))
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 0 & !!got_one ==1) %in% TRUE, 2, tempvar2))

    # eligible for 1
    # elig for 1+; rec'd 0
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!got_none ==1 & tempvar1 == 1) %in% TRUE, 3, tempvar2))

    # elig for 1+; rec'd all doses elig for & no others
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!got_all ==1 & tempvar1 == 1) %in% TRUE, 4, tempvar2))

    # elig for 1+; rec'd all doses elig for & some others not elig for
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!got_all ==1 & !!one_invalid == 1 & tempvar1 == 1) %in% TRUE, 5, tempvar2))

    # elig for 1+; rec'd 0 elig for & some others not elig for
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!no_correct ==1 & !!mov == 1 & !!one_invalid == 1 & tempvar1 == 1) %in% TRUE,
                                            6, tempvar2))

    # eligible for 2+
    # elig for 1+; rec'd 0
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!got_none ==1 & tempvar1 >= 2) %in% TRUE, 7, tempvar2))

    # elig for 1+; rec'd all doses elig for & no others
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!got_all ==1 & tempvar1 >= 2) %in% TRUE, 8, tempvar2))

    # elig for 1+; rec'd some doses elig for & no others
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!correct == 1 & !!mov == 1 & !!no_invalid == 1 & tempvar1 >= 2) %in% TRUE,
                          9, tempvar2))

    # elig for 1+; rec'd all doses elig for & some others not elig for
    dat <- dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!got_all ==1 & !!one_invalid == 1 & tempvar1 >= 2) %in% TRUE, 10, tempvar2))

    # elig for 1+; rec'd some doses elig for & some others not elig for
    dat <-dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!correct == 1 & !!mov == 1 & !!one_invalid == 1 & tempvar1 >= 2) %in% TRUE,
                                           11, tempvar2))
    # elig for 1+; rec'd 0 elig for & some others not elig for
    dat <-dat %>% mutate(tempvar2 = ifelse((!!table1 == 1 & !!no_correct == 1 & !!mov == 1 & !!one_invalid == 1 & tempvar1 >= 2) %in% TRUE,
                                           12, tempvar2))

    #label var table2_dv_`vc' "Study day performance details (`vc')"
    varlabel <- paste0(language_string(language_use = language_use, str = "OS_116", replaceq = TRUE),
                       " (",vc2,")")

    f <- paste0("dat$tempvar2 <- haven::labelled(dat$tempvar2, label = '",varlabel,"', labels = c('",label1,
                "' = 1, '",label2,"' = 2, '",label3,"' = 3, '", label4, "' = 4, '", label5, "' = 5, '", label6,
                "' = 6, '",label7,"' = 7, '",label8,"' = 8, '", label9, "' = 9, '", label10, "' = 10, '", label11,
                "' = 11, '",label12,"' = 12))" )
    eval(rlang::parse_expr(f))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("table2_elig_count_",stype[vc])
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("table2_dv_",stype[vc])

  } #end of stype vc loop

  # **********************************************
  # Table 3: Study day performance summary
  #    Need: 4 variables each with 2 levels (plus a missing level - for those who didn't have card)
	# Analyze: DESC_03 (denom will be all who responded)
	#	**********************************************

  for (vc in seq_along(stype)){
    if (stype[vc] == "crude"){
      vc2 <- language_string(language_use = language_use, str = "OS_14") # Crude
    } else {
      vc2 <- language_string(language_use = language_use, str = "OS_80") # Valid
    }

    dgot_all <- rlang::sym(paste0("got_all_elig_no_more_",stype[vc]))
    done_invalid <- rlang::sym(paste0("at_least_one_invalid_",stype[vc]))
    dcorrect <- rlang::sym(paste0("correct_validdose_",stype[vc]))
    dmov <- rlang::sym(paste0("mov_",stype[vc]))

    dat <- dat %>% mutate(tempvar1 = !!dgot_all,
                          tempvar2 = !!done_invalid,
                          tempvar3 = !!dcorrect,
                          tempvar4 = !!dmov)

    # label variable table3_dv_no_errors_`vc' "No Errors - `vc'"
    dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                    label = paste0(language_string(language_use = language_use, str = "OS_222")," - ",vc2))
    # label variable table3_dv_recd_invalid_`vc' "Received Invalid Doses - `vc'"
    dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                    label = paste0(language_string(language_use = language_use, str = "OS_223")," - ",vc2))
    # label variable table3_dv_recd_valid_`vc' "Received Valid Doses - `vc'"
    dat$tempvar3 <- haven::labelled(dat$tempvar3,
                                    label = paste0(language_string(language_use = language_use, str = "OS_224")," - ",vc2))
    # label variable table3_dv_mov_`vc' "Experienced 1+ MOV -`vc'"
    dat$tempvar4 <- haven::labelled(dat$tempvar4,
                                    label = paste0(language_string(language_use = language_use, str = "OS_225")," - ",vc2))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("table3_dv_no_errors_",stype[vc]) # Table 3 column 1: No errors
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("table3_dv_recd_invalid_",stype[vc]) # Table 3 column 2: Rec'd invalid doses
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("table3_dv_recd_valid_",stype[vc]) # Table 3 column 3: Rec'd valid doses
    names(dat)[which(names(dat) == "tempvar4")] <- paste0("table3_dv_mov_",stype[vc]) # Table 3 column 4: Experienced 1+ MOV

  } #end of stype vc loop
  # Number of children who had cards (N) (Note: no missing values here b/c either have a card or don't...missing & no card are the same)
	#   Note: This variable is the same regardless of valid/crude calculation
  dat <- dat %>% mutate(table3_dv_had_card = has_card_with_dob_and_dose)
  # label variable table3_dv_had_card "Total Number of Children with Cards"
  dat$table3_dv_had_card <- haven::labelled(dat$table3_dv_had_card, label = language_string(language_use = language_use, str = "OS_221"))

  # **************************************************
  # Table 4: Invalid doses administered on visit day
  #    Need: 1 variable with 2 levels (plus missing level) for each dose (missing level dose does not get summarized) & 3 end-cap variables
  # Analyze: Use DESC_02 for each variable with it's new features (N/%) & append to right
	#	**************************************************


  # *** Table 4: Left-side of table by dose (loop over valid/crude) ***
  for (vc in seq_along(stype)){

    dat <- dat %>% mutate(tempvar1 = NA) %>% mutate(tempvar1 = ifelse(has_card_with_dob_and_dose %in% 1, 0, tempvar1))
    dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                    label = paste0("Children with cards who received 1+ doses they weren't eligible for (",stype[vc],")"))

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      delig <- rlang::sym(paste0("elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))
      dgot <- rlang::sym(paste0("got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))

      dat <- dat %>% mutate(tempvar2 = NA) %>%
        mutate(tempvar2 = ifelse((has_card_with_dob_and_dose == 1 & !!delig == 0 & !!dgot == 1) %in% TRUE, 1, tempvar2)) %>% #invalid dose
        mutate(tempvar2 = ifelse((has_card_with_dob_and_dose == 1 & !!delig == 0 & !!dgot == 0) %in% TRUE, 0, tempvar2)) #correct no dose
      dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                      label = paste0("Invalid Dose (",stype[vc],") - ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
                                      labels = c("% Vx'd (invalid dose)" = 1, "% Not Vx'd (correct)" = 0))
      # NOTE: use DESC_02 to summarize (exclude missing obs - so use sub-total)

      # Update End-cap variable
      dat <- dat %>% mutate(tempvar1 = ifelse(tempvar2 %in% 1, 1, tempvar1))
      names(dat)[which(names(dat) == "tempvar2")] <- paste0("table4_dv_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc])
    } # end of MOV_OUTPUT_DOSE_LIST d loop

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("table4_dv_not_elig_",stype[vc])
  } # end of stype vc loop
  # Number of children who had cards (N) (Note: no missing values here b/c either have a card or don't...missing & no card are the same)
	#   Note: This variable is the same regardless of valid/crude calculation
  dat <- dat %>% mutate(table4_dv_had_card = has_card_with_dob_and_dose)

  # ******************************************************
  # Table 5: MOV & valid doses administered on visit day
  #    Need: 1 variable with 3 levels for each dose (missing level does not get summarized) & 7 end-cap variables
  # Analyze: WILL NEED A NEW PROGRAM TO DO THIS TABLE
  #          Dose indicators (left side of table) can be summarized with DESC_02 using new features (b/c not summarizing missing values)
  #          End-cap will need special code
  # ******************************************************

  for (vc in seq_along(stype)){
    dat <- dat %>% mutate(tempvar1 = NA, tempvar2 = 0, tempvar3 = 0, tempvar4 = NA)

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      delig <- rlang::sym(paste0("elig_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))
      dgot <- rlang::sym(paste0("got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))
      dmov <- rlang::sym(paste0("mov_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))

      dat <- dat %>% mutate(tempvar5 = NA) %>%
        mutate(tempvar5 = ifelse((has_card_with_dob_and_dose == 1 & !!delig == 1 & !!dgot == 1) %in% TRUE, 0, tempvar5)) %>% # valid dose
        mutate(tempvar5 = ifelse((has_card_with_dob_and_dose == 1 & !!dmov == 1) %in% TRUE, 1, tempvar5)) # MOV dose
      dat$tempvar5 <- haven::labelled(dat$tempvar5,
                                      label = paste0("MOV (",stype[vc],") - ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
                                      labels = c("% Not Vx'd (MOV)" = 1, "% Vx'd (valid dose)" = 0))

      # Below Variable Added 1-3-2018 MKT: Needed for table 5 ES_STUD_01_04GO
      #TODO: double check this part
      dat <- dat %>% mutate(tempvar6 = ifelse(!is.na(tempvar5),ifelse(tempvar5 %in% 1, 0, 1),NA))
      dat$tempvar6 <- haven::labelled(dat$tempvar6,
                                      label = paste0("Valid Vaccinated (",stype[vc],") - ",str_to_upper(MOV_OUTPUT_DOSE_LIST[d])),
                                      labels = c("Vx'd (valid dose)" = 1, "% Not Vx'd (MOV)" = 0))

      dat <- dat %>% mutate(tempvar1 = ifelse(tempvar5 %in% 1, 1, tempvar1)) %>%
        mutate(tempvar1 = ifelse((!!delig == 1 & is.na(tempvar1)) %in% TRUE, 0, tempvar1))

      dat <- dat %>% mutate(tempvar4 = ifelse(tempvar6 %in% 1, 1, tempvar4)) %>%
        mutate(tempvar4 = ifelse((!!delig == 1 & is.na(tempvar4)) %in% TRUE, 0, tempvar4))

      dat <- dat %>% mutate(tempvar2 = ifelse(tempvar5 %in% 1, tempvar2 + 1, tempvar2)) %>%
        mutate(tempvar3 = ifelse(!!delig %in% 1, tempvar3 + 1, tempvar3))

      names(dat)[which(names(dat) == "tempvar5")] <- paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_mov_",stype[vc])
      names(dat)[which(names(dat) == "tempvar6")] <- paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_valid_",stype[vc])

    } # end of MOV_OUTPUT_DOSE_LIST d loop

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("table5_any_mov_",stype[vc]) # col I & J & K
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("table5_mov_count_",stype[vc]) # col L & M
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("table5_elig_count_",stype[vc]) # col K
    names(dat)[which(names(dat) == "tempvar4")] <- paste0("table5_any_valid_",stype[vc]) # col F & G & H

  } # end of stype vc loop

  # Number of children who had cards (N) (Note: no missing values here b/c either have a card or don't...missing & no card are the same)
	#   Note: This variable is the same regardless of valid/crude calculation
  dat <- dat %>% mutate(table5_dv_had_card = has_card_with_dob_and_dose, table5_dv_had_card_endcap = table5_dv_had_card) %>%
    mutate(table5_dv_had_card_endcap = ifelse(!table5_dv_had_card %in% 1, NA, table5_dv_had_card_endcap))

  # ******************************************************
  # Reasons: Reasons for MOV on visit day
  #    Need: 4 variables each with 2 levels (plus missing level)
  #          These derived variables plus ES08AA* indicator variables will
  #            be summarized in MOV reasons tables.
  # Analyze: DESC_03
  # ******************************************************

  # Make local macro to use in "if" statement for missing category
  reasons_var_missing_list <- "((ES08AA_1B_12OS == '' & ES08AA_1C_09OS == '') | (is.na(ES08AA_1B_12OS) & is.na(ES08AA_1C_09OS)))"
  varlist <- c("ES08AA_1A_01", "ES08AA_1A_02", "ES08AA_1A_03","ES08AA_1B_01", "ES08AA_1B_02", "ES08AA_1B_03", "ES08AA_1B_04",
               "ES08AA_1B_05", "ES08AA_1B_06", "ES08AA_1B_07", "ES08AA_1B_08", "ES08AA_1B_09", "ES08AA_1B_10", "ES08AA_1B_11",
               "ES08AA_1B_12", "ES08AA_1C_01", "ES08AA_1C_02", "ES08AA_1C_03", "ES08AA_1C_04", "ES08AA_1C_05", "ES08AA_1C_06",
               "ES08AA_1C_07", "ES08AA_1C_08", "ES08AA_1C_09")

  for (v in seq_along(varlist)){
    reasons_var_missing_list <- paste0(reasons_var_missing_list," & (",varlist[v]," == 2 | is.na(",varlist[v],"))")
  } #end of varlist v loop

  # Check the variable ES08AA against the got* variables from calc_MOVs
  # Need to create separate ES08AA for crude and valid
  for (vc in seq_along(stype)){
    dat <- dat %>% mutate(tempvar1 = 0, tempvar2 = ES08AA)

    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      dgot <- rlang::sym(paste0("got_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))
      dat <- dat %>% mutate(tempvar1 = ifelse(!!dgot %in% 1, tempvar1 + 1, tempvar1))
    } # end of MOV_OUTPUT_DOSE_LIST d loop

    dat <- dat %>% mutate(tempvar2 = ifelse((tempvar1 >= 1) %in% TRUE, 1, tempvar2)) %>%
      mutate(tempvar2 = ifelse((tempvar1 == 0) %in% TRUE, 2, tempvar2))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("got_dose_today_",stype[vc])
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("ES08AA_",stype[vc])
  } # end of stype vc loop

  # Make indicator variables for each reason category A, B, C & missing (D)

  for (vc in seq_along(stype)){
    if (stype[vc] == "crude"){
      vc2 <- language_string(language_use = language_use, str = "OS_14") # Crude
    } else {
      vc2 <- language_string(language_use = language_use, str = "OS_80") # Valid
    }

    table1 <- rlang::sym(paste0("table1_dv_",stype[vc]))
    AA <- rlang::sym(paste0("ES08AA_",stype[vc]))
    got_one <- rlang::sym(paste0("got_at_least_one_",stype[vc]))
    got_all <- rlang::sym(paste0("got_all_elig_no_more_",stype[vc]))
    one_invalid <- rlang::sym(paste0("at_least_one_invalid_",stype[vc]))
    correct <- rlang::sym(paste0("correct_validdose_",stype[vc],"_list"))
    no_correct <- rlang::sym(paste0("no_correct_validdose_",stype[vc],"_list"))
    mov <- rlang::sym(paste0("mov_",stype[vc],"_list"))
    no_invalid <- rlang::sym(paste0("no_invalid_",stype[vc],"_list"))

    dat <- dat %>% mutate(tempvar0 = !!table1) #elig_indicator_`vc'

    # *** Reasons_1: child elig for 0 doses (shouldn't have been asked why not vac'd but was) (elig_indicator_`vc'==0)***
		# Reasons related to health workers
    dat <- dat %>% mutate(tempvar1 = NA) %>%
      mutate(tempvar1 = ifelse((tempvar0 == 0 & !!AA == 2) %in% TRUE, 0, tempvar1)) %>%
      mutate(tempvar1 = ifelse((tempvar0 == 0 & !!AA == 2 &
                                  (ES08AA_1A_01==1 | ES08AA_1A_02==1 | ES08AA_1A_03==1)) %in% TRUE, 1, tempvar1))
    # label var reasons_overview1_A_`vc' "MOV: Reasons related to health workers (`vc')"
    dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                    label = paste0(language_string(language_use = language_use, str = "OS_226")," (",vc2,")"))

    # Reasons related to caregivers
    dat <- dat %>% mutate(tempvar2 = NA) %>%
      mutate(tempvar2 = ifelse((tempvar0 == 0 & !!AA == 2) %in% TRUE, 0, tempvar2)) %>%
      mutate(tempvar2 = ifelse((tempvar0 == 0 & !!AA == 2 &
                                  (ES08AA_1B_01==1 | ES08AA_1B_02==1 | ES08AA_1B_03==1 | ES08AA_1B_04==1 | ES08AA_1B_05==1 |
                                     ES08AA_1B_06==1 | ES08AA_1B_07==1 | ES08AA_1B_08==1 | ES08AA_1B_09==1 | ES08AA_1B_10==1 |
                                     ES08AA_1B_11==1 | ES08AA_1B_12==1 | (!is.na(ES08AA_1B_12OS) & ES08AA_1B_12OS != ""))) %in% TRUE, 1, tempvar2))
    # label var reasons_overview1_B_`vc' "MOV: Reasons related to caregiver (`vc')"
    dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                    label = paste0(language_string(language_use = language_use, str = "OS_227")," (",vc2,")"))

    # Reasons related to health service
    dat <- dat %>% mutate(tempvar3 = NA) %>%
      mutate(tempvar3 = ifelse((tempvar0 == 0 & !!AA == 2) %in% TRUE, 0, tempvar3)) %>%
      mutate(tempvar3 = ifelse((tempvar0 == 0 & !!AA == 2 &
                                  (ES08AA_1C_01==1 | ES08AA_1C_02==1 | ES08AA_1C_03==1 | ES08AA_1C_04==1 | ES08AA_1C_05==1 |
                                     ES08AA_1C_06==1 | ES08AA_1C_07==1 | ES08AA_1C_08==1 | ES08AA_1C_09==1 |
                                     (!is.na(ES08AA_1C_09OS) & ES08AA_1C_09OS != ""))) %in% TRUE,1, tempvar3))
    # label var reasons_overview1_C_`vc' "MOV: Reasons related to health services (`vc')"
    dat$tempvar3 <- haven::labelled(dat$tempvar3,
                                    label = paste0(language_string(language_use = language_use, str = "OS_228")," (",vc2,")"))

    # Reason is missing
    #conditions <- paste0("(tempvar0 == 0 & !!AA == 2 & (", reasons_var_missing_list, ")) %in% TRUE")
    dat <- dat %>% mutate(tempvar4 = NA) %>%
      mutate(tempvar4 = ifelse((tempvar0 == 0 & !!AA == 2) %in% TRUE, 0, tempvar4)) %>%
      mutate(tempvar4 = ifelse(tempvar0 == 0 & !!AA == 2 & eval(rlang::parse_expr(reasons_var_missing_list)), 1, tempvar4))
    # label var reasons_overview1_D_`vc' "MOV: Reason missing (`vc')"
    dat$tempvar4 <- haven::labelled(dat$tempvar4,
                                    label = paste0(language_string(language_use = language_use, str = "OS_229")," (",vc2,")"))

		# *** Reasons_2: child elig for 1+ doses (elig_indicator_`vc'==1) ***
    # Reasons related to health workers
    dat <- dat %>% mutate(tempvar5 = NA) %>%
      mutate(tempvar5 = ifelse((tempvar0 == 1 & !!AA == 2) %in% TRUE, 0, tempvar5)) %>%
      mutate(tempvar5 = ifelse((tempvar0 == 1 & !!AA == 2 &
                                  (ES08AA_1A_01==1 | ES08AA_1A_02==1 | ES08AA_1A_03==1)) %in% TRUE, 1, tempvar5))
    # label var reasons_overview2_A_`vc' "MOV: Reasons related to health workers (`vc')"
    dat$tempvar5 <- haven::labelled(dat$tempvar5,
                                    label = paste0(language_string(language_use = language_use, str = "OS_226")," (",vc2,")"))

    # Reasons related to caregivers
    dat <- dat %>% mutate(tempvar6 = NA) %>%
      mutate(tempvar6 = ifelse((tempvar0 == 1 & !!AA == 2) %in% TRUE, 0, tempvar6)) %>%
      mutate(tempvar6 = ifelse((tempvar0 == 1 & !!AA == 2 &
                                  (ES08AA_1B_01==1 | ES08AA_1B_02==1 | ES08AA_1B_03==1 | ES08AA_1B_04==1 | ES08AA_1B_05==1 |
                                     ES08AA_1B_06==1 | ES08AA_1B_07==1 | ES08AA_1B_08==1 | ES08AA_1B_09==1 | ES08AA_1B_10==1 |
                                     ES08AA_1B_11==1 | ES08AA_1B_12==1 | (!is.na(ES08AA_1B_12OS) & ES08AA_1B_12OS != ""))) %in% TRUE, 1, tempvar6))
    # label var reasons_overview2_B_`vc' "MOV: Reasons related to caregiver (`vc')"
    dat$tempvar6 <- haven::labelled(dat$tempvar6,
                                    label = paste0(language_string(language_use = language_use, str = "OS_227")," (",vc2,")"))

    # Reasons related to health service
    dat <- dat %>% mutate(tempvar7 = NA) %>%
      mutate(tempvar7 = ifelse((tempvar0 == 1 & !!AA == 2) %in% TRUE, 0, tempvar7)) %>%
      mutate(tempvar7 = ifelse((tempvar0 == 1 & !!AA == 2 &
                                  (ES08AA_1C_01==1 | ES08AA_1C_02==1 | ES08AA_1C_03==1 | ES08AA_1C_04==1 | ES08AA_1C_05==1 |
                                     ES08AA_1C_06==1 | ES08AA_1C_07==1 | ES08AA_1C_08==1 | ES08AA_1C_09==1 |
                                     (!is.na(ES08AA_1C_09OS) & ES08AA_1C_09OS != ""))) %in% TRUE,1, tempvar7))
    # label var reasons_overview2_C_`vc' "MOV: Reasons related to health services (`vc')"
    dat$tempvar7 <- haven::labelled(dat$tempvar7,
                                    label = paste0(language_string(language_use = language_use, str = "OS_228")," (",vc2,")"))

    # Reason is missing
    #conditions <- paste0("(tempvar0 == 1 & !!AA == 2 & ", reasons_var_missing_list, ") %in% TRUE")
    dat <- dat %>% mutate(tempvar8 = NA) %>%
      mutate(tempvar8 = ifelse((tempvar0 == 1 & !!AA == 2) %in% TRUE, 0, tempvar8)) %>%
      mutate(tempvar8 = ifelse(tempvar0 == 1 & !!AA == 2 & eval(rlang::parse_expr(reasons_var_missing_list)), 1, tempvar8))
    # label var reasons_overview2_D_`vc' "MOV: Reason missing (`vc')"
    dat$tempvar8 <- haven::labelled(dat$tempvar8,
                                    label = paste0(language_string(language_use = language_use, str = "OS_229")," (",vc2,")"))

    names(dat)[which(names(dat) == "tempvar0")] <- paste0("elig_indicator_",stype[vc])
    names(dat)[which(names(dat) == "tempvar1")] <- paste0("reasons_overview1_A_",stype[vc])
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("reasons_overview1_B_",stype[vc])
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("reasons_overview1_C_",stype[vc])
    names(dat)[which(names(dat) == "tempvar4")] <- paste0("reasons_overview1_D_",stype[vc])
    names(dat)[which(names(dat) == "tempvar5")] <- paste0("reasons_overview2_A_",stype[vc])
    names(dat)[which(names(dat) == "tempvar6")] <- paste0("reasons_overview2_B_",stype[vc])
    names(dat)[which(names(dat) == "tempvar7")] <- paste0("reasons_overview2_C_",stype[vc])
    names(dat)[which(names(dat) == "tempvar8")] <- paste0("reasons_overview2_D_",stype[vc])
  } # end of stype vc loop

  # Make variable "ES08AA_1A_03_12":
  #   For block A "Reasons related to health workers" option #3 is child too
  #   sick to receive dose. If this is chosen, then a reason/symptom should
  #   be provided (e.g., fever).  This variable is "missing" and so is set
  #   to 1 if a reason/symptom is not provided.

  dat <- dat %>% mutate(ES08AA_1A_03_12 = ifelse(ES08AA %in% 2, 0, NA)) %>%
    relocate(ES08AA_1A_03_12, .after = ES08AA_1A_03_11) %>%
    mutate(ES08AA_1A_03_12 = ifelse((ES08AA_1A_03==1 & (ES08AA_1A_03_01==1 | ES08AA_1A_03_02==1 | ES08AA_1A_03_03==1 | ES08AA_1A_03_04==1 |
                                                          ES08AA_1A_03_05==1 | ES08AA_1A_03_06==1 | ES08AA_1A_03_07==1 | ES08AA_1A_03_08==1 |
                                                          ES08AA_1A_03_09==1 | ES08AA_1A_03_10==1 | ES08AA_1A_03_11==1)) %in% TRUE,
                                    0, ES08AA_1A_03_12)) %>%
    mutate(ES08AA_1A_03_12 = ifelse((ES08AA_1A_03==1 & (is.na(ES08AA_1A_03_01) & is.na(ES08AA_1A_03_02) & is.na(ES08AA_1A_03_03) &
                                                          is.na(ES08AA_1A_03_04) & is.na(ES08AA_1A_03_05) & is.na(ES08AA_1A_03_06) &
                                                          is.na(ES08AA_1A_03_07) & is.na(ES08AA_1A_03_08) & is.na(ES08AA_1A_03_09) &
                                                          is.na(ES08AA_1A_03_10) & is.na(ES08AA_1A_03_11))) %in% TRUE, 1,ES08AA_1A_03_12))

  # Sick Child : Did not provide a reason/symptom
  dat$ES08AA_1A_03_12 <- haven::labelled(dat$ES08AA_1A_03_12,
                                         label = paste0(language_string(language_use = language_use, str = "OS_382"),
                                                        ": ",
                                                        language_string(language_use = language_use, str = "OS_383")))
  # Create separate variables for each ES08AA_* var that breaks apart each by
  # eligible and ineligible MKT Added 2018-1-11
  # Added condition to clone if the correct reasons_overview was selected 2018-05-09 MKT
  # This ensures the correct number is included in denominator

  for (vc in seq_along(stype)){
    varlist1 <- c("ES08AA_1A_01", "ES08AA_1A_02", "ES08AA_1A_03_01", "ES08AA_1A_03_02",
                   "ES08AA_1A_03_03", "ES08AA_1A_03_04", "ES08AA_1A_03_05", "ES08AA_1A_03_06",
                   "ES08AA_1A_03_07", "ES08AA_1A_03_08", "ES08AA_1A_03_09", "ES08AA_1A_03_10",
                   "ES08AA_1A_03_11", "ES08AA_1A_03_12")

    for (v in seq_along(varlist1)){
      var <- rlang::sym(varlist1[v])
      O2A <- rlang::sym(paste0("reasons_overview2_A_",stype[vc]))
      O1A <- rlang::sym(paste0("reasons_overview1_A_",stype[vc]))
      ei  <- rlang::sym(paste0("elig_indicator_",stype[vc]))

      var_dat <- get(varlist1[v],dat)
      varlabel <- attr(var_dat,"label")

      dat <- dat %>% mutate(tempvar1 = ifelse(!!O2A %in% 1, !!var, NA)) %>%
        mutate(tempvar1 = ifelse(is.na(!!var) & !!O2A %in% 1, 0, tempvar1)) %>%
        mutate(tempvar1 = ifelse(!(!!ei) %in% 1, NA, tempvar1))

      dat <- dat %>% mutate(tempvar2 = ifelse(!!O1A %in% 1, !!var, NA)) %>%
        mutate(tempvar2 = ifelse(is.na(!!var) & !!O1A %in% 1, 0, tempvar2)) %>%
        mutate(tempvar2 = ifelse(!(!!ei) %in% 0, NA, tempvar2))

      dat$tempvar1 <- haven::labelled(dat$tempvar1,label = varlabel)
      dat$tempvar2 <- haven::labelled(dat$tempvar2,label = varlabel)

      names(dat)[which(names(dat) == "tempvar1")] <- paste0(varlist1[v],"_eligible_",stype[vc])
      names(dat)[which(names(dat) == "tempvar2")] <- paste0(varlist1[v],"_ineligible_",stype[vc])
    } # end of varlist1 v loop

    varlist2 <- c("ES08AA_1B_01", "ES08AA_1B_02", "ES08AA_1B_03", "ES08AA_1B_04", "ES08AA_1B_05",
                    "ES08AA_1B_06", "ES08AA_1B_07", "ES08AA_1B_08", "ES08AA_1B_09", "ES08AA_1B_10",
                    "ES08AA_1B_11", "ES08AA_1B_12")

    for (v in seq_along(varlist2)){
      var <- rlang::sym(varlist2[v])
      O2B <- rlang::sym(paste0("reasons_overview2_B_",stype[vc]))
      O1B <- rlang::sym(paste0("reasons_overview1_B_",stype[vc]))
      ei  <- rlang::sym(paste0("elig_indicator_",stype[vc]))

      var_dat <- get(varlist2[v],dat)
      varlabel <- attr(var_dat,"label")

      dat <- dat %>% mutate(tempvar1 = ifelse(!!O2B %in% 1, !!var, NA)) %>%
        mutate(tempvar1 = ifelse(is.na(!!var) & !!O2B %in% 1, 0, tempvar1)) %>%
        mutate(tempvar1 = ifelse(!(!!ei) %in% 1, NA, tempvar1))

      dat <- dat %>% mutate(tempvar2 = ifelse(!!O1B %in% 1, !!var, NA)) %>%
        mutate(tempvar2 = ifelse(is.na(!!var) & !!O1B %in% 1, 0, tempvar2)) %>%
        mutate(tempvar2 = ifelse(!(!!ei) %in% 0, NA, tempvar2))

      dat$tempvar1 <- haven::labelled(dat$tempvar1,label = varlabel)
      dat$tempvar2 <- haven::labelled(dat$tempvar2,label = varlabel)

      names(dat)[which(names(dat) == "tempvar1")] <- paste0(varlist2[v],"_eligible_",stype[vc])
      names(dat)[which(names(dat) == "tempvar2")] <- paste0(varlist2[v],"_ineligible_",stype[vc])
    } # end of varlist2 v loop

    varlist3 <- c("ES08AA_1C_01", "ES08AA_1C_02", "ES08AA_1C_03", "ES08AA_1C_04", "ES08AA_1C_05",
                    "ES08AA_1C_06", "ES08AA_1C_07", "ES08AA_1C_08", "ES08AA_1C_09")

    for (v in seq_along(varlist3)){
      var <- rlang::sym(varlist3[v])
      O2C <- rlang::sym(paste0("reasons_overview2_C_",stype[vc]))
      O1C <- rlang::sym(paste0("reasons_overview1_C_",stype[vc]))
      ei  <- rlang::sym(paste0("elig_indicator_",stype[vc]))

      var_dat <- get(varlist3[v],dat)
      varlabel <- attr(var_dat,"label")

      dat <- dat %>% mutate(tempvar1 = ifelse(!!O2C %in% 1, !!var, NA)) %>%
        mutate(tempvar1 = ifelse(is.na(!!var) & !!O2C %in% 1, 0, tempvar1)) %>%
        mutate(tempvar1 = ifelse(!(!!ei) %in% 1, NA, tempvar1))

      dat <- dat %>% mutate(tempvar2 = ifelse(!!O1C %in% 1, !!var, NA)) %>%
        mutate(tempvar2 = ifelse(is.na(!!var) & !!O1C %in% 1, 0, tempvar2)) %>%
        mutate(tempvar2 = ifelse(!(!!ei) %in% 0, NA, tempvar2))

      dat$tempvar1 <- haven::labelled(dat$tempvar1,label = varlabel)
      dat$tempvar2 <- haven::labelled(dat$tempvar2,label = varlabel)

      names(dat)[which(names(dat) == "tempvar1")] <- paste0(varlist3[v],"_eligible_",stype[vc])
      names(dat)[which(names(dat) == "tempvar2")] <- paste0(varlist3[v],"_ineligible_",stype[vc])
    } # end of varlist3 v loop

  } # end of stype vc loop

  # NOTE: No other derived variables need to be created for tables "MOV reasons #A/B/C"
  #       because variables from original dataset are already indicator variables
  #       and can be used to make those tables
  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/MOV_dvs.rds"))

  if (!vcqi_object_exists("RI_TEMP_DATASETS")){
    RI_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS,"MOV_dvs"))

  # ************************************************************************
  #
  # Assemble a dataset to serve as input to an R Shiny
  # ES MOV visualization.  Use a combination of table5_dv* and
  # flags from when we ran calculate_previsit_MOV_flags
  #
  # Save the results in a dataset named ES_R_Shiny_inputs

  # If the user has generated flags for the previsit MOVs, then
  # generate this dataset; otherwise skip this section of code.

  if (file.exists(paste0(VCQI_OUTPUT_FOLDER,"/RI_previsit_MOV_flags_to_merge.rds"))){
    dat <- dat %>% select(respid, all_of(starts_with("table5_dv")))
    dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_previsit_MOV_flags_to_merge.rds"))
    dat <- left_join(dat, dat2, by = "respid")

    dat3 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))
    vartokeep <- grep(glob2rx("level*id"), names(dat3), value=TRUE)
    vartokeep <- c(vartokeep, grep(glob2rx("level*name"), names(dat3), value=TRUE))
    dat3 <- dat3 %>% select(c(respid,all_of(vartokeep)))
    dat <- inner_join(dat, dat3, by = "respid")

    # Manufacture level3name and level1name if missing
    level3name_exists <- "level3name" %in% names(dat)
    level3id_exists <- "level3id" %in% names(dat)
    level1name_exists <- "level1name" %in% names(dat)
    level1id_exists <- "level1id" %in% names(dat)

    if ((level3name_exists == FALSE & level3id_exists == TRUE) %in% TRUE){
      dat <- dat %>% mutate(level3name = as.character(level3id))
    }
    if ((level1name_exists == FALSE & level1id_exists == TRUE) %in% TRUE){
      dat <- dat %>% mutate(level1name = as.character(level1id))
    }

    # Construct a new set of variables
    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      for (vc in seq_along(stype)){
        valid <- rlang::sym(paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_valid_",stype[vc]))
        ucor_mov <- rlang::sym(paste0("flag_uncor_mov_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc]))
        dmov <- rlang::sym(paste0("table5_dv_",MOV_OUTPUT_DOSE_LIST[d],"_mov_",stype[vc]))

        dat <- dat %>% mutate(tempvar1 = 0) %>% #not eligible for this dose today
          mutate(tempvar1 = ifelse((!!valid == 1 & table5_dv_had_card == 1) %in% TRUE, 1, tempvar1)) %>% # had a valid dose at first opp
          mutate(tempvar1 = ifelse((tempvar1 == 1 & !!ucor_mov == 1 & table5_dv_had_card == 1) %in% TRUE,
                                   2, tempvar1)) %>% # corrected an earlier MOV
          mutate(tempvar1 = ifelse((!!dmov == 1 & table5_dv_had_card == 1) %in% TRUE, 3, tempvar1)) # experienced an MOV

        dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                        label = paste0("Study day outcome for ",MOV_OUTPUT_DOSE_LIST[d]," - ",stype[vc]),
                                        labels = c("Not eligible" = 0, "Received - first opportunity" = 1,
                                                   "Received - corrected earlier MOV" = 2, "Experienced MOV" = 3))

        names(dat)[which(names(dat) == "tempvar1")] <- paste0("table5_shiny_",MOV_OUTPUT_DOSE_LIST[d],"_",stype[vc])
      } #end of stype vc loop
    } #end of MOV_OUTPUT_DOSE_LIST d loop

    dat <- dat %>% select(-c(all_of(starts_with("table5_dv"))))

    saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/ES_R_Shiny_inputs.rds"))
    # Do NOT add ES_R_Shiny_inputs to the list of RI_TEMP_DATASETS because we ALWAYS want to save it
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
