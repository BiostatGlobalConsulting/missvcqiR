#' Clean up RI analysis related dates and set ticks
#'
#' This program accomplishes several things:
#'
#' - It merges the RI and RIHC datasets (if RIHC records were sought)
#'
#' - It checks to be sure there are date and tick variables for each dose in the
#' RI_DOSE_LIST (both in the card and register datasets)
#'
#' - If RIHC records were not sought, it puts empty register variables in the
#' dataset.
#'
#' - It assigns a dob_for_valid_dose_calculations, if there are m, d, and y data
#' elements in the dob from card, history, or register.
#'
#' - It looks at the vaccination dates on the cards and in the register,
#' checking to see that they occurred a) after the child's birthdate, if known,
#' b) after the earliest possible birthdate in the survey if the child's dob is
#' not known, c) before the survey began, and d) if a dose in a series, it
#' checks to be sure that sequential doses occur in chronological order.
#'
#' - Where it finds a vaccine date on card or register that does not have these
#' properties, it sets the date to missing and marks a tick mark instead, so the
#' child will get credit for having the dose, but the nonsensical date will not
#' be passed into the measures that interpret vaccination dates.
#'
#' - Finally it merges the new dataset containing clean dates and ticks with the
#' full RI dataset.
#'
#' The dataset that comes from this program will need unique IDs and then it
#' should have everything needed to calculate the RI analyses.
#'
#' @param VCP VCQI current program name to be logged, default to be the function
#'   name
#'
#' @return Three datasets in VCQI_OUTPUT_FOLDER: <VCQI_RI_DATASET>_clean,
#'   <VCQI_RI_DATASET>_dqd, <VCQI_RI_DATASET>_dq_flags
#'
#' @export
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import tidyselect
#' @import tidyr
#' @import stringr
#' @rawNamespace import(tools, except = makevars_user)
#' @import haven
#'
#' @examples
#' cleanup_RI_dates_and_ticks()

# cleanup_RI_dates_and_ticks R version 1.11 - Biostat Global Consulting - 2023-08-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-06  1.00      Mia Yu          Original R version
# 2022-07-08  1.01      Caitlin Clary   Updates to code flow, add multi-dose order
#                                       checks
# 2022-07-22  1.02      Mia Yu          Updates the way merging the new clean
#                                       variables on to the RI survey dataset
# 2022-07-25  1.03      Mia Yu          Bug fixed
# 2022-08-01  1.04      Caitlin Clary   Multi-dose order checks: update to check
#                                       all earlier dose-later dose pairs
# 2022-08-05  1.05      Mia Yu          Fixes the bug that when non date can be used
#                                       the max/min function return -Inf
# 2022-09-22  1.06      Mia Yu          Fix single_birthdate NA problem
# 2022-10-04  1.07      Mia Yu          Fix flag22 problem
# 2022-10-05  1.08      Mia Yu          Package version
# 2022-10-18  1.09      Caitlin Clary   Added vcqi_halt_immediately call
# 2022-10-19  1.10      Mia Yu          Add variable labels and VCQI_DOB_PREFER_DOC part
# 2023-08-21  1.11      Caitlin Clary   Fill holes in history evidence
# *******************************************************************************

cleanup_RI_dates_and_ticks <- function(VCP = "cleanup_RI_dates_and_ticks"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){

    # Use VCQI_RI_DATASET as defined at the end of check_interview_date
    # Either _preclean or the original RI dataset - either way, saved in VCQI_OUTPUT_FOLDER
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET))

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){

      dat <- mutate(dat, RIHC01 = RI01, RIHC03 = RI03, RIHC14 = RI11, RIHC15 = RI12)

      # Left join with RIHC dataset
      dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_RIHC_DATASET))
      dat2 <- select(dat2, RIHC01, RIHC03, RIHC14, RIHC15,
                     ends_with("date_register_d"),
                     ends_with("date_register_m"),
                     ends_with("date_register_y"),
                     ends_with("tick_register"))

      dat <- left_join(dat, dat2, by = c("RIHC01", "RIHC03", "RIHC14", "RIHC15"))

      exitflag <- 0
      errormsgs <- NULL

      type <- c("card", "register")
      varlist <- c("dob", RI_DOSE_LIST)

      for(s in seq_along(type)){
        for(d in seq_along(varlist)){

          # Check if all date variables exist
          vardname <- paste0(varlist[d], "_date_", type[s], "_d")
          if(!(vardname %in% names(dat))){
            exitflag <- 1
            errormsgs <- c(errormsgs, paste0("cleanup_RI_dates_and_ticks: Expected to find ",
                                             vardname, " in the RI dataset"))
            errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_FOR_ALL is ",
                                             RI_RECORDS_SOUGHT_FOR_ALL))
            errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_IF_NO_CARD is ",
                                             RI_RECORDS_SOUGHT_IF_NO_CARD))
          }

          # Check if all month variables exist
          varmname <- paste0(varlist[d], "_date_", type[s], "_m")
          if(!(varmname %in% names(dat))){
            exitflag <- 1
            errormsgs <- c(errormsgs)
            errormsgs <- c(errormsgs, paste0("cleanup_RI_dates_and_ticks: Expected to find ",
                                             varmname, " in the RI dataset"))
            errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_FOR_ALL is ",
                                             RI_RECORDS_SOUGHT_FOR_ALL))
            errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_IF_NO_CARD is ",
                                             RI_RECORDS_SOUGHT_IF_NO_CARD))
          }

          # Check if all year variables exist
          varyname <- paste0(varlist[d], "_date_", type[s], "_y")
          if(!(varyname %in% names(dat))){
            exitflag <- 1
            errormsgs <- c(errormsgs)
            errormsgs <- c(errormsgs, paste0("cleanup_RI_dates_and_ticks: Expected to find ",
                                             varyname, " in the RI dataset"))
            errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_FOR_ALL is ", RI_RECORDS_SOUGHT_FOR_ALL))
            errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_IF_NO_CARD is ", RI_RECORDS_SOUGHT_IF_NO_CARD))
          }

          for (l in seq_along(RI_DOSE_LIST)){
            tickname <- paste0(RI_DOSE_LIST[l],"_tick_",type[s])
            if(!(tickname %in% names(dat))){
              exitflag <- 1
              errormsgs <- c(errormsgs, paste0("cleanup_RI_dates_and_ticks: Expected to find ",tickname," in the RI dataset"))
              errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_FOR_ALL is ", RI_RECORDS_SOUGHT_FOR_ALL))
              errormsgs <- c(errormsgs, paste0("RI_RECORDS_SOUGHT_IF_NO_CARD is ", RI_RECORDS_SOUGHT_IF_NO_CARD))
            }
          }

        }
      }

      if(exitflag == 1){
        vcqi_global(VCQI_ERROR, 1)
        vcqi_halt_immediately(
          halt_message = errormsgs
        )
      }
    }

    # Create blank register variables if register data was not sought
    if(RI_RECORDS_NOT_SOUGHT == 1){
      type <- c("card")
      for(d in seq_along(RI_DOSE_LIST)){

        dat <- dat %>%
          mutate(
            !! paste0(RI_DOSE_LIST[d], "_register_tick") := NA,
            !! paste0(RI_DOSE_LIST[d], "_register_date") := NA,
            !! paste0(RI_DOSE_LIST[d], "_date_register_m") := NA,
            !! paste0(RI_DOSE_LIST[d], "_date_register_d") := NA,
            !! paste0(RI_DOSE_LIST[d], "_date_register_y") := NA
          )
      }

      dat <- mutate(dat,
                    dob_date_register_m = NA,
                    dob_date_register_d = NA,
                    dob_date_register_y = NA,
                    dob_register_date = NA)
    }

    # ********************************************************************************
    # ********************************************************************************
    # Identify respondents with an unambiguous date of birth and assign it for
    # valid dose calculations

    # If there are several values specified for dob (across card, history, and register)
    # then use the earliest one (because it will yield the highest count of valid vaccination dates)
    # ********************************************************************************
    # ********************************************************************************

    #set VCQI_DOB_PREFER_DOC default value if not specified
    if (!vcqi_object_exists("VCQI_DOB_PREFER_DOC")){
      vcqi_global(VCQI_DOB_PREFER_DOC, 0)
    }

    if (VCQI_DOB_PREFER_DOC %in% 1){
      # Is there a single m, single d, and single y specified across all three
      # sources?  Note that the m, d, and y can come from different sources...but
      # if they comprise together a valid date, let's use it.

      t <- nrow(dat)

      dat <- dat %>%
        rowwise() %>%
        mutate(
          maxmonth = max(dob_date_card_m, dob_date_history_m, dob_date_register_m, na.rm = TRUE),
          minmonth = min(dob_date_card_m, dob_date_history_m, dob_date_register_m, na.rm = TRUE),

          maxdate = max(dob_date_card_d, dob_date_history_d, dob_date_register_d, na.rm = TRUE),
          mindate = min(dob_date_card_d, dob_date_history_d, dob_date_register_d, na.rm = TRUE),

          maxyear = max(dob_date_card_y, dob_date_history_y, dob_date_register_y, na.rm = TRUE),
          minyear = min(dob_date_card_y, dob_date_history_y, dob_date_register_y, na.rm = TRUE)) %>%
        ungroup() %>% suppressWarnings()

      dat <- dat %>% mutate(maxmonth = ifelse((maxmonth == Inf | maxmonth == -Inf) %in% TRUE, NA, maxmonth),
                            minmonth = ifelse((minmonth == Inf | minmonth == -Inf) %in% TRUE, NA, minmonth),
                            maxdate = ifelse((maxdate == Inf | maxdate == -Inf) %in% TRUE, NA, maxdate),
                            mindate = ifelse((mindate == Inf | mindate == -Inf) %in% TRUE, NA, mindate),
                            maxyear = ifelse((maxyear == Inf | maxyear == -Inf) %in% TRUE, NA, maxyear),
                            minyear = ifelse((minyear == Inf | minyear == -Inf) %in% TRUE, NA, minyear))

      dat <- dat %>% mutate(single_birthdate = case_when(
        (maxmonth != minmonth | maxdate != mindate | maxyear != minyear) ~ 0,
        (is.na(maxmonth) | is.na(maxdate) | is.na(maxyear)) ~ 0,
        TRUE ~ 1))

      #If yes, use that value
      dat <- dat %>% mutate(dob_for_valid_dose_calculations = if_else(
        single_birthdate %in% 1, vcqi_mdy(maxmonth, maxdate, maxyear), NA_Date_
      ))

      #Drop the value if it is outside the range of valid birth dates
      earliest <- vcqi_mdy(EARLIEST_SVY_VACC_DATE_M, EARLIEST_SVY_VACC_DATE_D, EARLIEST_SVY_VACC_DATE_Y)
      latest <- vcqi_mdy(LATEST_SVY_VACC_DATE_M, LATEST_SVY_VACC_DATE_D, LATEST_SVY_VACC_DATE_Y)

      dat <- dat %>%
        mutate(
          dob_for_valid_dose_calculations = if_else((dob_for_valid_dose_calculations < earliest |  dob_for_valid_dose_calculations > latest) %in% TRUE, NA_Date_, dob_for_valid_dose_calculations)
        ) %>%
        # Drop unneeded variables
        select(-c(maxmonth, maxdate, maxyear, minmonth, mindate, minyear))

      subdat <- subset(dat, !is.na(dob_for_valid_dose_calculations))
      count1 <- nrow(subdat)

      # If not a single date, prefer the card
      dat <- dat %>% mutate(dob_for_valid_dose_calculations = if_else(
        is.na(dob_for_valid_dose_calculations),
        vcqi_mdy(dob_date_card_m, dob_date_card_d, dob_date_card_y),
        dob_for_valid_dose_calculations
      )) %>% suppressWarnings()

      #Drop the value if it is outside the range of valid birth dates
      dat <- dat %>%
        mutate(
          dob_for_valid_dose_calculations = if_else((dob_for_valid_dose_calculations < earliest |  dob_for_valid_dose_calculations > latest) %in% TRUE, NA_Date_, dob_for_valid_dose_calculations)
        )

      subdat <- subset(dat, !is.na(dob_for_valid_dose_calculations))
      count2 <- nrow(subdat)

      # If not from card, prefer registry
      dat <- dat %>% mutate(dob_for_valid_dose_calculations = if_else(
        is.na(dob_for_valid_dose_calculations),
        vcqi_mdy(dob_date_register_m, dob_date_register_d, dob_date_register_y),
        dob_for_valid_dose_calculations
      )) %>% suppressWarnings()

      #Drop the value if it is outside the range of valid birth dates
      dat <- dat %>%
        mutate(
          dob_for_valid_dose_calculations = if_else((dob_for_valid_dose_calculations < earliest |  dob_for_valid_dose_calculations > latest) %in% TRUE, NA_Date_, dob_for_valid_dose_calculations)
        )

      subdat <- subset(dat, !is.na(dob_for_valid_dose_calculations))
      count3 <- nrow(subdat)

      # Use recall if card and registry do not work

      dat <- dat %>% mutate(dob_for_valid_dose_calculations = if_else(
        is.na(dob_for_valid_dose_calculations),
        vcqi_mdy(dob_date_history_m, dob_date_history_d, dob_date_history_y),
        dob_for_valid_dose_calculations
      )) %>% suppressWarnings()

      #Drop the value if it is outside the range of valid birth dates
      dat <- dat %>%
        mutate(
          dob_for_valid_dose_calculations = if_else((dob_for_valid_dose_calculations < earliest |  dob_for_valid_dose_calculations > latest) %in% TRUE, NA_Date_, dob_for_valid_dose_calculations)
        )

      subdat <- subset(dat, !is.na(dob_for_valid_dose_calculations))
      count4 <- nrow(subdat)

      vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", t, " children in the survey, ", count1, " had a valid unambiguious date of birth between card, register and history"))
      vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", t, " children in the survey, ", count2 - count1, " had a valid date of birth preferred from card."))
      vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", t, " children in the survey, ", count3 - count2, " had a valid date of birth preferred from registry."))
      vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", t, " children in the survey, ", count4 - count3, " had a valid date of birth set from recall (history)."))

      # Otherwise, use the earliest plausible dob from card, register, or history
      temptype <- c("card", "register", "history")

      for(s in seq_along(temptype)){
        dobvar1 <- rlang::sym(paste0("dob_date_",temptype[s],"_m"))
        dobvar2 <- rlang::sym(paste0("dob_date_",temptype[s],"_d"))
        dobvar3 <- rlang::sym(paste0("dob_date_",temptype[s],"_y"))

        dat <- dat %>%
          mutate(tempvar1 = suppressWarnings(vcqi_mdy(!!dobvar1, !!dobvar2, !!dobvar3)),
                 !!paste0("dob_", temptype[s]) := if_else((tempvar1 < earliest | tempvar1 > (latest - VCQI_RI_MIN_AGE_OF_ELIGIBILITY)) %in% TRUE, NA_Date_, tempvar1)) %>%
          select(-tempvar1)
      }

      dat <- dat %>%
        rowwise() %>%
        # use suppressWarnings() because if_else() works by evaluating both the value to return when the condition is met and the value to return when the condition is not met.
        mutate(mindob = if_else((is.na(dob_card) & is.na(dob_history) & is.na(dob_register)) %in% TRUE, NA_Date_, suppressWarnings(min(dob_card, dob_history, dob_register, na.rm = TRUE)))) %>%
        ungroup() %>%
        mutate(
          plausible_birthdate = ifelse((is.na(dob_for_valid_dose_calculations) & !is.na(mindob)) %in% TRUE, 1, NA),
        ) %>%
        select(-mindob)
    } else{
      # Is there a single m, single d, and single y specified across all three
      # sources?  Note that the m, d, and y can come from different sources...but
      # if they comprise together a valid date, let's use it.

      dat <- dat %>%
        rowwise() %>%
        mutate(
          maxmonth = max(dob_date_card_m, dob_date_history_m, dob_date_register_m, na.rm = TRUE),
          minmonth = min(dob_date_card_m, dob_date_history_m, dob_date_register_m, na.rm = TRUE),

          maxdate = max(dob_date_card_d, dob_date_history_d, dob_date_register_d, na.rm = TRUE),
          mindate = min(dob_date_card_d, dob_date_history_d, dob_date_register_d, na.rm = TRUE),

          maxyear = max(dob_date_card_y, dob_date_history_y, dob_date_register_y, na.rm = TRUE),
          minyear = min(dob_date_card_y, dob_date_history_y, dob_date_register_y, na.rm = TRUE)) %>%
        ungroup() %>% suppressWarnings()

      dat <- dat %>% mutate(maxmonth = ifelse((maxmonth == Inf | maxmonth == -Inf) %in% TRUE, NA, maxmonth),
                            minmonth = ifelse((minmonth == Inf | minmonth == -Inf) %in% TRUE, NA, minmonth),
                            maxdate = ifelse((maxdate == Inf | maxdate == -Inf) %in% TRUE, NA, maxdate),
                            mindate = ifelse((mindate == Inf | mindate == -Inf) %in% TRUE, NA, mindate),
                            maxyear = ifelse((maxyear == Inf | maxyear == -Inf) %in% TRUE, NA, maxyear),
                            minyear = ifelse((minyear == Inf | minyear == -Inf) %in% TRUE, NA, minyear))

      dat <- dat %>% mutate(single_birthdate = case_when(
        (maxmonth != minmonth | maxdate != mindate | maxyear != minyear) ~ 0,
        (is.na(maxmonth) | is.na(maxdate) | is.na(maxyear)) ~ 0,
        TRUE ~ 1))

      #If yes, use that value
      dat <- dat %>% mutate(dob_for_valid_dose_calculations = if_else(
        single_birthdate %in% 1, vcqi_mdy(maxmonth, maxdate, maxyear), NA_Date_
      ))

      #Drop the value if it is outside the range of valid birth dates
      earliest <- vcqi_mdy(EARLIEST_SVY_VACC_DATE_M, EARLIEST_SVY_VACC_DATE_D, EARLIEST_SVY_VACC_DATE_Y)
      latest <- vcqi_mdy(LATEST_SVY_VACC_DATE_M, LATEST_SVY_VACC_DATE_D, LATEST_SVY_VACC_DATE_Y)

      dat <- dat %>%
        mutate(
          single_birthdate = if_else(!((dob_for_valid_dose_calculations < earliest | dob_for_valid_dose_calculations > latest) %in% FALSE), 0, single_birthdate),
          dob_for_valid_dose_calculations = if_else((dob_for_valid_dose_calculations < earliest |  dob_for_valid_dose_calculations > latest) %in% TRUE, NA_Date_, dob_for_valid_dose_calculations)
        ) %>%
        # Drop unneeded variables
        select(-c(maxmonth, maxdate, maxyear, minmonth, mindate, minyear))

      vcqi_log_comment(VCP, 4, "Data",
                       paste0("Of the ", nrow(dat), " children in the survey, ",
                              length(which(dat$single_birthdate == 1)),
                              " had a valid unambiguous date of birth between card, register and history."))

      temptype <- c("card", "register", "history")

      for(s in seq_along(c("card", "register", "history"))){
        dobvar1 <- rlang::sym(paste0("dob_date_",temptype[s],"_m"))
        dobvar2 <- rlang::sym(paste0("dob_date_",temptype[s],"_d"))
        dobvar3 <- rlang::sym(paste0("dob_date_",temptype[s],"_y"))

        dat <- dat %>%
          mutate(tempvar1 = suppressWarnings(vcqi_mdy(!!dobvar1, !!dobvar2, !!dobvar3)),
                 !!paste0("dob_", temptype[s]) := if_else((tempvar1 < earliest | tempvar1 > (latest - VCQI_RI_MIN_AGE_OF_ELIGIBILITY)) %in% TRUE, NA_Date_, tempvar1)) %>%
          select(-tempvar1)
      }

      dat <- dat %>%
        rowwise() %>%
        # use suppressWarnings() because if_else() works by evaluating both the value to return when the condition is met and the value to return when the condition is not met.
        mutate(
          mindob = if_else(
            (is.na(dob_card) & is.na(dob_history) & is.na(dob_register)) %in% TRUE,
            NA_Date_,
            suppressWarnings(min(dob_card, dob_history, dob_register, na.rm = TRUE)))
        ) %>%
        ungroup() %>%
        mutate(
          plausible_birthdate = ifelse(
            (is.na(dob_for_valid_dose_calculations) & !is.na(mindob)) %in% TRUE,
            1, NA),
          dob_for_valid_dose_calculations = if_else(
            is.na(dob_for_valid_dose_calculations),
            mindob, dob_for_valid_dose_calculations)
        ) %>%
        select(-mindob)

      t2 <- length(which(dat$single_birthdate == 0 & is.na(dat$dob_for_valid_dose_calculations)))
      vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", nrow(dat), " children in the survey, ", t2, " used the earliest valid birth date provided from card, register and history"))

      t2 <- length(which(is.na(dat$dob_for_valid_dose_calculations)))
      vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", nrow(dat), " children in the survey, ", t2, " did not have a valid dob on card, register or history."))

    }

    # ************************************************************************
    # Calculate age at time of survey

    # Age is calculated using four steps

    # If the respondent has DOB and a date of the interview
    # then we use those two dates to calculate age

    # Else if they have dob and no date of interview then we use the
    # LATEST_SVY_VACC_DATE as the date of the interview

    # Else if they have no dob then we use completed age in month * 30.4

    # Else if they have only completed years then we use that * 365

    dat <- dat %>% mutate(age_at_interview = NA, date_of_interview = NA) %>%
      mutate(date_of_interview = vcqi_mdy(RI09_m, RI09_d, RI09_y)) %>%
      mutate(age_at_interview = ifelse(
        is.na(age_at_interview),
        date_of_interview - dob_for_valid_dose_calculations,
        age_at_interview
      )) %>%
      mutate(date_of_last_possible_vacc = latest) %>%
      mutate(age_at_interview = ifelse(
        is.na(age_at_interview),
        date_of_last_possible_vacc - dob_for_valid_dose_calculations,
        age_at_interview
      )) %>% suppressWarnings()

    if("RI25" %in% names(dat)){
      dat <- mutate(dat, age_at_interview = ifelse(is.na(age_at_interview),
                                                   round(RI25*30.4),
                                                   age_at_interview))
    }

    if("RI24" %in% names(dat)){
      dat <- mutate(dat, age_at_interview = ifelse(is.na(age_at_interview),
                                                   round(RI24 * 365.25),
                                                   age_at_interview))
    }

    # Create variables to determine the following:
    # FLAGs 01-19 require date
    # flag01-missing month
    # flag02-missing day
    # flag03-missing year
    # flag04-missing only day
    # flag05-missing all (month day and year)
    # flag06-missing any (month or day or year)
    # flag07-nonsense date(all components but mdy function results in missing)
    #
    # FLAGs 20-50 based on final date from flags 01-07
    # flag20-dose date before earliest possible dob in survey
    # flag21-dose date before dob
    # flag22-dose date after survey date
    #
    #
    # flag00-one or more of the above flags is set to 1

    # Logical statements to create flags 01-07
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        monthvar <- rlang::sym(paste0(RI_DOSE_LIST[v],"_date_", type[s], "_m"))
        datevar  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_date_", type[s], "_d"))
        yearvar  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_date_", type[s], "_y"))

        dat <- dat %>%
          mutate(
            # Flag 1: missing month
            tempvar1 = ifelse(is.na(!!monthvar), 1, 0),
            # Flag 2: missing day
            tempvar2 = ifelse(is.na(!!datevar), 1, 0),
            # Flag 3: missing year
            tempvar3 = ifelse(is.na(!!yearvar), 1, 0),
            # Flag 4: missing *only* day
            tempvar4 = ifelse(tempvar1 %in% 0 & tempvar2 %in% 1 & tempvar3 %in% 0, 1, 0),
            # Flag 5: missing complete date
            tempvar5 = ifelse(tempvar1 %in% 1 & tempvar2 %in% 1 & tempvar3 %in% 1, 1, 0),
            # Flag 6: missing any date component
            tempvar6 = ifelse(tempvar1 %in% 1 | tempvar2 %in% 1 | tempvar3 %in% 1, 1, 0),
            # Flag 7: nonsense date
            tempvar7 = ifelse(tempvar6 %in% FALSE &
                                suppressWarnings(is.na(vcqi_mdy(!!monthvar, !!datevar, !!yearvar)
                                )) %in% TRUE, 1, 0),
            # Check (temporary): any of flags 1-7 set?
            tempvar8 = ifelse(tempvar1 %in% 1 | tempvar2 %in% 1 | tempvar3 %in% 1 |
                                tempvar4 %in% 1 | tempvar5 %in% 1 | tempvar6 %in% 1 |
                                tempvar7 %in% 1, TRUE, FALSE)
          )

        dat$tempvar1 <- haven::labelled(
          dat$tempvar1,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -Missing Month")) %>%
          suppressWarnings()

        dat$tempvar2 <- haven::labelled(
          dat$tempvar2,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -Missing Day")) %>%
          suppressWarnings()

        dat$tempvar3 <- haven::labelled(
          dat$tempvar3,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -Missing Year")) %>%
          suppressWarnings()

        dat$tempvar4 <- haven::labelled(
          dat$tempvar4,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -Missing Day Only")) %>%
          suppressWarnings()

        dat$tempvar5 <- haven::labelled(
          dat$tempvar5,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -Missing Complete Date")) %>%
          suppressWarnings()

        dat$tempvar6 <- haven::labelled(
          dat$tempvar6,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -Missing Any Date Component")) %>%
          suppressWarnings()

        dat$tempvar7 <- haven::labelled(
          dat$tempvar7,
          label = paste0(RI_DOSE_LIST[v], " date, ", type[s],
                         " -All date components result to nonsense date")) %>%
          suppressWarnings()

        names(dat)[which(names(dat) == "tempvar1")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag01")
        names(dat)[which(names(dat) == "tempvar2")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag02")
        names(dat)[which(names(dat) == "tempvar3")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag03")
        names(dat)[which(names(dat) == "tempvar4")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag04")
        names(dat)[which(names(dat) == "tempvar5")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag05")
        names(dat)[which(names(dat) == "tempvar6")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag06")
        names(dat)[which(names(dat) == "tempvar7")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag07")
        names(dat)[which(names(dat) == "tempvar8")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_flagsum01")

      }
    }

    #Logical statement for DOB

    # Creating new full dose date variable if flags01-07 equal zero

    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        monthvar <- rlang::sym(paste0(RI_DOSE_LIST[v],"_date_",type[s],"_m"))
        datevar  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_date_",type[s],"_d"))
        yearvar  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_date_",type[s],"_y"))
        flagsum  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_flagsum01"))

        dat <- dat %>%
          mutate(!!paste0(RI_DOSE_LIST[v],"_",type[s],"_date") := if_else(
            !!flagsum %in% FALSE, suppressWarnings(vcqi_mdy(!!monthvar, !!datevar, !!yearvar)), NA_Date_))
      }
    }

    ## Generate DOB variables
    dat <- dat %>%
      # Generate history dob in one variable for flag21
      mutate(
        dob_date_history = suppressWarnings(vcqi_mdy(dob_date_history_m,
                                                     dob_date_history_d,
                                                     dob_date_history_y)),
        # Generate card dob in one variable
        dob_date_card = suppressWarnings(vcqi_mdy(dob_date_card_m, dob_date_card_d, dob_date_card_y)),
        # Generate one variable for start date and end date
        earliest_svy_vacc_date = earliest,
        latest_svy_vacc_date = latest,
        # Drop DOB if too early
        dob_date_card = if_else((dob_date_card < earliest_svy_vacc_date) %in% TRUE,
                                NA_Date_, dob_date_card),
        dob_date_history = if_else((dob_date_history < earliest_svy_vacc_date) %in% TRUE,
                                   NA_Date_, dob_date_history),
        #Drop DOB if too late
        dob_date_card = if_else((dob_date_card > latest_svy_vacc_date) %in% TRUE,
                                NA_Date_, dob_date_card),
        dob_date_history = if_else((dob_date_history > latest_svy_vacc_date) %in% TRUE,
                                   NA_Date_, dob_date_history),

        dob_card_dqd_date = dob_date_card,
        dob_history_dqd_date = dob_date_history
      )

    if(RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){

      dat <- dat %>%
        # Generate register dob in one variable
        mutate(
          dob_date_register = suppressWarnings(vcqi_mdy(
            dob_date_register_m, dob_date_register_d, dob_date_register_y)),
          # Drop register DOB if too early or too late
          dob_date_register = if_else(
            (dob_date_register < earliest_svy_vacc_date) | (dob_date_register > latest_svy_vacc_date),
            NA_Date_, dob_date_register),
          dob_register_dqd_date = dob_date_register
        )
    }

    # Logical statements for flags20-22
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        # Variable of interest: <dose>_<card/register>_date
        vaccdate <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_date"))

        # Find minimum age for this dose from the schedule
        minage <- get(paste0(RI_DOSE_LIST[v],"_min_age_days"), envir = .GlobalEnv)

        dat <- dat %>%
          mutate(
            # Flag 20: is the date before the earliest vaccination date possible for this survey
            tempvar1 = ifelse((!!vaccdate < earliest_svy_vacc_date & !is.na(!!vaccdate))%in% TRUE, 1, 0),
            # Flag 21a: is the date prior to the child's DOB
            tempvar2 = ifelse((!!vaccdate < dob_for_valid_dose_calculations & !is.na(!!vaccdate) & !is.na(dob_for_valid_dose_calculations))%in% TRUE, 1, 0),
            # Flag 21b: for non birth doses, is the dose date equal to the DOB?
            tempvar2 = ifelse((!!vaccdate == dob_for_valid_dose_calculations & !(minage %in% 0) & !is.na(!!vaccdate) & !is.na(dob_for_valid_dose_calculations))%in% TRUE, 1, tempvar2),
            # Flag 22a: is the dose date after the interview date
            tempvar3 = ifelse((!!vaccdate > date_of_interview & !is.na(!!vaccdate)) %in% TRUE, 1, 0),
            # Flag 22b: if interview date is missing, check against latest vx date possible for this survey
            tempvar3 = ifelse((!!vaccdate > latest_svy_vacc_date & !is.na(!!vaccdate) & is.na(date_of_interview))%in% TRUE, 1, tempvar3),
            # Check (temporary): any of flags 20-22 set?
            tempvar4 = ifelse(tempvar1 %in% 1 | tempvar2 %in% 1 | tempvar3 %in% 1, TRUE, FALSE)
          )

        dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                        label = paste0(RI_DOSE_LIST[v], " date, ", type[s], " -Before Earliest Possible Vacc Date in Survey")) %>% suppressWarnings()
        if (minage == 0){
          dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                          label = paste0(RI_DOSE_LIST[v], " date, ", type[s], " -Before DOB")) %>% suppressWarnings()
        } else if(minage != 0){
          dat$tempvar2 <- haven::labelled(dat$tempvar2,
                                          label = paste0(RI_DOSE_LIST[v], " date, ", type[s], " -Before or Equal to DOB")) %>% suppressWarnings()
        }
        dat$tempvar3 <- haven::labelled(dat$tempvar3,
                                        label = paste0(RI_DOSE_LIST[v], " date, ", type[s], " -After Interview Date or Latest Possible Vacc Date in Survey")) %>% suppressWarnings()

        names(dat)[which(names(dat) == "tempvar1")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag20")
        names(dat)[which(names(dat) == "tempvar2")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag21") # changed this to tempvar2 from tempvar3 on July 11 [CBC]
        names(dat)[which(names(dat) == "tempvar3")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag22")
        names(dat)[which(names(dat) == "tempvar4")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_flagsum02")
      }
    }

    # Create overall flag for flags 01-07 and 20-22
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        flagsum01  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_flagsum01"))
        flagsum02  <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_flagsum02"))

        dat <- mutate(dat,
                      !!paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag00") := ifelse(!!flagsum01 %in% TRUE | !!flagsum02 %in% TRUE, 1, 0))
      }
    }

    dat <- select(dat, -c(ends_with("_flagsum01"), ends_with("_flagsum02")))

    # Create new variable with dose date if there are no flags set
    # Create overall flag for flags 01-07 and 20-22
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        flag00 <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag00"))
        datevar <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_date"))

        dat <- mutate(dat, !!paste0(RI_DOSE_LIST[v],"_", type[s], "_dqd_date") := if_else(!!flag00 %in% 0, !!datevar , NA_Date_))

        dat <- select(dat,-c(!!datevar))
      }
    }

    #If the user requests a report on data quality, issue it now

    if (VCQI_REPORT_DATA_QUALITY == 1) {
      #Note: type object gets overwritten in the date_tick_chk programs below but needed downstream so save and restore
      savetypes <- type

      date_tick_chk_01_dob_present()
      date_tick_chk_02_dob_concordant()
      date_tick_chk_03_sensible_dob()
      # vcqi_source("date_tick_chk_04_dose_concordant") # TO DO uncomment when finished
      # vcqi_source("date_tick_chk_05_excel_report")    # TO DO uncomment when finished

      #vcqi_read(date_tick_in_progress)
      #use date_tick_in_progress, clear

      type <- savetypes
    }

    # Create new variable for tick mark to indicate if there was no good date-
    # did they have a tick mark OR
    # did the document indicate it with an invalid date
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        flag00 <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag00"))
        flag05 <- rlang::sym(paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag05"))
        tick <- rlang::sym(paste0(RI_DOSE_LIST[v],"_tick_",type[s]))

        # Set a tick if: the card had a tick, or it had a date with a problem
        dat <- mutate(dat, tempvar1 = ifelse((!!tick %in% 1 | !!flag00 %in% 1) %in% TRUE, 1, 0))
        # *Don't* set a tick if the original variable was missing the entire date and missing the tick
        dat <- mutate(dat, tempvar1 = ifelse((!(!!tick %in% 1) & !!flag05 %in% 1) %in% TRUE, 0, tempvar1))
        dat <- mutate(dat, tempvar1 = ifelse((is.na(!!tick) & !!flag05 %in% 1) %in% TRUE, NA, tempvar1))

        names(dat)[which(names(dat) == "tempvar1")] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_dqd_tick")

      }
    }

    #Count the total number of dates populated
    dd <- 0
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        countvar <- get(paste0(RI_DOSE_LIST[v],"_",type[s],"_date_dq_flag05"),dat)
        dd <- dd + length(which(countvar == 0))
      }
    }

    #Post to log if tick was changed to yes due to invalid date
    iv_date_tick <- 0
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        countvar1 <- get(paste0(RI_DOSE_LIST[v],"_",type[s],"_dqd_tick"),dat)
        countvar2 <- get(paste0(RI_DOSE_LIST[v],"_tick_",type[s]),dat)
        iv_date_tick <- iv_date_tick + length(which(countvar1 %in% 1 & countvar2 %in% c(0,2))) ###TO DO double check
      }
    }

    vcqi_log_comment(VCP, 4, "Data", paste0("Of the ", dd, " dose dates in the survey, ", iv_date_tick, " dates were incomplete or fell outside the possible vaccination date range for children eligible for this survey so the dates were made missing, and the corresponding tick variable made a yes."))


    # Rename to take 'dqd' out of the dose date/tick variable names
    for(s in seq_along(type)){
      for(v in seq_along(RI_DOSE_LIST)){
        names(dat)[which(names(dat) == paste0(RI_DOSE_LIST[v],"_",type[s],"_dqd_date"))] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_date")
        names(dat)[which(names(dat) == paste0(RI_DOSE_LIST[v],"_",type[s],"_dqd_tick"))] <- paste0(RI_DOSE_LIST[v],"_",type[s],"_tick")

      }
      names(dat)[which(names(dat) == paste0("dob_",type[s],"_dqd_date"))] <- paste0("dob_",type[s],"_date")
    }
    names(dat)[which(names(dat) == "dob_history_dqd_date")] <- "dob_history_date"

    # ************************************************************
    #Check to see if dose shifting is requested and run program

    #We run it here because we've finished writing the DQ report.
    #So we have documented the number of duplicate dates and
    #out of order dates in the original data, but haven't yet
    #changed dates to ticks for being out-of-order or duplicates.

    #Running here allows us to use dose shifting to emulate DHS
    #processing if we want to.

    if (!vcqi_object_exists("NUM_DOSE_SHIFTS")){vcqi_global(NUM_DOSE_SHIFTS,0)}
    if (NUM_DOSE_SHIFTS >=  1) {
      dat <- shift_RI_dates(NUM_DOSE_SHIFTS, dat = dat)
    }

    # ************************************************************

    # Fix dates and ticks if consecutive doses are out of order

    # Create dose pairs data frame to loop through - all earlier dose-later dose pairs in each multi-dose list

    dps <- lapply(2:9, function(x) if(vcqi_object_exists(paste0("RI_MULTI_", x, "_DOSE_LIST"))){
      data.frame(
        doselist = paste0("RI_MULTI_", x, "_DOSE_LIST"),
        dosecount = x
      )
    }) %>% do.call(rbind, .)

    if (!is.null(dps)){

      dps <- lapply(seq_along(dps$doselist), function(x) data.frame(
        dose = str_to_lower(get(dps$doselist[x])),
        doselist = dps$doselist[x]
      )) %>%
        do.call(rbind, .) %>%
        full_join(dps, ., by = "doselist") %>%
        uncount(dosecount, .remove = FALSE)

      multidoses <- dps %>% group_by(dose) %>%
        mutate(n = 1:n()) %>%
        mutate(dose = paste0(dose, n)) %>% pull(dose)

      dps <- dps  %>%
        group_by(dose) %>%
        mutate(
          earlier = c(1:n()),
          later = c(1:n())
        ) %>%
        tidyr::expand(earlier, later) %>%
        filter(earlier < later) %>%
        # Create some variables used for filling in holes in history evidence
        mutate(
          earlier_later_distance = later - earlier,
          check_prev_flag = ifelse(earlier_later_distance > 1, 1, 0)
        )

      # Create change-to-tick temporary variables
      for(s in seq_along(type)){
        for(i in seq_along(multidoses)){
          temp <- paste0(multidoses[i], "_", type[s], "_change_to_tick_temp") %>%
            rlang::sym()

          dat <- dat %>%
            mutate(!!temp := NA)
        }
      }

      for(s in seq_along(type)){
        for(j in 1:nrow(dps)){

          earlier_dose <- paste0(dps$dose[j], dps$earlier[j], "_", type[s], "_date") %>% rlang::sym()
          later_dose <- paste0(dps$dose[j], dps$later[j], "_", type[s], "_date") %>% rlang::sym()
          earlier_tick <- paste0(dps$dose[j], dps$earlier[j], "_", type[s], "_tick") %>% rlang::sym()
          later_tick <- paste0(dps$dose[j], dps$later[j], "_", type[s], "_tick") %>% rlang::sym()
          earlier_changeflag <- paste0(dps$dose[j], dps$earlier[j], "_", type[s], "_change_to_tick_temp") %>% rlang::sym()
          later_changeflag <- paste0(dps$dose[j], dps$later[j], "_", type[s], "_change_to_tick_temp") %>% rlang::sym()

          # Check for doses out of order, flag to change to ticks
          dat <- dat %>%
            mutate(
              tempvar = ifelse(!!earlier_dose >= !!later_dose & !is.na(!!earlier_dose) & !is.na(!!later_dose),
                               1, 0),
              !!earlier_changeflag := ifelse(tempvar %in% 1, 1, !!earlier_changeflag),
              !!later_changeflag := ifelse(tempvar %in% 1, 1, !!later_changeflag)
            )

          # Count doses out of order and log
          if(sum(dat$tempvar, na.rm = TRUE) > 0){
            vcqi_log_comment(
              VCP, 4, "Data",
              paste0("The ", type[s], " date for dose ", dps$later[j], " of ", dps$dose[j],
                     " is either the same as dose ", dps$earlier[j], " or occurred before dose ",
                     dps$earlier[j], " in ", sum(dat$tempvar, na.rm = TRUE),
                     " instances. Both dates were set to missing and tick set to yes."))
          }

          # Check if later dose received (by date or tick) but earlier dose missing
          # Flag to add tick for earlier dose
          dat <- dat %>%
            mutate(
              tempvar2 = ifelse((!is.na(!!later_dose) | !!later_tick %in% 1) & (is.na(!!earlier_dose) & !(!!earlier_tick %in% 1)), 1, 0),
              !!earlier_changeflag := ifelse(tempvar2 %in% 1, 1, !!earlier_changeflag)
            )

          if(sum(dat$tempvar2, na.rm = TRUE) > 0){
            vcqi_log_comment(
              VCP, 4, "Data",
              paste0("The ", type[s], " date for dose ", dps$dose[j], dps$later[j],
                     " was received but ", dps$dose[j], dps$earlier[j], " was not in ",
                     sum(dat$tempvar2, na.rm = TRUE), " instances. ",
                     str_to_upper(dps$dose[j]), dps$earlier[j], " tick set to yes.")
            )
          }

        } # End of j (dose pairs) loop
      } # End of s (type) loop

      # Use change_to_tick variables to update _date and _tick variables
      for(s in seq_along(type)){
        for(i in seq_along(multidoses)){
          loop_date <- paste0(multidoses[i], "_", type[s], "_date") %>% rlang::sym()
          loop_tick <- paste0(multidoses[i], "_", type[s], "_tick") %>% rlang::sym()
          loop_flag <- paste0(multidoses[i], "_", type[s], "_change_to_tick_temp") %>% rlang::sym()

          dat <- dat %>%
            mutate(!!loop_date := if_else(!!loop_flag %in% 1, NA_Date_, !!loop_date),
                   !!loop_tick := ifelse(!!loop_flag %in% 1, 1, !!loop_tick))
        }
      }

      # Drop temporary variables
      dat <- dat %>%
        select(-contains("tempvar"),
               -contains("tick_temp"))


      # Loop over dose series and fill any holes in history evidence ----

      fill_history <- TRUE
      if (vcqi_object_exists("DO_NOT_FILL_HISTORY_HOLES")){
        if (DO_NOT_FILL_HISTORY_HOLES %in% 1){
          fill_history <- FALSE
        }
      }

      if (fill_history == TRUE){

        for(d in seq_along(unique(dps$dose))){

          whichseries <- unique(dps$dose)[d]

          dps_d <- dps %>%
            filter(dose %in% whichseries)

          # Set temporary flags for doses to change
          for(j in 1:nrow(dps_d)){

            earlier_dose <- paste0(dps_d$dose[j], dps_d$earlier[j], "_history") %>% rlang::sym()
            later_dose <- paste0(dps_d$dose[j], dps_d$later[j], "_history") %>% rlang::sym()

            # Flag 24 - history shows later dose received but not earlier dose
            # Note Stata VCQI uses flag 23 for 2-dose & flag 24 for 3-dose series
            # while R VCQI uses flag 24 for all multidose series

            temp_changeflag <- paste0(
              "temp24_", dps_d$dose[j], "_", dps_d$later[j], dps_d$earlier[j],
              "_history_flag24") %>%
              rlang::sym()

            dat <- dat %>%
              mutate(
                !!temp_changeflag := ifelse(
                  !!later_dose %in% 1 & !(!!earlier_dose %in% 1), 1, 0),
              )

            # Check previous flags - e.g. if dose1 history is missing and both
            # dose2 and dose3 are present, the dose21 flag should be set, *not*
            # the dose31 flag.
            if (dps_d$check_prev_flag[j] %in% 1){

              prevflag_laterdoses <- dps_d$later[j] - c(1:(dps_d$earlier_later_distance[j]-1))

              for(k in seq_along(prevflag_laterdoses)){
                prev_changeflag <- paste0(
                  "temp24_", dps_d$dose[j], "_", prevflag_laterdoses[k], dps_d$earlier[j],
                  "_history_flag24") %>%
                  rlang::sym()

                dat <- dat %>%
                  mutate(
                    !!temp_changeflag := ifelse(
                      !!prev_changeflag %in% 1, 0, !!temp_changeflag),
                  )
              } # end k loop for updating flags
            } # end if

          } # end j loop through dose pairs

          # Summarize info from flags about which history evidence was changed
          laterdoses <- sort(unique(dps$later))
          for(k in seq_along(laterdoses)){

            temp <- dat %>%
              select(contains(paste0("temp24_", whichseries, "_", laterdoses[k]))) %>%
              mutate(changed = rowSums(., na.rm = TRUE),
                     changed = ifelse(changed > 0, 1, 0))

            if (nrow(filter(temp, changed %in% 1)) > 0){

              vcqi_log_comment(
                VCP, 4, "Data",
                paste0(
                  "For ", nrow(filter(temp, changed %in% 1)),
                  " respondents, there was history evidence of ",
                  stringr::str_to_upper(whichseries), laterdoses[k],
                  " but not of 1+ earlier doses of ",
                  stringr::str_to_upper(whichseries),
                  "; VCQI imputed history evidence for the missing earlier dose(s)."
                ))
            }
          } # end k loop for writing log comments

          # After temp flags are set and log comments written, update history
          # evidence and set flags for proximate doses to match Stata VCQI flags
          dps_d_p <- dps_d %>% arrange(dose, desc(later), desc(earlier)) %>%
            filter(earlier_later_distance %in% 1)
          for(j in 1:nrow(dps_d_p)){

            earlier_dose <- paste0(dps_d_p$dose[j], dps_d_p$earlier[j], "_history") %>% rlang::sym()
            later_dose <- paste0(dps_d_p$dose[j], dps_d_p$later[j], "_history") %>% rlang::sym()

            changeflag <- paste0(
              dps_d_p$dose[j], "_", dps_d_p$later[j], dps_d_p$earlier[j], "_history_flag24") %>%
              rlang::sym()

            dat <- dat %>%
              mutate(
                flag24temp := ifelse(!!later_dose %in% 1 & !(!!earlier_dose %in% 1), 1, 0),
                !!earlier_dose := ifelse(
                  !!later_dose %in% 1 & !(!!earlier_dose %in% 1), 1, !!earlier_dose)
              )

            # Label flag variables
            dat$flag24temp <- haven::labelled(
              dat$flag24temp,
              label = paste0("History shows dose ", dps_d_p$later[j], " but not dose ", dps_d_p$earlier[j]) %>%
              suppressWarnings()
            )
            names(dat)[which(names(dat) == "flag24temp")] <- paste0(
              dps_d_p$dose[j], "_", dps_d_p$later[j], dps_d_p$earlier[j], "_history_flag24")
          }

        } # end d loop (dose series, for filling in history evidence)

        # Remove temporary flags
        dat <- dat %>% select(-contains("temp24"))

      } # end if filling history holes

    } # End of if(!is.null(dps))

    # Create flags to indicate no card or no register
    dat <- dat %>%
      mutate(no_card = 1,
             no_register = 1)

    for(d in seq_along(RI_DOSE_LIST)){
      for(s in seq_along(type)){
        datevar <- rlang::sym(paste0(RI_DOSE_LIST[d],"_",type[s],"_date"))
        tickvar <- rlang::sym(paste0(RI_DOSE_LIST[d],"_",type[s],"_tick"))

        if(type[s] == "card"){
          dat <- mutate(dat,
                        no_card = ifelse((!is.na(!!datevar) | !is.na(!!tickvar)), 0, no_card))
        }

        if(type[s] == "register"){
          dat <- mutate(dat,
                        no_register = ifelse(
                          (!is.na(!!datevar) | !is.na(!!tickvar)), 0, no_register))
        }
      }
    }

    vcqi_log_comment(VCP, 4, "Data", paste0(sum(dat$no_card, na.rm = TRUE), " respondents did not have a card record in the dataset with dates or ticks recorded on it."))

    vcqi_log_comment(VCP, 4, "Data", paste0(sum(dat$no_register, na.rm = TRUE), " respondents did not have a register record with dates or ticks recorded on it."))

    # Create indicator variable for children who have a card, a valid dob & at least one dose date that occurs after dob
    #First, generate variable "showed_card_with_dates_after_dob"
    dat <- mutate(dat,
                  card_date_count = 0,
                  flag_dob_before_dosedate = NA_real_)

    for(d in seq_along(RI_DOSE_LIST)){
      carddate <- rlang::sym(paste0(RI_DOSE_LIST[d],"_card_date"))

      dat <- dat %>%
        mutate(
          card_date_count = ifelse(
            !is.na(!!carddate),
            card_date_count + 1, card_date_count),
          flag_dob_before_dosedate = ifelse(
            !!carddate >= dob_for_valid_dose_calculations &
              !is.na(!!carddate) &
              !is.na(dob_for_valid_dose_calculations),
            1, flag_dob_before_dosedate)
        )
    }

    dat <- mutate(dat,
                  showed_card_with_dates_after_dob = ifelse(
                    card_date_count > 0 &
                      flag_dob_before_dosedate %in% 1, 1, 0))

    #Now generate has_card_with_dob_and_dosedate variable
    dat$has_card_with_dob_and_dosedate <- 0 # Initialize as zero
    dat <- dat %>%
      mutate(
        has_card_with_dob_and_dosedate = ifelse(
          !is.na(dob_for_valid_dose_calculations) &
            showed_card_with_dates_after_dob %in% 1,
          1, has_card_with_dob_and_dosedate)
      )

    # Add variable labels

    dat$age_at_interview <- haven::labelled(dat$age_at_interview, label = "Age at interview (days)") %>% suppressWarnings()
    dat$no_card <- haven::labelled(dat$no_card, label = "No Card with Dates in Dataset") %>% suppressWarnings()
    dat$no_register <- haven::labelled(dat$no_register, label = "No Register Record with Dates in Dataset") %>% suppressWarnings()
    dat$card_date_count <- haven::labelled(dat$card_date_count, label = "Number of Dates on Card") %>% suppressWarnings()
    dat$flag_dob_before_dosedate <- haven::labelled(dat$flag_dob_before_dosedate, label = "Dob before at least one dose date") %>% suppressWarnings()
    dat$showed_card_with_dates_after_dob <- haven::labelled(dat$showed_card_with_dates_after_dob,
                                                            label = "Card Seen & 1+ dates on card after dob") %>% suppressWarnings()
    dat$has_card_with_dob_and_dosedate <- haven::labelled(dat$has_card_with_dob_and_dosedate,
                                                          label = "Child had card with valid dob & 1+ dose date after dob (0=no; 1=yes)") %>% suppressWarnings()

    for (s in seq_along(type)){
      for (v in seq_along(RI_DOSE_LIST)){

        f <- paste0(
          "dat$", RI_DOSE_LIST[v], "_", type[s],
          "_date_dq_flag00 <- haven::labelled(dat$",
          RI_DOSE_LIST[v], "_", type[s], "_date_dq_flag00,
          label = '", RI_DOSE_LIST[v], " date, ", type[s],
          " -Flag Problem(s) with Date') %>% suppressWarnings()")
        eval(parse_expr(f))

      }
    }

    # Save a file with flags (<VCQI_RI_DATASET>_dq_flags)
    filename <- paste0(VCQI_OUTPUT_FOLDER, "/",
                       tools::file_path_sans_ext(VCQI_RI_DATASET),
                       "_dq_flags.rds")
    saveRDS(dat, file = filename)

    datasetname <- paste0(tools::file_path_sans_ext(VCQI_RI_DATASET),
                          "_dq_flags.rds")
    vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, datasetname))

    # Keep cleaned date/tick/history/dob variables
    dat <- select(dat, c(RI01,RI03,RI11,RI12,
                         ends_with("card_date"),
                         ends_with("card_tick"),
                         no_card, no_register,
                         ends_with("register_date"),
                         ends_with("register_tick"),
                         dob_for_valid_dose_calculations,
                         age_at_interview,
                         ends_with("history"),
                         has_card_with_dob_and_dosedate,
                         card_date_count,
                         flag_dob_before_dosedate,
                         showed_card_with_dates_after_dob),
                  -dob_history)

    #Save a file with the cleaned up and renamed date, tick, history, and dob variables (<VCQI_RI_DATASET>_dqd)
    dat <- dat[ , order(names(dat))]
    filename <- paste0(VCQI_OUTPUT_FOLDER ,"/",
                       tools::file_path_sans_ext(VCQI_RI_DATASET),
                       "_dqd.rds")
    saveRDS(dat, file = filename)

    datasetname <- paste0(tools::file_path_sans_ext(VCQI_RI_DATASET), "_dqd.rds")
    vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, datasetname))

    # Merge the new clean variables on to the RI survey dataset, save
    # (<VCQI_RI_DATASET>_clean)

    dat2 <- dat %>%
      select(
        RI01, RI03, RI11, RI12, ends_with("card_date"), ends_with("card_tick"),
        no_card, no_register, ends_with("register_date"),
        ends_with("register_tick"), dob_for_valid_dose_calculations,
        age_at_interview, ends_with("history"), has_card_with_dob_and_dosedate,
        card_date_count, flag_dob_before_dosedate,
        showed_card_with_dates_after_dob
      )

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET)) %>%
      select(-ends_with("history"))

    # colstodrop <- rev(which(names(dat2) %in% names(dat)))
    #
    # for (q in seq_along(colstodrop)){
    #   colnumber <- colstodrop[q]
    #   if (!(names(dat2)[colnumber] %in% c("RI01", "RI03", "RI11", "RI12"))){
    #     dat2 <- dat2[,-colnumber]
    #   }
    # }

    dat <- left_join(dat, dat2, by = c("RI01", "RI03", "RI11", "RI12"))

    filename <- paste0(VCQI_OUTPUT_FOLDER, "/",
                       tools::file_path_sans_ext(VCQI_RI_DATASET)
                       ,"_clean.rds")
    saveRDS(dat, file = filename)

    datasetname <- paste0(tools::file_path_sans_ext(VCQI_RI_DATASET),
                          "_clean.rds")
    vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, datasetname))

    if (all(is.na(dat$dob_for_valid_dose_calculations))){
      vcqi_log_comment(
        VCP, 2, "Warning",
        "None of the records in the dataset have a full date of birth, so VCQI will not be able to calculate some RI indicators.")
      vcqi_global(VCQI_NO_DOBS, 1)
    } else{
      vcqi_global(VCQI_NO_DOBS, 0)
    }
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
