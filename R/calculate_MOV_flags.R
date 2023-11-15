#' Calculate derived variables for missed opportunity for simultaneous vaccination (MOV) analysis
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return RI_MOV_long_form_data and RI_MOV_flags_to_merge datasets in VCQI_OUTPUT_FOLDER
#'
#' @export
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import tidyselect
#' @import stringr
#'
#' @examples
#' calculate_MOV_flags()

# calculate_MOV_flags R version 1.07 - Biostat Global Consulting - 2023-07-14
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-12  1.00      Mia Yu          Original R version
# 2022-09-01  1.01      Mia Yu          Fix bugs
# 2022-09-27  1.02      Caitlin Clary   Multidose sections
# 2022-09-29  1.03      Caitlin Clary   Bug fix: drop rows with missing dob
# 2022-10-13  1.04      Caitlin Clary   Change ifelse to if_else in multidose
#                                       section of RI_RECORDS_SOUGHT_FOR_ALL == 1
#                                       to avoid pivot type compatibility error
# 2022-12-16  1.05      Caitlin Clary   Ensure variables are numeric before
#                                       labeling (all-NA vars default to logical)
# 2023-07-14  1.06      Caitlin Clary   When RI_RECORDS_SOUGHT_FOR_ALL = 1, no
#                                       longer mix and match card and register
#                                       within a single child's record
# 2023-10-23  1.07      Caitlin Clary   In step 02, fix bugs with (a) rows with
#                                       missing visit dates; (b) doses with no
#                                       visit dates in dataset being dropped
#                                       (can't drop bc used in code downstream)
# ******************************************************************************

calculate_MOV_flags <- function(VCP = "calculate_MOV_flags"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Run as long as VCQI_CHECK_INSTEAD_OF_RUN isn't set to 1:
  if (!vcqi_object_value("VCQI_CHECK_INSTEAD_OF_RUN", 1)){

    # Read RI dataset with IDs
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))

    # If none of the respondents have a dob_for_valid_dose_calculations
    # then do not set MOV flags

    if (VCQI_NO_DOBS == 1){
      vcqi_log_comment(VCP, 2, "Warning", "User attempted to calculate Missed Opportunities for Vaccination (MOV) flags, but none of the respondents has a complete data of birth for valid dose calculations.")
    } else {
      if (!vcqi_object_exists("VCQI_TESTING_CODE")){VCQI_TESTING_CODE <- 0}

      if (VCQI_TESTING_CODE == 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step00.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step00.rds"))
      }

      dat <- dat %>%
        select(respid, dob_for_valid_dose_calculations,
               ends_with("card_date"), ends_with("card_tick"),
               no_card, ends_with("register_date"), ends_with("register_tick"),
               ends_with("_history"), has_card_with_dob_and_dosedate)

      # If RI_RECORDS_NOT_SOUGHT, we just use data from the cards.

      # Otherwise, for MOV purposes we copy register dates to the card fields
      # and calculate everything with the card variables.

      # The following paragraph is no longer true as of 2023-07-14:
      # If RI_RECORDS_SOUGHT_FOR_ALL then when there is no card, copy the register
      # record to the card fields, and when there is both a card and an HC record,
      # then fill missing dates on the card from register for:
      #   a) any missing single-dose vaccine if the register has a date and card
      #      does not
      #   b) any series of doses if the register has more dates in the series
      #      than the card...if the register has fewer or the same number,
      #      then just use the data from the card

      # Define multi-dose data frame
      multi <- lapply(2:9, function(x) if(vcqi_object_exists(paste0("RI_MULTI_", x, "_DOSE_LIST"))){
        data.frame(
          doselist = paste0("RI_MULTI_", x, "_DOSE_LIST"),
          dosecount = x
        )
      }) %>% do.call(rbind, .)

      if(!is.null(multi)){
        multi <- lapply(seq_along(multi$doselist), function(x) data.frame(
          dose = str_to_lower(get(multi$doselist[x])),
          doselist = multi$doselist[x]
        )) %>%
          do.call(rbind, .) %>%
          full_join(multi, ., by = "doselist")
      }

      if (RI_RECORDS_NOT_SOUGHT == 1) {
        # No action required
      }

      if (RI_RECORDS_NOT_SOUGHT == 0){
        for (d in seq_along(RI_DOSE_LIST)){

          eval(parse_expr(paste0("dat <- mutate(dat,", RI_DOSE_LIST[d],
                                 "_card_date = if_else(no_card %in% 1,",
                                 RI_DOSE_LIST[d], "_register_date,",
                                 RI_DOSE_LIST[d], "_card_date))")))

          eval(parse_expr(paste0("dat <- mutate(dat,", RI_DOSE_LIST[d],
                                 "_card_tick = if_else(no_card %in% 1,",
                                 RI_DOSE_LIST[d],"_register_tick,",
                                 RI_DOSE_LIST[d],"_card_tick))")))

        } # end of dose loop
      } # end of if (RI_RECORDS_NOT_SOUGHT == 0)

      # Below section removed 2023-07-14 - we no longer mix and match card and
      # register dates within a single child's record

      # if (RI_RECORDS_SOUGHT_FOR_ALL == 1){
      #
      #   if(vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
      #     single_dose <- str_to_lower(RI_SINGLE_DOSE_LIST)
      #
      #     for (d in seq_along(single_dose)){
      #
      #       eval(parse_expr(paste0("dat <- mutate(dat,", single_dose[d],
      #                              "_card_date = if_else(no_card %in% 1,",
      #                              single_dose[d], "_register_date,",
      #                              single_dose[d],"_card_date))")))
      #
      #       eval(parse_expr(paste0("dat <- mutate(dat,", single_dose[d],
      #                              "_card_tick = if_else(no_card %in% 1,",
      #                              single_dose[d], "_register_tick,",
      #                              single_dose[d], "_card_tick))")))
      #
      #       if ("moveit" %in% names(dat)){dat <- select(dat, -moveit)}
      #
      #       # Use register data if there's a date on register and not card or
      #       # if there are no dates, but there's a tick on register and not card
      #       carddate <- rlang::sym(paste0(single_dose[d], "_card_date"))
      #       regdate <- rlang::sym(paste0(single_dose[d], "_register_date"))
      #       cardtick <- rlang::sym(paste0(single_dose[d], "_card_tick"))
      #       regtick <- rlang::sym(paste0(single_dose[d], "_register_tick"))
      #
      #       dat <- dat %>%
      #         mutate(
      #           moveit = if_else(
      #             (is.na(!!carddate) & !is.na(!!regdate)) |
      #               (is.na(!!carddate) & is.na(!!regdate) &
      #                  !is.na(!!regtick) & !(!!cardtick %in% 1)),
      #             1, 0))
      #
      #       eval(parse_expr(paste0("dat <- mutate(dat,", single_dose[d],
      #                              "_card_date = if_else(moveit %in% 1,",
      #                              single_dose[d], "_register_date,",
      #                              single_dose[d], "_card_date))")))
      #
      #       eval(parse_expr(paste0("dat <- mutate(dat,", single_dose[d],
      #                              "_card_tick = if_else(moveit %in% 1,",
      #                              single_dose[d], "_register_tick,",
      #                              single_dose[d],"_card_tick))")))
      #
      #       dat <- select(dat, -moveit)
      #
      #     } # end of single dose loop
      #   }
      #
      #   #rm(carddate, regdate, cardtick, regtick) %>% suppressWarnings()
      #
      #   # Multi-dose: use register data where appropriate
      #
      #   if(!is.null(multi)){
      #     for(d in 1:nrow(multi)){
      #
      #       dn <- multi$dose[d]
      #       di <- seq(1, multi$dosecount[d], by = 1)
      #
      #       # Use register data if there are more dates in the register for the
      #       # series than appear on the card
      #
      #       # Grab register variables:
      #       reg_vars <- dat %>% select(c(paste0(dn, di, "_register_date"))) %>%
      #         mutate(across(everything(), ~ if_else(!is.na(.x), 1, 0))) %>%
      #         mutate(
      #           temp_register_dates = rowSums(.[, 1:length(.)], na.rm = TRUE)
      #         ) %>% select(temp_register_dates)
      #
      #       # Grab card variables:
      #       card_vars <- dat %>% select(c(paste0(dn, di, "_card_date"))) %>%
      #         mutate(across(everything(), ~ if_else(!is.na(.x), 1, 0))) %>%
      #         mutate(
      #           temp_card_dates = rowSums(.[, 1:length(.)], na.rm = TRUE)
      #         ) %>% select(temp_card_dates)
      #
      #       dat$temp_register_dates <- reg_vars$temp_register_dates
      #       dat$temp_card_dates <- card_vars$temp_card_dates
      #
      #       dat <- dat %>%
      #         mutate(
      #           moveit = if_else(temp_register_dates > temp_card_dates, 1, 0)
      #         )
      #
      #       for(i in seq_along(di)){
      #
      #         dat <- dat %>%
      #           mutate(
      #             !!paste0(dn, di[i], "_card_date") := if_else(
      #               moveit %in% 1,
      #               !!rlang::sym(paste0(dn, di[i], "_register_date")),
      #               !!rlang::sym(paste0(dn, di[i], "_card_date"))),
      #             !!paste0(dn, di[i], "_card_tick") := if_else(
      #               moveit %in% 1,
      #               !!rlang::sym(paste0(dn, di[i], "_register_tick")),
      #               !!rlang::sym(paste0(dn, di[i], "_card_tick")))
      #           )
      #       }
      #     } # end multidose (d) loop
      #
      #     # Drop temporary variables
      #     dat <- dat %>%
      #       select(-temp_register_dates, -temp_card_dates, -moveit)
      #
      #   } # end if(!is.null(multi))
      #
      # } # end of RI_RECORDS_SOUGHT_FOR_ALL

      # Now set the card tick variable to yes if the card date is missing and
      # the tick is not set, but the history is set...for MOV flags, we treat
      # a history report as the same as a card tick...we do not put a child
      # with either a card tick or a history report in the denominator for MOVs

      for (d in seq_along(RI_DOSE_LIST)){
        eval(parse_expr(paste0(
          "dat <- mutate(dat,", RI_DOSE_LIST[d],
          "_card_tick = if_else(is.na(",RI_DOSE_LIST[d],
          "_card_tick), 0, ", RI_DOSE_LIST[d], "_card_tick))")))

        eval(parse_expr(paste0(
          "dat <- mutate(dat, ",RI_DOSE_LIST[d],
          "_card_tick = if_else((is.na(",RI_DOSE_LIST[d],
          "_card_date) &", RI_DOSE_LIST[d], "_card_tick %in% 0 & ",
          RI_DOSE_LIST[d], "_history %in% 1) %in% TRUE, 1,",
          RI_DOSE_LIST[d], "_card_tick))")))
      } # end of dose loop

      dat <- dat %>%
        select(respid, dob_for_valid_dose_calculations,
               ends_with("card_date"), ends_with("card_tick"),
               -dob_card_date)

      # NOTE: Stata version makes faux third doses for two-dose series - that
      # approach isn't taken in the R version

      # Generate a string D/T/M (date/tick/missing) for doses in a series
      # Note: This variable will be used at bottom of code when updating MOV flags

      if (!is.null(multi)){
        for(d in 1:nrow(multi)){
          dn <- multi$dose[d]
          di <- seq(1, multi$dosecount[d], by = 1)

          for(i in seq_along(di)){

            dat <- dat %>%
              mutate(
                !!paste0(dn, di[i], "_str1") := case_when(
                  is.na(!!rlang::sym(paste0(dn, di[i], "_card_date"))) &
                    !!rlang::sym(paste0(dn, di[i], "_card_tick")) %in% 0 ~ "M",
                  !!rlang::sym(paste0(dn, di[i], "_card_tick")) %in% 1 ~ "T",
                  !is.na(!!rlang::sym(paste0(dn, di[i], "_card_date"))) ~ "D"
                )
              )
          }

          # Concatenate doses into a string variable
          tempdat <- dat %>% select(contains("str1") & contains(dn)) %>%
            unite(tempvar, everything(), sep = "")

          dat[paste0(dn, "_str")] <- tempdat$tempvar

        } # end multidose (d) loop
        dat <- dat %>% select(-contains("str1"))
      } # end if (!is.null(multi))

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step01.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step01.rds"))
      }

      # *******************************************************************************
      # Now we're going to convert this from a wide dataset with one row per
      # respondent to a long dataset with one row per respondent/date...so if
      # the first respondent was vaccinated on 5 different dates, they'll have
      # five rows in the long dataset...each row indicates which vaccines the
      # person received on that date.  Each row also indicates whether they got
      # the dose according to tick instead of date, and we carry along the useful
      # character strings for the multi-dose vaccines, as well.

      dat <- dat %>%
        pivot_longer(contains("card_date"), names_to = "visitdose",
                     values_to = "visitdate")

      datsummary <- dat %>%
        group_by(visitdose) %>%
        summarize(n_dates = sum(!is.na(visitdate)))

      no_dates <- datsummary %>%
        filter(n_dates %in% 0) %>% pull(visitdose)

      no_dates_doses <- no_dates %>%
        stringr::str_replace(., "_card_date", "")

      # Note - downstream we'll restore variables for doses in no_dates, which
      # are needed later in this program

      dat <- dat %>%
        filter(!is.na(visitdate)) %>%
        #filter(!is.na(visitdate) | (is.na(visitdate) & visitdose %in% no_dates)) %>%
        mutate(visitdose = str_replace(visitdose, "_card_date", ""),
               test = 1) %>%
        pivot_wider(names_from = "visitdose",
                    values_from = "test",
                    names_prefix = "got_")

      if (length(no_dates_doses) > 0){
        for (ds in seq_along(no_dates_doses)){
          dosevar_d <- rlang::sym(paste0("got_", no_dates_doses[ds]))
          dat <- dat %>% mutate(!!dosevar_d := 0)
        }
      }

      # Logic for MISS-VCQI - handling the faux "visit" dose, which is handled
      # differently the two times MISS-VCQI calls calculate_MOV_flags
      if (any(names(dat) %in% "got_visit")){
        if ("visit_card_date" %in% no_dates){
          dat <- dat %>%
            filter(is.na(got_visit))
        }
      }

      # For multidose series, grab the got_<dose><#> columns and coalesce, then
      # attach the coalesced column as got_<dose> and drop the got_<dose><#> cols

      if(!is.null(multi)){
        for(d in 1:nrow(multi)){
          dn <- multi$dose[d]
          di <- 1:multi$dosecount[d]

          var_i <- NULL
          for(ind in seq_along(di)){var_i <- c(var_i, paste0("got_", dn, di[ind]))}

          var_i <- var_i[var_i %in% names(dat)]

          temp <- dat %>% select(all_of(var_i))
          vars_to_drop <- names(temp)
          temp <- temp %>%
            mutate(var = coalesce(!!! select(., everything())))

          dat[paste0("got_", dn)] <- temp$var
          dat <- select(dat, -all_of(vars_to_drop))
        } # end d loop (multi doses)
      }

      dat <- dat %>%
        rename_with(~ paste0("got_", str_replace(., "_card_tick", ""), "_tick"),
                    .cols = contains("_card_tick")) %>%
        rename(dob = dob_for_valid_dose_calculations)

      # Define vector for ordering variables (except <dose>_str vars)
      tempvarnames <- c("respid", "dob", "visitdate")
      if (vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
        for(s in seq_along(RI_SINGLE_DOSE_LIST)){
          tempvarnames <- c(
            tempvarnames,
            paste0("got_", str_to_lower(RI_SINGLE_DOSE_LIST[s])),
            paste0("got_", str_to_lower(RI_SINGLE_DOSE_LIST[s]), "_tick"))}
      }

      if (!is.null(multi)){
        for(s in 1:nrow(multi)){
          tempvarnames <- c(
            tempvarnames,
            paste0("got_", multi$dose[s]),
            paste0("got_", multi$dose[s], 1:multi$dosecount[s], "_tick"))}}

      tempvarnames <- tempvarnames[tempvarnames %in% names(dat)]

      dat <- dat %>%
        # Order vars using tempvarnames; everything() puts dose_<str> vars at end
        select(all_of(tempvarnames), everything()) %>%
        # Replace NAs with 0s in the got_<dose> columns
        mutate(across(.cols = contains("got_"), ~ifelse(is.na(.), 0, .))) %>%
        # Make sure all rows are unique
        unique() %>%
        # And drop any rows missing DOB
        filter(!is.na(dob))

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step02.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step02.rds"))
      }

      # ******************************************************************************
      # Right-o...now the dataset is long

      # Load up the scalars with the vaccination schedule

      # These scalars have already been defined in the control program so there
      # is no need to re-define them here.
      # do "${VCQI_PROGRAMS_ROOTPATH}/RI/RI_schedule.do"

      dat <- dat %>%
        mutate(age = visitdate - dob) %>%
        relocate(age, .after = visitdate) %>%
        arrange(respid, age)

      # Make a unique id for each person
      dat <- dat %>% mutate(person = respid) %>%
        relocate(c(person, age), .after = last_col())

      # **********************************************
      # Set up variables for the multi-dose vaccines
      # **********************************************
      type <- c("crude", "valid")

      if(!is.null(multi)){
        for(d in 1:nrow(multi)){
          dn <- multi$dose[d]
          di <- seq(1, multi$dosecount[d], by = 1)

          for(ty in seq_along(type)){

            for(i in seq_along(di)){

              gotseries <- rlang::sym(paste0("got_", dn)) # got_penta
              dosestr <- rlang::sym(paste0(dn, "_str")) # penta_str

              agedose <- rlang::sym(paste0("age_", dn, i, "_", type[ty])) # age_penta1_crude
              ageat <- rlang::sym(paste0("age_at_", dn, i, "_", type[ty]))
              creditdose <- rlang::sym(paste0("credit_", dn, i, "_", type[ty])) # credit_penta1_crude
              eligdose <- rlang::sym(paste0("elig_", dn, i, "_", type[ty])) # elig_penta1_crude
              gotdose <- rlang::sym(paste0("got_", dn, i, "_", type[ty]))
              cumdose <- rlang::sym(paste0("cum_", dn, i, "_", type[ty]))
              flagdose <- rlang::sym(paste0("flag_got_", dn, i, "_", type[ty]))

              minage <- get(paste0(dn, i, "_min_age_days"), envir = globalenv())

              if (i > 1){
                minint <- get(paste0(dn, i, "_min_interval_days"), envir = globalenv())
                agedoseprev <- rlang::sym(paste0("age_at_", dn, i-1, "_", type[ty]))
                flagdoseprev <- rlang::sym(paste0("flag_got_", dn, i-1, "_", type[ty]))
              }

              # Make age_<dose><#>_<type> variable
              dat <- dat %>%
                mutate(!!agedose := age)

              if (i == 1){
                dat <- dat %>%
                  mutate(
                    !!creditdose := ifelse(age >= minage, 1, 0),
                    !!eligdose := ifelse(age >= minage, 1, 0)
                  )

                # If early doses count, then child is always eligible
                if (type[ty] == "crude"){
                  dat <- dat %>%
                    mutate(!!creditdose := 1)
                }

                # Not eligible and no credit if received per tick mark/history
                dat <- dat %>%
                  mutate(

                    !!creditdose := ifelse(stringr::str_sub(!!dosestr, i, i) == "T",
                                           0, !!creditdose),
                    !!eligdose := ifelse(stringr::str_sub(!!dosestr, i, i) == "T",
                                         0, !!eligdose)
                  )

                dat <- dat %>%
                  mutate(
                    !!gotdose := ifelse(
                      !!creditdose %in% 1 & !!gotseries %in% 1, 1, 0)
                  )

                dat <- dat %>%
                  group_by(person) %>%
                  mutate(!!cumdose := cumsum(!!gotdose)) %>%
                  ungroup()

                dat <- dat %>%
                  mutate(
                    !!gotdose := ifelse(!!gotdose %in% 1 & !!cumdose %in% 1, 1, 0)
                  ) %>%
                  select(-!!cumdose)

                dat <- dat %>%
                  mutate(
                    tempage = !!gotdose * age # "dropthis" vars in Stata
                  ) %>%
                  group_by(person) %>%
                  mutate(!!ageat := max(tempage, na.rm = TRUE),
                         !!flagdose := max(!!gotdose, na.rm = TRUE)
                  ) %>% suppressWarnings()
                dat <- dat %>% mutate(!!ageat := ifelse((!!ageat == Inf | !!ageat == -Inf) %in% TRUE, NA, !!ageat))
                dat <- dat %>% mutate(!!flagdose := ifelse((!!flagdose == Inf | !!flagdose == -Inf) %in% TRUE, NA, !!flagdose))

                # For crude doses, replace eligible with 0 if child got an early dose
                # Note: comment says this is for crude doses, but in Stata they commented out the condition
                dat <- dat %>% mutate(
                  !!eligdose := ifelse(!!flagdose %in% 1 & age > !!ageat,
                                       0, !!eligdose)
                )

              } else if (i > 1){
                dat <- dat %>%
                  mutate(
                    !!creditdose := ifelse(
                      !!flagdoseprev %in% 1 &
                        age >= (!!agedoseprev + minint), 1, 0
                    ),
                    !!eligdose := ifelse(
                      !!flagdoseprev %in% 1 &
                        age >= (!!agedoseprev + minint) &
                        age >= minage, 1, 0
                    )
                  )

                # If early doses count, then child is always eligible
                if(type[ty] == "crude"){
                  dat <- dat %>%
                    mutate(!!creditdose := ifelse(
                      !!flagdoseprev %in% 1 & age > !!agedoseprev, 1, 0
                    ))
                }

                # Not eligible/no credit if any of doses 1:i were rec'd by tick
                # mark or history
                dat <- dat %>%
                  mutate(
                    tempstring = stringr::str_sub(!!dosestr, 1, i),
                    tempanytick = ifelse(stringr::str_detect(tempstring, "T"), 1, 0),
                    !!creditdose := ifelse(tempanytick %in% 1, 0, !!creditdose),
                    !!eligdose := ifelse(tempanytick %in% 1, 0, !!eligdose)
                  ) %>% select(-c(tempstring,tempanytick))

                dat <- dat %>%
                  mutate(
                    !!gotdose := ifelse(
                      !!creditdose %in% 1 & !!gotseries %in% 1, 1, 0)
                  )

                dat <- dat %>%
                  group_by(person) %>%
                  mutate(!!cumdose := cumsum(!!gotdose)) %>%
                  ungroup()

                dat <- dat %>%
                  mutate(
                    !!gotdose := ifelse(!!gotdose %in% 1 & !!cumdose %in% 1, 1, 0)
                  ) %>%
                  select(-!!cumdose)

                dat <- dat %>%
                  mutate(
                    tempage = !!gotdose * age # "dropthis" vars in Stata
                  ) %>%
                  group_by(person) %>%
                  mutate(!!ageat := max(tempage, na.rm = TRUE),
                         !!flagdose := max(!!gotdose, na.rm = TRUE)
                  ) %>% ungroup() %>% suppressWarnings()
                dat <- dat %>% mutate(!!ageat := ifelse((!!ageat == Inf | !!ageat == -Inf) %in% TRUE, NA, !!ageat))
                dat <- dat %>% mutate(!!flagdose := ifelse((!!flagdose == Inf | !!flagdose == -Inf) %in% TRUE, NA, !!flagdose))

                # For crude doses, replace eligible with 0 if child got an early dose
                # Note: comment says this is for crude doses, but in Stata they commented out the condition
                dat <- dat %>% mutate(
                  !!eligdose := ifelse(!!flagdose %in% 1 & age > !!ageat,
                                       0, !!eligdose)
                )
              }

            } # end i loop

            #Note: paste0("cum_", dn, "_", type[ty]) also created later
            # Cumulative doses of <dn> up to and including this visit
            cum_dn <- rlang::sym(paste0("cum_", dn, "_", type[ty]))
            got_dn <- rlang::sym(paste0("got_", dn))

            dat <- dat %>%
              mutate(!!cum_dn := cumsum(!!got_dn))

            # label var cum_`d'_`t' "Cumulative doses of `d' (up to and including this visit) - `t'"

          } # end t loop

          dat <- dat %>% select(-contains("temp"))

        } # end d loop
      } # end if (!is.null(multi))

      # ********************************************************
      # Set up the same variables for the single-dose vaccines
      # ********************************************************
      if (vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
        single_dose <- str_to_lower(RI_SINGLE_DOSE_LIST)
        type <- c("crude","valid")
        for (d in seq_along(single_dose)){
          for (t in seq_along(type)){
            # this just makes a copy of the age variable for easy
            # reading in the data editor; drop later
            dat <- dat %>% mutate(!!paste0("age_",single_dose[d],"_",type[t]) := age)

            minage <- get(paste0(single_dose[d],"_min_age_days"),envir = parent.frame())
            #can we count the dose if it occurs in this visit?
            dat <- dat %>% mutate(tempvar1 = if_else(age >= minage, 1, 0))
            #would it be an MOV if not given at this visit?
            dat <- dat %>% mutate(tempvar2 = if_else(age >= minage, 1, 0))

            #if early doses count, then s/he is always eligible
            if (type[t] == "crude"){
              dat <- dat %>% mutate(tempvar1 = 1)
            }

            # Not eligible and no credit if rec'd by tick/history
            tick <- rlang::sym(paste0("got_",single_dose[d],"_tick"))
            dat <- dat %>% mutate(tempvar1 = ifelse(!!tick %in% 1, 0, tempvar1),
                                  tempvar2 = ifelse(!!tick %in% 1, 0, tempvar2))

            # if user specified max age for valid doses using the scalar
            # `d'_max_age_days, then use it to clarify that they will not receive
            # credit nor are they eligible for the dose if they are too old

            if (vcqi_object_exists(paste0(single_dose[d],"_max_age_days"))){
              minage <- get(paste0(single_dose[d],"_min_age_days"),envir = parent.frame())
              maxage <- get(paste0(single_dose[d],"_max_age_days"),envir = parent.frame())
              dat <- dat %>% mutate(tempvar1 = if_else((age >= minage & age <= maxage) %in% TRUE, 1, 0),
                                    tempvar2 = if_else((age >= minage & age <= maxage) %in% TRUE, 1, 0))
            }

            # did s/he get a valid dose at this visit?
            gotdose <- rlang::sym(paste0("got_",single_dose[d]))
            dat <- dat %>% mutate(tempvar3 = if_else(tempvar1 %in% 1 & !!gotdose %in% 1, 1, 0))
            #rm(credit,gotdose) %>% suppressWarnings()

            # only track the first valid dose; ignore later doses
            dat <- dat %>% group_by(person) %>% arrange(person) %>% mutate(cum = cumsum(if_else(is.na(tempvar3), 0, tempvar3)) + tempvar3*0) %>% ungroup()
            dat <- dat %>% mutate(tempvar3 = if_else((tempvar3 %in% 1 & cum %in% 1) %in% TRUE,1,0)) %>% select(-c(cum))

            # calculate and remember the age at which they got this dose
            dat <- dat %>% mutate(drop = tempvar3 * age)
            dat <- dat %>% group_by(person) %>% arrange(person) %>% mutate(tempvar4 = max(drop,na.rm = TRUE)) %>% ungroup() %>% suppressWarnings()
            dat <- dat %>% mutate(tempvar4 = ifelse((tempvar4 == Inf | tempvar4 == -Inf) %in% TRUE, NA, tempvar4))
            dat <- select(dat, -c(drop))

            # later we will drop all rows but one for this person, so
            # set a flag here in all rows indicating that they got a valid dose
            dat <- dat %>%
              group_by(person) %>%
              arrange(person) %>%
              mutate(tempvar5 = max(tempvar3, na.rm = TRUE)) %>%
              ungroup() %>% suppressWarnings()
            dat <- dat %>% mutate(tempvar5 = ifelse((tempvar5 == Inf | tempvar5 == -Inf) %in% TRUE, NA, tempvar5))

            # DAR: changing this to be true for both crude & valid

            # For crude doses, replace eligible with 0 if child got an early dose

            dat <- dat %>%
              mutate(tempvar2 = if_else((tempvar5 %in% 1 & (age > tempvar4)) %in% TRUE,
                                        0, tempvar2))

            names(dat)[which(names(dat) == "tempvar1")] <- paste0(
              "credit_", single_dose[d],"_",type[t])
            names(dat)[which(names(dat) == "tempvar2")] <- paste0(
              "elig_", single_dose[d], "_", type[t])
            names(dat)[which(names(dat) == "tempvar3")] <- paste0(
              "got_", single_dose[d], "_", type[t])
            names(dat)[which(names(dat) == "tempvar4")] <- paste0(
              "age_at_", single_dose[d], "_", type[t])
            names(dat)[which(names(dat) == "tempvar5")] <- paste0(
              "flag_got_", single_dose[d], "_", type[t])

          } # end of type loop
        } # end of single dose loop

      } # end if single dose list exists

      # Label elig_* and credit_*
      ##################################
      ####place holder for var label####
      ##################################

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step03.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step03.rds"))
      }

      # *********************************************************
      # Calculate the variables for the mov measures
      # *********************************************************

      type <- c("crude","valid")
      for (d in seq_along(RI_DOSE_LIST)){
        for (t in seq_along(type)){

          # how many rec'd up to and including this visit?
          got <- rlang::sym(paste0("got_", RI_DOSE_LIST[d], "_", type[t]))

          dat <- dat %>%
            group_by(person) %>%
            mutate(
              tempvar1 = cumsum(!!got)
            )

          # #label variable cum_`d'_`t' "Cumulative doses of `d' received up-to-and-including this visit - `t'"

          # an mov is when s/he is eligible and doesn't receive it
          elig <- rlang::sym(paste0("elig_", RI_DOSE_LIST[d], "_", type[t]))
          dat <- dat %>%
            mutate(tempvar2 = if_else((!!elig %in% 1 & tempvar1 %in% 0) %in% TRUE,1,0))
          #label variable mov_`d'_`t' "Experienced an MOV for `d' in this visit - `t'"

          # cumulative movs up to and including this visit
          dat <- dat %>% group_by(person) %>% arrange(person) %>% mutate(tempvar3 = cumsum(if_else(is.na(tempvar2), 0, tempvar2)) + tempvar2*0) %>% ungroup()
          #label variable cum_mov_`d'_`t' "Cumulative MOVs for `d' thru this visit - `t'"

          # corrected mov is when they have had 1+ movs and then they get it
          dat <- dat %>% mutate(tempvar4 = if_else((tempvar3 > 0 & !!got %in% 1) %in% TRUE, 1, 0))
          #label variable cor_mov_`d'_`t' "Experienced a corrected MOV for `d' - `t'"

          # set a flag (in all visits) if the child had a 1+ corrected movs
          dat <- dat %>% group_by(person) %>% arrange(person) %>% mutate(tempvar5 = sum(tempvar4, na.rm = TRUE)) %>% ungroup()
          dat <- dat %>% mutate(tempvar5 = if_else(tempvar5 > 0 ,1,0))
          #label variable flag_cor_mov_`d'_`t' "Experienced 1+ corrected MOVs for `d' - `t'"

          # record (in all visits) the child's total number of movs
          dat <- dat %>% group_by(person) %>% arrange(person) %>% mutate(tempvar6 = sum(tempvar2, na.rm = TRUE)) %>% ungroup()
          #label variable total_mov_`d'_`t' "Total MOVs for `d' - `t'"

          # set a counter (in all visits) of the number of eligible opportunities
          dat <- dat %>%
            group_by(person) %>%
            mutate(
              !!paste0("total_elig_", RI_DOSE_LIST[d],
                       "_", type[t]) := sum(!!elig, na.rm = TRUE)) %>%
            ungroup()

          #label variable total_elig_`d'_`t' "Total visits where eligible to receive `d' - `t'"

          # set a flag (in all visits) if the child had 1+ movs
          dat <- dat %>% mutate(tempvar7 = if_else(tempvar6 > 0 ,1 ,0))
          #label variable flag_had_mov_`d'_`t' "Had 1+ MOVs for `d' in any visit - `t'"

          # set a flag (in all visits) if the child had only uncorrected movs for this dose
          flaggot <- rlang::sym(paste0("flag_got_",RI_DOSE_LIST[d],"_",type[t]))
          dat <- dat %>%
            mutate(
              !!paste0("flag_uncor_mov_", RI_DOSE_LIST[d], "_", type[t]) :=
                if_else((tempvar7 %in% 1 & !!flaggot %in% 0) %in% TRUE, 1 ,0))

          names(dat)[which(names(dat) == "tempvar1")] <- paste0("cum_", RI_DOSE_LIST[d], "_", type[t])
          names(dat)[which(names(dat) == "tempvar2")] <- paste0("mov_", RI_DOSE_LIST[d], "_", type[t])
          names(dat)[which(names(dat) == "tempvar3")] <- paste0("cum_mov_", RI_DOSE_LIST[d], "_", type[t])
          names(dat)[which(names(dat) == "tempvar4")] <- paste0("cor_mov_", RI_DOSE_LIST[d], "_", type[t])
          names(dat)[which(names(dat) == "tempvar5")] <- paste0("flag_cor_mov_", RI_DOSE_LIST[d], "_", type[t])
          names(dat)[which(names(dat) == "tempvar6")] <- paste0("total_mov_", RI_DOSE_LIST[d], "_", type[t])
          names(dat)[which(names(dat) == "tempvar7")] <- paste0("flag_had_mov_", RI_DOSE_LIST[d], "_", type[t])

          dat <- dat %>%
            relocate(ends_with(paste0(RI_DOSE_LIST[d], "_", type[t])),
                     .after = last_col())

        } # end of type loop
      } # end of dose loop

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step04.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step04.rds"))
      }

      # ********************************************************************************
      #  Update MOV flags
      # ********************************************************************************

      # If dose was recorded by tick mark/history, do not include in MOV
      # Set the MOV-related flags to zero

      if(vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
        single_dose <- str_to_lower(RI_SINGLE_DOSE_LIST)
        type <- c("crude", "valid")

        for (d in seq_along(single_dose)){
          for (t in seq_along(type)){
            eval(parse_expr(paste0(
              "dat <- mutate(dat, mov_", single_dose[d], "_",type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, cum_mov_", single_dose[d], "_",type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, cum_mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, cor_mov_", single_dose[d], "_",type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, cor_mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, flag_cor_mov_", single_dose[d], "_", type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, flag_cor_mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, total_mov_", single_dose[d], "_", type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, total_mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, flag_had_mov_", single_dose[d], "_", type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, flag_had_mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, flag_uncor_mov_", single_dose[d], "_", type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, flag_uncor_mov_",
              single_dose[d], "_", type[t], "))")))

            eval(parse_expr(paste0(
              "dat <- mutate(dat, total_elig_", single_dose[d], "_", type[t],
              " = if_else(got_", single_dose[d], "_tick %in% 1, 0, total_elig_",
              single_dose[d], "_", type[t], "))")))

          } # end of type loop
        } # end of single dose loop
      } # end if single dose list exists

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step05.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step05.rds"))
      }

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_step06.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS,"RI_MOV_step06.rds"))
      }

      # **************************************************************************
      # For MOVs that were later corrected, calculate days from initial MOV to correction
      # **************************************************************************
      type <- c("crude", "valid")

      for(d in seq_along(RI_DOSE_LIST)){
        for(t in seq_along(type)){
          mov <- rlang::sym(paste0("mov_", RI_DOSE_LIST[d], "_", type[t]))
          flagcor <- rlang::sym(paste0("flag_cor_mov_", RI_DOSE_LIST[d], "_", type[t]))
          doseage <- rlang::sym(paste0("age_at_", RI_DOSE_LIST[d], "_", type[t]))

          # Calculate days between this MOV and the date it was corrected
          dat <- dat %>%
            mutate(tempvar1 = ifelse((!!mov %in% 1 & !!flagcor %in% 1) %in% TRUE,
                                     (!!doseage - age), NA))

          # Save the max extra days (# of days from first MOV until it was corrected)
          dat <- dat %>%
            group_by(person) %>%
            arrange(person) %>%
            mutate(tempvar2 = max(tempvar1, na.rm = TRUE)) %>%
            ungroup() %>% suppressWarnings()

          dat <- dat %>%
            mutate(tempvar2 = ifelse(tempvar2 == Inf | tempvar2 == -Inf,
                                     NA, tempvar2))
          dat <- dat %>% select(-c(tempvar1))
          names(dat)[which(names(dat) == "tempvar2")] <- paste0("days_until_cor_",RI_DOSE_LIST[d],"_",type[t])
          #label variable days_until_cor_`d'_`t' "Days b/t 1st MOV & correction: `d' - `t'"

        } #end of type loop
      } #end of dose loop

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step06b.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_step06b.rds"))
      }

      # ********************************************************************************
      # Loop over all doses that will be listed in MOV output
      # and calculate a final set of derived variables
      # ********************************************************************************

      # The user may specify a global that lists a subset of doses to be included
      # in MOV tables and figures and in the summaries of how many eligible visits
      # were made and whether the child recieved and MOV for any dose
      #
      # By 'any dose' we mean 'any dose' in the macro named MOV_OUTPUT_DOSE_LIST
      #
      # Note that if the user does not specify MOV_OUTPUT_DOSE_LIST, VCQI sets it
      # equal to RI_DOSE_LIST in the program check_RI_analysis_metadata

      dat <- dat %>% mutate(elig_for_anydose_crude = 0,
                            elig_for_anydose_valid = 0,
                            mov_for_anydose_crude = 0,
                            mov_for_anydose_valid = 0,
                            total_visit_movs_crude = 0,
                            total_visit_movs_valid = 0)
      ##################################
      ####place holder for var label####
      ##################################

      # Replace this global with the lower-case version in case the
      # user mistakenly used upper case
      vcqi_global(MOV_OUTPUT_DOSE_LIST, str_to_lower(MOV_OUTPUT_DOSE_LIST))

      type <- c("crude", "valid")
      for(d in seq_along(MOV_OUTPUT_DOSE_LIST)){
        for(t in seq_along(type)){

          eval(parse_expr(paste0(
            "dat <- mutate(dat, elig_for_anydose_", type[t],
            " = if_else(elig_", MOV_OUTPUT_DOSE_LIST[d],  "_",type[t],
            " %in% 1, 1, elig_for_anydose_", type[t], "))")))

          eval(parse_expr(paste0(
            "dat <- mutate(dat, mov_for_anydose_", type[t],
            " = if_else(mov_", MOV_OUTPUT_DOSE_LIST[d], "_",type[t],
            " %in% 1, 1, mov_for_anydose_", type[t], "))")))

          eval(parse_expr(paste0(
            "dat <- mutate(dat, total_visit_movs_", type[t],
            " = if_else(mov_", MOV_OUTPUT_DOSE_LIST[d], "_", type[t],
            " %in% 1, total_visit_movs_", type[t],
            " + 1, total_visit_movs_", type[t], "))")))

        } # end of type loop
      } # end of dose loop

      for(t in seq_along(type)){
        elig <- rlang::sym(paste0("elig_for_anydose_", type[t]))
        mov <- rlang::sym(paste0("mov_for_anydose_", type[t]))
        total <- rlang::sym(paste0("total_visit_movs_", type[t]))
        dat <- dat %>%
          group_by(person) %>%
          arrange(person) %>%
          mutate(!!paste0("total_elig_visits_", type[t]) :=
                   sum(!!elig, na.rm = TRUE),
                 !!paste0("total_mov_visits_", type[t]) :=
                   sum(!!mov, na.rm = TRUE),
                 !!paste0("total_movs_", type[t]) :=
                   sum(!!total, na.rm = TRUE)) %>% ungroup()

        #label variable total_elig_visits_`t' "Total visits eligible for 1+ doses - `t'"
        #label variable total_mov_visits_`t'  "Total visits with MOVs - `t'"
        #label variable total_movs_`t'        "Total MOVs - `t'"
      }

      dat <- dat %>% relocate(
        c(elig_for_anydose_crude, mov_for_anydose_crude, total_visit_movs_crude,
          total_elig_visits_crude, total_mov_visits_crude, total_movs_crude,
          elig_for_anydose_valid, mov_for_anydose_valid, total_visit_movs_valid,
          total_elig_visits_valid, total_mov_visits_valid, total_movs_valid),
        .after = last_col()) %>%
        select(-c(starts_with("age_")))

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step07.rds"))
        vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS,"RI_MOV_step07.rds"))
      }

      # Add variable labels
      type <- c("crude", "valid")

      for(ty in seq_along(type)){
        for(d in seq_along(RI_DOSE_LIST)){

          credit_var <- rlang::sym(paste0("credit_", RI_DOSE_LIST[d], "_", type[ty]))
          elig_var <- rlang::sym(paste0("elig_", RI_DOSE_LIST[d], "_", type[ty]))
          flag_got_var <- rlang::sym(paste0("flag_got_", RI_DOSE_LIST[d], "_", type[ty]))
          cum_var <- rlang::sym(paste0("cum_", RI_DOSE_LIST[d], "_", type[ty]))
          mov_var <- rlang::sym(paste0("mov_", RI_DOSE_LIST[d], "_", type[ty]))
          cum_mov_var <- rlang::sym(paste0("cum_mov_", RI_DOSE_LIST[d], "_", type[ty]))
          cor_mov_var <- rlang::sym(paste0("cor_mov_", RI_DOSE_LIST[d], "_", type[ty]))
          flag_cor_mov_var <- rlang::sym(paste0("flag_cor_mov_", RI_DOSE_LIST[d], "_", type[ty]))
          total_mov_var <- rlang::sym(paste0("total_mov_", RI_DOSE_LIST[d], "_", type[ty]))
          total_elig_var <- rlang::sym(paste0("total_elig_", RI_DOSE_LIST[d], "_", type[ty]))
          flag_had_mov_var <- rlang::sym(paste0("flag_had_mov_", RI_DOSE_LIST[d], "_", type[ty]))
          flag_uncor_mov_var <- rlang::sym(paste0("flag_uncor_mov_", RI_DOSE_LIST[d], "_", type[ty]))
          days_until_cor_var <- rlang::sym(paste0("days_until_cor_", RI_DOSE_LIST[d], "_", type[ty]))

          dat <- dat %>%
            mutate(
              !!credit_var := haven::labelled(
                as.numeric(!!credit_var),
                label = paste0("Would give credit for a valid dose of ",
                               RI_DOSE_LIST[d], " in this visit - ", type[ty])),
              !!elig_var := haven::labelled(
                as.numeric(!!elig_var),
                label = paste0("Eligible for ", RI_DOSE_LIST[d],
                               " in this visit - ", type[ty])),
              !!flag_got_var := haven::labelled(
                as.numeric(!!flag_got_var),
                label = paste0("Received ", RI_DOSE_LIST[d],
                               " in this visit - ", type[ty])),
              !!cum_var := haven::labelled(
                as.numeric(!!cum_var),
                label = paste0("Cumulative doses of ", RI_DOSE_LIST[d],
                               " received up-to-and-including this visit - ", type[ty])),
              !!mov_var := haven::labelled(
                as.numeric(!!mov_var),
                label = paste0("Experienced an MOV for ", RI_DOSE_LIST[d],
                               " in this visit - ", type[ty])),
              !!cum_mov_var := haven::labelled(
                as.numeric(!!cum_mov_var),
                label = paste0("Cumulative MOVs for ", RI_DOSE_LIST[d],
                               " thru this visit - ", type[ty])),
              !!cor_mov_var := haven::labelled(
                as.numeric(!!cor_mov_var),
                label = paste0("Experienced a corrected MOV for ",
                               RI_DOSE_LIST[d], " - ", type[ty])),
              !!flag_cor_mov_var := haven::labelled(
                as.numeric(!!flag_cor_mov_var),
                label = paste0("Experienced 1+ corrected MOVs for ",
                               RI_DOSE_LIST[d], " - ", type[ty])),
              !!total_mov_var := haven::labelled(
                as.numeric(!!total_mov_var),
                label = paste0("Total MOVs for ", RI_DOSE_LIST[d], " - ", type[ty])),
              !!total_elig_var := haven::labelled(
                as.numeric(!!total_elig_var),
                label = paste0("Total visits where eligible to receive ",
                               RI_DOSE_LIST[d], " - ", type[ty])),
              !!flag_had_mov_var := haven::labelled(
                as.numeric(!!flag_had_mov_var),
                label = paste0("Had 1+ MOVs for ", RI_DOSE_LIST[d],
                               " in any visit - ", type[ty])),
              !!flag_uncor_mov_var := haven::labelled(
                as.numeric(!!flag_uncor_mov_var),
                label = paste0("Experienced 1+ uncorrected MOVs for ",
                               RI_DOSE_LIST[d], " in any visit - ", type[ty])),
              !!days_until_cor_var := haven::labelled(
                as.numeric(!!days_until_cor_var),
                label = paste0("Days b/t 1st MOV & correction: ",
                               RI_DOSE_LIST[d], " - ", type[ty]))
            )

        } # end dose loop

        total_elig_visits_var <- rlang::sym(paste0("total_elig_visits_", type[ty]))
        total_mov_visits_var <- rlang::sym(paste0("total_mov_visits_", type[ty]))
        total_movs_var <- rlang::sym(paste0("total_movs_", type[ty]))

        dat <- dat %>%
          mutate(
            !!total_elig_visits_var := haven::labelled(
              as.numeric(!!total_elig_visits_var),
              label = paste0("Total visits eligible for 1+ doses - ", type[ty])),
            !!total_mov_visits_var := haven::labelled(
              as.numeric(!!total_mov_visits_var),
              label = paste0("Total visits with MOVs - ", type[ty])),
            !!total_movs_var := haven::labelled(
              as.numeric(!!total_movs_var),
              label = paste0("Total MOVs - ", type[ty]))
          )

      } # end type loop

      dat <- dat %>%
        mutate(
          elig_for_anydose_crude = haven::labelled(
            as.numeric(elig_for_anydose_crude),
            label = "Eligible for 1+ doses - crude"),
          elig_for_anydose_valid = haven::labelled(
            as.numeric(elig_for_anydose_valid),
            label = "Eligible for 1+ doses - valid"),
          mov_for_anydose_crude = haven::labelled(
            as.numeric(mov_for_anydose_crude),
            label = "Had MOV for 1+ doses - crude"),
          mov_for_anydose_valid = haven::labelled(
            as.numeric(mov_for_anydose_valid),
            label = "Had MOV for 1+ doses - valid"),
          total_visit_movs_crude = haven::labelled(
            as.numeric(total_visit_movs_crude),
            label ="Total visits with MOVs - crude"),
          total_visit_movs_valid = haven::labelled(
            as.numeric(total_visit_movs_valid),
            label = "Total visits with MOVs - valid")
          # NOTE: do not label DOB variable because it causes type issues downstream
          # dob = haven::labelled(
          #   dob, label = "Date of birth")
        )

      # Create dataset of CVDIMS variables
      # Dataset is 1 row per respondent & contains 3 variables (respid, cvdims_sequence_crude, cvdims_sequence_valid)
      saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_long_form_data.rds"))
      vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_long_form_data.rds"))

      # ********************************************************************************
      # Only keep one row per person

      dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_long_form_data.rds"))
      dat <- dat %>%
        select(-c(age, starts_with("got"),
                  ends_with("str"),
                  visitdate)) %>%
        group_by(person) %>%
        mutate(tempid = row_number()) %>%
        ungroup()
      dat <- subset(dat, tempid == 1)
      dat <- dat %>% select(-c(person, tempid))

      #label variable dob "Date of birth"

      if (VCQI_TESTING_CODE %in% 1){
        saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_step08.rds"))
        vcqi_global(RI_TEMP_DATASETS,c(RI_TEMP_DATASETS, "RI_MOV_step08.rds"))
      }

      saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_flags_to_merge.rds"))
      vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_MOV_flags_to_merge.rds"))

    } # end of if VCQI_NO_DOBS
  } # end of if VCQI_CHECK_INSTEAD_OF_RUN

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
