#' Calculate derived variables for RI_COVG_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_02_<ANALYSIS_COUNTER>)
#'
#' @importFrom vctrs vec_as_names
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import stringr
#' @import tidyselect

# RI_COVG_02_03DV R version 1.06 - Biostat Global Consulting - 2022-12-05
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-10  1.00      Mia Yu          Original R version
# 2022-08-16  1.01      Caitlin Clary   Adding multi-dose
# 2022-08-26  1.02      Caitlin Clary   Suppress "New names" message from bind_cols
# 2022-09-01            Caitlin Clary   Address which_valid bug for multidose
#                                       series (doses 2+)
# 2022-09-21  1.03      Mia Yu          Fix NA not in date format problem
# 2022-09-22  1.04      Caitlin Clary   Make robust when no single dose list
# 2022-10-11  1.05      Mia Yu          Package version
# 2022-12-05  1.06      Caitlin Clary   Variable labels
# *******************************************************************************

RI_COVG_02_03DV <- function(VCP = "RI_COVG_02_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", ANALYSIS_COUNTER,".rds"))

  type <- c("card", "register")

  # Define function: quiet version of bind_cols (no "new names" messages)
  vcqi_bind_cols <- function(
    ...,
    .name_repair = c("unique", "universal", "check_unique", "minimal")){
    bind_cols(...,
              .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
  }

  # Create new age_dose_received variables
  for(s in seq_along(type)){
    for(d in seq_along(RI_DOSE_LIST)){
      date <- rlang::sym(paste0(RI_DOSE_LIST[d], "_", type[s], "_date"))

      dat <- dat %>%
        mutate(
          !!paste0("age_at_", RI_DOSE_LIST[d], "_", type[s]) := ifelse(
            !is.na(!!date),
            !!date - dob_for_valid_dose_calculations,
            as.difftime("NA")))

      #label variable age_at_`d'_`s' "Age when `d' was received on `s'"

    } # end of dose loop
  } # end of type loop

  # Generate variables to determine if the dose was received in a valid timeframe
  if(vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
    single_dose <- str_to_lower(RI_SINGLE_DOSE_LIST)
    for(s in seq_along(type)){
      for(d in seq_along(single_dose)){
        # Assume age_at_`d' was calculated using a valid dob and valid vaccination date.
        # Assume min_age scalar already in memory

        age <- rlang::sym(paste0("age_at_", single_dose[d], "_", type[s]))
        min_age <- get(paste0(single_dose[d],"_min_age_days"), envir = .GlobalEnv)
        dat <- dat %>% mutate(tempvar1 = ifelse(
          (!!age >= min_age & !is.na(!!age)) %in% TRUE, 1, 0
        ))
        #label variable got_valid_`d'_by_`s' "Valid dose received for `d' on `s'"

        # Occasionally users will specify a maximum age at which a dose is
        # considered valid (e.g., for one of the birth doses) so check
        # here to see if they have specified a max; if so, enforce it

        if (vcqi_object_exists(paste0(single_dose[d], "_max_age_days"))){
          max_age <- get(paste0(single_dose[d],"_max_age_days"), envir = .GlobalEnv)
          dat <- dat %>% mutate(tempvar1 = ifelse((!!age > max_age & !is.na(!!age)) %in% TRUE, 0, tempvar1))
        }

        # For (uncommon) doses where the age of dose-eligibility is older than
        # the age of survey-eligibility (e.g., 2nd dose of measles in 2nd
        # year of life) remove respondents from the denominator if they are
        # not clearly age-eligible for the dose
        dat <- dat %>%
          mutate(
            tempvar1 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                               & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                              NA, tempvar1),
            tempvar1 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar1)
          )

        names(dat)[which(names(dat) == "tempvar1")] <- paste0("got_valid_", single_dose[d], "_by_", type[s])

      } # end of single dose loop
    } # end of type loop
  }
  # Multi-dose series: got_valid and which_valid variables ----

  # Summarize multi-dose lists
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

    for(s in seq_along(type)){
      for(d in seq_along(multi$dose)){

        dn <- multi$dose[d]
        di <- seq(1, multi$dosecount[d], by = 1)

        # Set up variables
        # Valid dose received for <dose> by <source>
        dat[paste0("got_valid_", dn, di, "_by_", type[s])] <- 0
        # Dose number for valid <dose> by <source>
        dat[paste0("which_valid_", dn, di, "_by_", type[s])] <- NA

        # Identify the *first* valid dose in the series

        for(i in seq_along(di)){
          wv <- rlang::sym(paste0("which_valid_", dn, "1_by_", type[s]))
          ageat <- rlang::sym(paste0("age_at_", dn, rev(di)[i], "_", type[s]))
          agemin <- get(paste0(dn, "1_min_age_days"), envir = .GlobalEnv)

          dat <- dat %>%
            mutate(
              !!wv := ifelse(!!ageat >= agemin & !is.na(!!ageat), rev(di)[i], !!wv)
            )
        } # end of i loop

        # Subsequent doses in the series
        for(a in 2:length(di)){

          # Which recorded dose = valid dose a
          wv <- rlang::sym(paste0("which_valid_", dn, di[a], "_by_", type[s]))

          # Interval for dose a
          intmin <- get(paste0(dn, di[a], "_min_interval_days"),
                        envir = .GlobalEnv)

          # Variable name: which valid for previous dose
          wvp <- rlang::sym(paste0("which_valid_", dn, di[a-1], "_by_", type[s]))
          # Need to reference age at previous valid dose, so create a temp var
          # Names of all age_at_<dose#>_<source> variables
          varlist <- lapply(seq_along(di), function(x) rlang::sym(
            paste0("age_at_", dn, x, "_", type[s])))

          # If which_valid = X, pull age from age_at_<doseX> column
          # Result: y columns (y = number of doses in series) bound together
          tempdat <- lapply(seq_along(di), function(x) mutate(
            dat,
            temp = ifelse(!is.na(!!wvp) & !!wvp %in% x, !!varlist[[x]], NA)
          ) %>% select(temp)
          ) %>% do.call(vcqi_bind_cols, .)

          # Coalesce the age variables created above (which are mutually exclusive)
          tempdat <- tempdat %>%
            mutate(var = coalesce(!!! select(., everything())))

          dat$temp_age_at_previous_valid <- tempdat$var

          # Starting with the last dose in the sequence, see if that dose would be a valid
          # a'th dose in the series
          for(j in length(di):a){

            # Age when dose j was received
            ageat <- rlang::sym(paste0("age_at_", dn, di[j], "_", type[s]))

            dat <- dat %>%
              mutate(
                !!wv := ifelse(!!ageat >= temp_age_at_previous_valid + intmin &
                                 !is.na(!!ageat) & !is.na(temp_age_at_previous_valid) &
                                 !(!!wvp %in% j), # dose j already assigned to previous dose
                               j, !!wv
                )
              )
          } # end j loop
        } # end a loop

        for(i in seq_along(di)){
          gv <- rlang::sym(paste0("got_valid_", dn, i, "_by_", type[s]))
          wv <- rlang::sym(paste0("which_valid_", dn, i, "_by_", type[s]))
          agemin <- get(paste0(dn, i, "_min_age_days"), envir = .GlobalEnv)

          dat <- dat %>%
            mutate(
              !!gv := ifelse(!is.na(!!wv) & !!wv > 0, 1, !!gv),
              # For (uncommon) doses where the age of dose eligibility is older than
              # the age of survey eligibility, e.g. 2nd dose of measles in 2nd
              # year of life, remove respondents from the denominator if they are
              # not clearly age-eligible for the dose
              !!gv := ifelse(agemin > VCQI_RI_MIN_AGE_OF_ELIGIBILITY &
                               (is.na(age_at_interview) | age_at_interview < agemin),
                             NA, !!gv),
              !!gv := ifelse(psweight == 0 | is.na(psweight), NA, !!gv)
            )

        } # end i loop

      } # end d loop
    } # end s loop

    dat <- select(dat, -temp_age_at_previous_valid)
  } # end if(!is.null(multi))

  # Generate the variable for valid dose by card or register
  # Generate the variable to analyze, based on whether & how
  # RI records were sought at the health centers

  # Generate the age at valid dose by card and register variables here
  for(s in seq_along(type)){

    if(vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
      single_dose <- str_to_lower(RI_SINGLE_DOSE_LIST)

      # Loop through single doses
      for(d in seq_along(single_dose)){
        age <- rlang::sym(paste0("age_at_", single_dose[d], "_", type[s]))
        valid <- rlang::sym(paste0("got_valid_", single_dose[d], "_by_", type[s]))

        dat <- dat %>% mutate(tempvar1 = ifelse(!!valid %in% 1, !!age, NA))
        #label variable age_at_valid_`d'_`s' "Age at valid `d' by `s'"

        min_age <- get(paste0(single_dose[d],"_min_age_days"), envir = .GlobalEnv)
        dat <- dat %>%
          mutate(tempvar1 = ifelse(
            ((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
             & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
            NA, tempvar1))
        dat <- dat %>%
          mutate(tempvar1 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar1))

        names(dat)[which(names(dat) == "tempvar1")] <- paste0("age_at_valid_", single_dose[d], "_", type[s])
      } # end of d loop for single dose
    }
    # Note that for multi-dose vaccines, early doses may have been invalid
    # so the date at valid Xn might be the date when they received dose
    # X(n+1) or X(n+2); loop over j and use the which_valid_Xn_by_s
    # variable to sort this out properly

    # Loop through multi-dose vaccines
    if(!is.null(multi)){
      for(d in seq_along(multi$dose)){

        # Dose name
        dn <- multi$dose[d]
        # Dose numbers
        di <- seq(1, multi$dosecount[d], by = 1)

        # Loop through doses
        for(i in seq_along(di)){
          ageat <- rlang::sym(paste0("age_at_valid_", dn, i, "_", type[s]))
          wv <- rlang::sym(paste0("which_valid_", dn, i, "_by_", type[s]))

          # Names of all age_at_<dose#>_<source> variables
          varlist <- lapply(seq_along(di), function(x) rlang::sym(
            paste0("age_at_", dn, x, "_", type[s])))

          # If which_valid = X, pull age from age_at_<doseX> column
          # Result: y columns (y = number of doses in series) bound together
          age_at_doses <- lapply(seq_along(di), function(x) mutate(
            dat,
            temp = ifelse(!is.na(!!wv) & !!wv %in% x, !!varlist[[x]], NA)
          ) %>% select(temp)
          ) %>% do.call(vcqi_bind_cols, .)

          # Coalesce the age variables created above (which are mutually exclusive)
          age_at_doses <- age_at_doses %>%
            mutate(var = coalesce(!!! select(., everything())))

          # Add that coalesced variable as the age_at_valid_<doseX>_<source> variable
          dat <- dat %>% mutate(!!ageat := age_at_doses$var)

        } # end i loop
      } # end d loop for multi-dose vaccines
    } # end if(!is.null(multi))

  } # end of type loop


  for (d in seq_along(RI_DOSE_LIST)){

    bycard <- rlang::sym(paste0("got_valid_", RI_DOSE_LIST[d], "_by_card"))
    byreg  <- rlang::sym(paste0("got_valid_", RI_DOSE_LIST[d], "_by_register"))
    agebycard <- rlang::sym(paste0("age_at_valid_", RI_DOSE_LIST[d], "_card"))
    agebyreg <- rlang::sym(paste0("age_at_valid_", RI_DOSE_LIST[d], "_register"))
    min_age <- get(paste0(RI_DOSE_LIST[d], "_min_age_days"),  envir = .GlobalEnv)

    dat <- dat %>%
      mutate(tempvar1 = ifelse((!!bycard %in% 1 | !!byreg %in% 1) %in% TRUE, 1, 0))
    #label variable got_valid_`d'_c_or_r "Either card or register indicate `d' was valid."

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1){
      #if either card or register say it is valid dose count as valid assign value from card initially
      dat <- dat %>% mutate(tempvar2 = ifelse((!!bycard %in% 1 | !!byreg %in% 1) %in% TRUE, 1, 0))
      #label variable got_valid_`d'_to_analyze "Received valid dose for `d'- RI_RECORDS_SOUGHT_FOR_ALL"
      dat <- dat %>% mutate(tempvar3 = ifelse(!!bycard %in% 1, !!agebycard, NA))
      # replace with value from register if card is missing or register is valid
      dat <- dat %>% mutate(tempvar3 = ifelse((!(!!bycard %in% 1) & !!byreg %in% 1) %in% TRUE, !!agebyreg, tempvar3))
    } else if (RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      #only use register if card is missing
      dat <- dat %>% mutate(tempvar2 = !!bycard)
      #label variable got_valid_`d'_to_analyze "Received valid dose for `d'- RI_RECORDS_SOUGHT_IF_NO_CARD"
      dat <- dat %>% mutate(tempvar3 = ifelse(!!bycard %in% 1, !!agebycard, NA))
      #label variable age_at_valid_`d' "Age received valid dose for `d'- RI_RECORDS_SOUGHT_IF_NO_CARD"

      # replace with value from register if card is missing or register is valid
      dat <- dat %>% mutate(tempvar2 = ifelse(no_card %in% 1, !!byreg, tempvar2),
                            tempvar3 = ifelse((no_card %in% 1 & !!byreg %in% 1) %in% TRUE, !!agebyreg,tempvar3))
    } else if (RI_RECORDS_NOT_SOUGHT == 1){
      dat <- dat %>% mutate(tempvar2 = !!bycard)
      #label variable got_valid_`d'_to_analyze "Received valid dose for `d'- RI_RECORDS_NOT_SOUGHT"
      dat <- dat %>% mutate(tempvar3 = ifelse(!!bycard %in% 1, !!agebycard, NA))
      #label variable age_at_valid_`d' "Age received valid dose for `d'- RI_RECORDS_NOT_SOUGHT"
    }

    dat <- dat %>% mutate(tempvar1 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                                            NA, tempvar1))
    dat <- dat %>% mutate(tempvar1 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar1))
    dat <- dat %>% mutate(tempvar2 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                                            NA, tempvar2))
    dat <- dat %>% mutate(tempvar2 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar2))
    dat <- dat %>% mutate(tempvar3 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                             & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                                            NA, tempvar3))
    dat <- dat %>% mutate(tempvar3 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar3))

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("got_valid_",RI_DOSE_LIST[d],"_c_or_r")
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("got_valid_",RI_DOSE_LIST[d],"_to_analyze")
    names(dat)[which(names(dat) == "tempvar3")] <- paste0("age_at_valid_",RI_DOSE_LIST[d])

  }# end of dose loop

  # Generate variable to determine if vaccine was received by their first birthday
  for (s in seq_along(type)){
    for (d in seq_along(RI_DOSE_LIST)){
      valid <- rlang::sym(paste0("got_valid_",RI_DOSE_LIST[d],"_by_",type[s]))
      age <- rlang::sym(paste0("age_at_",RI_DOSE_LIST[d],"_",type[s]))
      min_age <- get(paste0(RI_DOSE_LIST[d],"_min_age_days"), envir = .GlobalEnv)

      dat <- dat %>% mutate(tempvar1 = ifelse((!!valid %in% 1 & !!age <= 365) %in% TRUE, 1, 0))

      dat <- dat %>% mutate(tempvar1 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                                               & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                                              NA, tempvar1))
      dat <- dat %>% mutate(tempvar1 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar1))

      names(dat)[which(names(dat) == "tempvar1")] <- paste0("valid_",RI_DOSE_LIST[d],"_age1_",type[s])
    }# end of dose loop
  }# end of type loop

  # Generate variable to indicate if the vaccine was valid and timely on either card or register
  for (d in seq_along(RI_DOSE_LIST)){
    age1card <- rlang::sym(paste0("valid_",RI_DOSE_LIST[d],"_age1_card"))
    age1reg  <- rlang::sym(paste0("valid_",RI_DOSE_LIST[d],"_age1_register"))
    min_age <- get(paste0(RI_DOSE_LIST[d],"_min_age_days"), envir = .GlobalEnv)

    dat <- dat %>% mutate(tempvar1 = ifelse((!!age1card %in% 1 | !!age1reg %in% 1) %in% TRUE, 1, 0))
    # label variable valid_`d'_age1_c_or_r "Received valid `d' by age 1, by card or register"

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1){
      #if either card or register say it is valid dose count as valid
      dat <- dat %>% mutate(tempvar2 = ifelse((!!age1card %in% 1 | !!age1reg %in% 1) %in% TRUE, 1, 0))
      #label variable valid_`d'_age1_to_analyze "Received valid dose for `d' by age 1- RI_RECORDS_SOUGHT_FOR_ALL"

    } else if (RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
      #only use register if card is missing
      dat <- dat %>% mutate(tempvar2 = ifelse((!!age1card %in% 1 | (no_card %in% 1 & !!age1reg %in% 1)) %in% TRUE, 1, 0))
      #label variable valid_`d'_age1_to_analyze "Received valid dose for `d' by age 1- RI_RECORDS_SOUGHT_IF_NO_CARD"

    } else if (RI_RECORDS_NOT_SOUGHT == 1){
      dat <- dat %>% mutate(tempvar2 = !!age1card)
      #label variable valid_`d'_age1_to_analyze "Received valid dose for `d' by age 1- RI_RECORDS_NOT_SOUGHT "
    }

    dat <- dat %>% mutate(
      tempvar1 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                         & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                        NA, tempvar1),
      tempvar1 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar1),
      tempvar2 = ifelse(((min_age > VCQI_RI_MIN_AGE_OF_ELIGIBILITY)
                         & (is.na(age_at_interview)| (age_at_interview < min_age))) %in% TRUE,
                        NA, tempvar2),
      tempvar2 = ifelse((psweight == 0 | is.na(psweight)) %in% TRUE, NA, tempvar2)
    )

    names(dat)[which(names(dat) == "tempvar1")] <- paste0("valid_",RI_DOSE_LIST[d],"_age1_c_or_r")
    names(dat)[which(names(dat) == "tempvar2")] <- paste0("valid_",RI_DOSE_LIST[d],"_age1_to_analyze")

  } # end of dose loop

  # Variable labelling ----

  for(s in seq_along(type)){
    for(d in seq_along(RI_DOSE_LIST)){

      age_at_var <- rlang::sym(paste0("age_at_", RI_DOSE_LIST[d], "_", type[s]))

      dat <- dat %>%
        mutate(
          # Ensure numeric
          !!age_at_var := as.numeric(!!age_at_var),
          # Add labels
          !!age_at_var := haven::labelled(
            !!age_at_var,
            label = paste0("Age when ", RI_DOSE_LIST[d], " was received on ", type[s]))
        )

    } # end RI dose loop


    for(d in seq_along(single_dose)){

      got_valid_single <- rlang::sym(paste0("got_valid_", single_dose[d], "_by_", type[s]))
      age_valid_single <- rlang::sym(paste0("age_at_valid_", single_dose[d], "_", type[s]))

      dat <- dat %>%
        mutate(
          # Ensure numeric
          !!got_valid_single := as.numeric(!!got_valid_single),
          !!age_valid_single := as.numeric(!!age_valid_single),
          # Add labels
          !!got_valid_single := haven::labelled(
            !!got_valid_single,
            label = paste0("Valid dose received for ", single_dose[d], " on ", type[s])),
          !!age_valid_single := haven::labelled(
            !!age_valid_single,
            label = paste0("Age at valid ", single_dose[d], " by ", type[s])
          )
        )

    } # end single dose loop

  } # end of type loop

  if(!is.null(multi)){

    for(s in seq_along(type)){
      for(d in seq_along(multi$dose)){

        dn <- multi$dose[d]
        di <- seq(1, multi$dosecount[d], by = 1)

        # Loop through doses
        for(i in seq_along(di)){

          got_valid_multi <- rlang::sym(paste0("got_valid_", dn, i, "_by_", type[s]))
          ageat <- rlang::sym(paste0("age_at_valid_", dn, i, "_", type[s]))
          wv <- rlang::sym(paste0("which_valid_", dn, i, "_by_", type[s]))

          dat <- dat %>%
            mutate(
              # Ensure numeric
              !!got_valid_multi := as.numeric(!!got_valid_multi),
              !!wv := as.numeric(!!wv),
              !!ageat := as.numeric(!!ageat),
              # Add labels
              !!got_valid_multi := haven::labelled(
                !!got_valid_multi,
                label = paste0("Valid dose received for ", dn, i, " on ", type[s])
              ),
              !!wv := haven::labelled(
                !!wv,
                label = paste0("Dose number valid for ", dn, i, " on ", type[s])
              ),
              !!ageat := haven::labelled(
                !!ageat,
                label = paste0("Age at valid ", dn, i, " by ", type[s])
              ))
        } # end i loop

        # label variable got_valid_`d'1_by_`s' "Valid dose received for `d'1 on `s'"
        # label variable which_valid_`d'1_by_`s' "Dose number valid for `d'1 on `s'"
        # label variable age_at_valid_`d'`i'_`s' "Age at valid `d'`i' by `s'"


      } # end multi dose loop
    } # end s (type) loop

  } # end if !is.null(multi)

  for (d in seq_along(RI_DOSE_LIST)){

    c_or_r <- rlang::sym(paste0("got_valid_", RI_DOSE_LIST[d], "_c_or_r"))
    got_valid_ta <- rlang::sym(paste0("got_valid_", RI_DOSE_LIST[d], "_to_analyze"))
    age_at_valid <- rlang::sym(paste0("age_at_valid_", RI_DOSE_LIST[d]))

    c_or_r_age1 <- rlang::sym(paste0("valid_", RI_DOSE_LIST[d], "_age1_c_or_r"))
    valid_age1 <- rlang::sym(paste0("valid_", RI_DOSE_LIST[d], "_age1_to_analyze"))

    dat <- dat %>%
      mutate(
        # Ensure numeric
        !!c_or_r := as.numeric(!!c_or_r),
        !!c_or_r_age1 := as.numeric(!!c_or_r_age1),
        # Add labels
        !!c_or_r := haven::labelled(
          !!c_or_r,
          label = paste0("Either card or register indicate ", RI_DOSE_LIST[d], " was valid")
        ),
        !!c_or_r_age1 := haven::labelled(
          !!c_or_r_age1,
          label = paste0("Received valid dose for ", RI_DOSE_LIST[d], " by age 1, by card or register")
        )
      )

    if (RI_RECORDS_SOUGHT_FOR_ALL == 1){

      dat <- dat %>%
        mutate(
          # Ensure numeric
          !!got_valid_ta := as.numeric(!!got_valid_ta),
          !!age_at_valid := as.numeric(!!age_at_valid),
          !!valid_age1 := as.numeric(!!valid_age1),
          # Add labels
          !!got_valid_ta := haven::labelled(
            !!got_valid_ta,
            label = paste0("Received valid dose for ", RI_DOSE_LIST[d],
                           " - RI_RECORDS_SOUGHT_FOR_ALL")),
          !!age_at_valid := haven::labelled(
            !!age_at_valid,
            label = paste0("Age received valid dose for ", RI_DOSE_LIST[d],
                           " - RI_RECORDS_SOUGHT_FOR_ALL")
          ),
          !!valid_age1 := haven::labelled(
            !!valid_age1,
            label = paste0("Received valid dose for ", RI_DOSE_LIST[d],
                           " by age 1 - RI_RECORDS_SOUGHT_FOR_ALL")
          )
        )

    } else if (RI_RECORDS_SOUGHT_IF_NO_CARD == 1){

      dat <- dat %>%
        mutate(
          # Ensure numeric
          !!got_valid_ta := as.numeric(!!got_valid_ta),
          !!age_at_valid := as.numeric(!!age_at_valid),
          !!valid_age1 := as.numeric(!!valid_age1),
          # Add labels
          !!got_valid_ta := haven::labelled(
            !!got_valid_ta,
            label = paste0("Received valid dose for ", RI_DOSE_LIST[d],
                           " - RI_RECORDS_SOUGHT_IF_NO_CARD")),
          !!age_at_valid := haven::labelled(
            !!age_at_valid,
            label = paste0("Age received valid dose for ", RI_DOSE_LIST[d],
                           " - RI_RECORDS_SOUGHT_IF_NO_CARD")
          ),
          !!valid_age1 := haven::labelled(
            !!valid_age1,
            label = paste0("Received valid dose for ", RI_DOSE_LIST[d],
                           " by age 1 - RI_RECORDS_SOUGHT_IF_NO_CARD")
          )
        )

    } else if (RI_RECORDS_NOT_SOUGHT == 1){

      dat <- dat %>%
        mutate(
          # Ensure numeric
          !!got_valid_ta := as.numeric(!!got_valid_ta),
          !!age_at_valid := as.numeric(!!age_at_valid),
          !!valid_age1 := as.numeric(!!valid_age1),
          # Add labels
          !!got_valid_ta := haven::labelled(
            !!got_valid_ta,
            label = paste0("Received valid dose for ", RI_DOSE_LIST[d],
                           " - RI_RECORDS_NOT_SOUGHT")),
          !!age_at_valid := haven::labelled(
            !!age_at_valid,
            label = paste0("Age received valid dose for ", RI_DOSE_LIST[d],
                           " - RI_RECORDS_NOT_SOUGHT")
          ),
          !!valid_age1 := haven::labelled(
            !!valid_age1,
            label = paste0("Received valid dose for ", RI_DOSE_LIST[d],
                           " by age 1 - RI_RECORDS_NOT_SOUGHT")
          )
        )

    }

  } # end of dose loop

  # Save data and end ----

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", ANALYSIS_COUNTER,".rds"))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

