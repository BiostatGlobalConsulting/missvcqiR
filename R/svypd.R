#' Calculate a proportion, confidence interval, and other quantities using survey data
#'
#' @param svydf Survey design object
#' @param var Indicator variable, taking values 0, 1, or NA
#' @param subset_condition Optional condition for subsetting the data
#' @param ci_level Confidence level, 100 - alpha. User may specify ci_level or ci_level_list, but not both.
#' @param ci_level_list List of confidence levels, e.g. c(90, 95). Svypd will calculate 2-sided limits for as many levels as specified here. User may specify ci_level or ci_level_list, but not both.
#' @param ci_method Confidence interval calculation method. May be Logit, Agresti-Coull, Clopper-Pearson, Fleiss, Jeffreys, Wilson, or Wald.
#' @param adjust Adjust effective sample size for confidence interval calculations, per Dean and Pagano (2015) and Korn and Graubard (1998).
#' @param truncate If TRUE then the design effect (DEFF) is not allowed to be lower than 1.
#'
#' @return A dataset with a survey-adjusted proportion estimate, confidence interval(s), and other parameters
#'
#' @import survey
#' @rawNamespace import(rlang, except = c(local_options, with_options))
#' @import dplyr

# svypd R version 1.05 - Biostat Global Consulting - 2023-01-09
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-25  1.00      Caitlin Clary   Original R version
# 2022-10-07  1.01      Mia Yu          Package version
# 2022-10-13  1.02      Caitlin Clary   Procedure when outcome is uniformly 0 or
#                                       1; simplify phat/se calculation; add
#                                       survey.lonely.psu process
# 2022-11-09  1.03      Caitlin Clary   Add null result sections
# 2022-11-15  1.04      Caitlin Clary   Export neff
# 2023-01-09  1.05      Caitlin Clary   Hide zero weight warning from svyciprop
# *******************************************************************************

svypd <- function(
    svydf,                     # Survey design object
    var,                       # Indicator variable, takes values 0, 1, or NA
    subset_condition = NULL,   # Logic to subset svydf, e.g. "varx == 1"
    ci_level = 95,             # Confidence level, 100 - alpha
    ci_level_list = NULL,      # List of confidence levels, e.g. c(90, 95), to generate multiple CIs
    ci_method = "Wilson",      # Confidence interval type
    adjust = TRUE,             # Adjust effective sample size for CI calculations
    truncate = TRUE            # Truncate DEFF (not allowed to be less than 1)
){

  # Inside function, set survey design handling of lone PSUs
  # User's setting is restored at the end of the function
  save_user_survey_option <- options("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")

  dat_full <- svydf

  # Halt if variable of interest not in svydf
  if (!any(names(dat_full$variables) == var)){
    stop(paste0("Variable ", var, " not found in ", svydf, "."))
  }

  # Halt if the variable of interest takes values other than 0, 1, or missing
  outcome <- get(var, dat_full$variables)
  if (any(! outcome %in% c(0, 1, NA))){

    vcqi_log_comment(VCP, 1, "Error",
                     paste0("To use svypd, the variable ", var,
                            " should contain only 0s, 1s, and missing values."))

    vcqi_global(VCQI_ERROR, 1)

    vcqi_halt_immediately(
      halt_message = paste0(
        "To use svypd, the variable ", var,
        " should contain only 0s, 1s, and missing values.")
    )
  }

  # Halt if there are no 0s or 1s in the variable of interest
  if (all(is.na(outcome))){

    # svyp_null_result

    ci_levels_svyp_null_result <- ifelse(!is.null(ci_level_list), ci_level_list, ci_level)

    out <- data.frame(
      estimate = NA,
      stderr = NA,
      cilevel = ci_levels_svyp_null_result,
      cill = NA,
      ciul = NA,
      lcb = NA,
      ucb = NA,
      deff = NA,
      icc = NA,
      n = 0,
      nwtd = 0,
      nclusters = 0,
      nwtd_est = NA
    )

  } else {

    # Create subset data using subset_condition
    if (!is.null(subset_condition)){
      dat <- subset(dat_full, eval(rlang::parse_expr(subset_condition)))
    } else {
      dat <- dat_full
    }

    dat$variables$outcome <- get(var, dat$variables)

    # Fail gracefully if no observations meet the subset condition
    if (nrow(dat) == 0 | all(is.na(get(var, dat$variables)))){

      # svyp_null_result

      ci_levels_svyp_null_result <- ifelse(!is.null(ci_level_list), ci_level_list, ci_level)

      out <- data.frame(
        estimate = NA,
        stderr = NA,
        cilevel = ci_levels_svyp_null_result,
        cill = NA,
        ciul = NA,
        lcb = NA,
        ucb = NA,
        deff = NA,
        icc = NA,
        n = 0,
        nwtd = 0,
        nclusters = 0,
        nwtd_est = NA
      )
    } else {

      # Estimate parameters ----

      # If the variable is 100% 0s or 100% 1s, set phat, se, and nwtd appropriately
      # And if the variable is a mix of 0s and 1s, use output from survey functions

      outcome2 <- get(var, dat$variables)
      outcome2 <- outcome2[!is.na(outcome2)]

      if (length(outcome2[outcome2 == 1]) == length(outcome2) |
          length(outcome2[outcome2 == 0]) == length(outcome2)){

        if (length(outcome2[outcome2 == 1]) == length(outcome2)){
          phat <- 1
          se <- 0
        }

        if (length(outcome2[outcome2 == 0]) == length(outcome2)){
          phat <- 0
          se <- 0
        }

      } else {
        # Given survey design, calculate proportion and standard error

        params <- survey::svyciprop( # gives same est/se as svymean
          ~outcome, design = dat, df = degf(dat), na.rm = TRUE) %>%
          suppressWarnings()

        phat <- as.numeric(coef(params))
        se <- as.numeric(SE(params))
      }

      # Weighted N
      nwtd <- sum(svytable(~outcome, dat))

      # Weighted number with outcome = 1
      nwtd_est <- data.frame(svytable(~outcome, dat))
      nwtd_est <- nwtd_est[nwtd_est$outcome == 1,]$Freq

      # If empty cell for outcome==1, set nwtd_est to 0
      if(length(nwtd_est) == 0){nwtd_est <- 0}

      # Calculate degrees of freedom ----
      df_n <- nrow(filter(dat$variables, outcome %in% c(0, 1)))

      # Cluster variable
      clustvar <- names(dat_full$cluster) # TO DO is this always populated?

      # From design: are there strata and clusters?
      has_strata <- dat$has.strata
      has_clusters <- ifelse(nrow(unique(dat$cluster)) == nrow(dat), FALSE, TRUE)

      # Stratum variable
      if (dat_full$has.strata == TRUE){
        stratvar <- names(dat_full$strata)
      } else {
        stratvar <- NULL
      }

      # Count the number of strata and clusters involved in the prevalence calculation
      if (dat$has.strata == TRUE){
        df_strata <- dat$strata %>% unique() %>% nrow()
      } else {
        df_strata <- 0
      }

      # The df calculation should include ALL clusters in strata where there are
      # respondents who match the `if' criteria; Use tempvars to be sure to count
      # all those clusters, even though some of the clusters may NOT contain
      # respondents who match the `if' criteria.
      #
      # This point is mentioned specifically in West, B. T., Berglund, P., &
      # Heeringa, S. G. (2008). A closer examination of subpopulation analysis of
      # complex-sample survey data. Stata J, 8(4), 520-31.

      if (has_clusters == TRUE & has_strata == TRUE){

        strata_in_calculation <- unique(dat$strata)[,1]

        dat_temp <- dat_full
        dat_temp$variables$tempstrat <- get(stratvar, dat_full$variables)
        dat_temp$variables$tempclust <- get(clustvar, dat_full$variables)

        dat_temp <- dat_temp$variables %>%
          filter(tempstrat %in% strata_in_calculation)

        df_cluster <- length(unique(dat_temp$tempclust))

      } else if (has_clusters == TRUE & has_strata == FALSE){
        df_cluster <- dat$cluster %>% unique() %>% nrow()
      } else if (has_clusters == FALSE){
        df_cluster <- 0
      }

      # Degrees of freedom ----

      # If strata and clusters are specified, then df = # clusters - # strata
      if (has_strata == TRUE & has_clusters == TRUE){
        df <- df_cluster - df_strata
      }

      # If no clusters, then df = N - # strata
      if (has_strata == TRUE & has_clusters == FALSE){
        df <- nrow(dat) - df_strata
      }

      # If not stratified, then df = # clusters - 1
      if (has_strata == FALSE & has_clusters == TRUE){
        df <- df_cluster - 1
      }

      # If no clusters or strata, then df = N - 1
      if (has_strata == FALSE & has_clusters == FALSE){
        df <- nrow(dat) - 1
      }

      # If the standard error is zero, there is no clustering effect, so
      # we set df = N - 1
      if (se == 0){
        df <- nrow(dat) - 1
      }

      loopcis <- ifelse(!is.null(ci_level_list), ci_level_list, ci_level)
      out <- NULL
      for(i in seq_along(loopcis)){

        cil <- loopcis[i]

        cicalc <- svyp_ci_calc(
          p = phat,
          stderr = se,
          n = df_n,
          dof = df,
          level = cil,
          method = ci_method, # "Wilson",
          adjust = TRUE,
          truncate = TRUE
        )

        ci_level_os <- (100 - 2*(100-cil))
        cic1 <- cicalc[cicalc$level == cil,]
        cic2 <- cicalc[cicalc$level == ci_level_os,]

        temp <- data.frame(
          estimate = phat,
          stderr = se,
          cilevel = cil,
          cill = cic1$lcb_2sided,
          ciul = cic1$ucb_2sided,
          lcb = cic2$lcb_2sided,
          ucb = cic2$ucb_2sided,
          deff = cic1$DEFF,
          neff = cic1$neff,
          icc = NA, # not calculated in this program - add later
          n = df_n,
          nwtd = nwtd,
          nclusters = df_cluster,
          nwtd_est = nwtd_est
        )

        out <- rbind(out, temp)
      }
    } # end nrow(dat) > 0
  } # end variable not all NA

  # Reset survey.lonely.psu option
  options(survey.lonely.psu = save_user_survey_option)

  out

}
