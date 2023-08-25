#' Calculate survey-adjusted confidence intervals of different types for an estimated proportion
#'
#' @param p Estimated proportion
#' @param stderr Standard error
#' @param n Sample size
#' @param dof Degrees of freedom
#' @param level Confidence level
#' @param cilevellist A list of confidence levels (CI calculated for each)
#' @param nweighted Pass-through
#' @param nclusters Pass-through
#' @param method Confidence interval method
#' @param adjust If adjust effective sample size, default to be TRUE
#' @param truncate Truncate DEFF or not, default to be TRUE
#'
#' @return a dataset
#'
#' @import dplyr
#' @rawNamespace import(stats, except = c(filter,lag))

# svyp_ci_calc R version 1.03 - Biostat Global Consulting - 2023-02-22
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-25  1.00      Caitlin Clary   Original R version
# 2022-10-12  1.01      Caitlin Clary   More robust Clopper-Pearson defaulting
# 2022-10-31  1.02      Caitlin Clary   Add error and stop conditions
# 2023-02-22  1.03      Caitlin Clary   Fix Clopper-Pearson CI mismatch (needed
#                                       a rowwise() call)
# *******************************************************************************

svyp_ci_calc <- function(
    p,               # Estimated proportion
    stderr,          # Standard error
    n,               # Sample size
    dof,             # Degrees of freedom
    level = NULL,         # Confidence level
    cilevellist = NULL,   # A list of confidence levels (CI calculated for each)
    nweighted = NULL,            # Pass-through
    nclusters = NULL,            # Pass-through
    method,               # Confidence interval method
    adjust = TRUE,
    truncate = TRUE
){

  # Check CI method specified
  if(!method %in% c(
    "Wald", "Wilson", "Clopper", "Clopper-Pearson", "Exact",
    "Jeffreys", "Agresti", "Agresti-Coull", "Logit", "Fleiss", "Wilsoncc"
  )){

    errormsgs <- "Error in svypd: the ci_method option must be either Wald, Wilson, Clopper, Clopper-Pearson, Exact, Jeffreys, Agresti, Agresti-Coull, Logit, Fleiss, or Wilsoncc"

    # This program is used in the innermost loop of a World Health Organization
    # software suite named the Vaccination Coverage Quality Indicators (VCQI).
    # If svyp_ci_calc was called as part of VCQI, then VCQI_LOGOPEN will be set
    # to 1. In that case, direct an error message to the VCQI log and exit VCQI.
    # Otherwise, exit using a standard stop() call.

    if(vcqi_object_value("VCQI_LOGOPEN", 1)){
      vcqi_global(VCQI_ERROR, 1)
      vcqi_log_comment("svypd", 1, "Error", errormsgs)
      vcqi_halt_immediately(halt_message = errormsgs)
    } else {
      stop(errormsgs)
    }
  }

  # User may not specify both level and cilevellist
  if (!is.null(level) & !is.null(cilevellist)){

    errormsgs <- "For svyp_ci_calc: specify level or cilevellist, but not both"

    # Ditto above: exit using vcqi_halt_immediately if appropriate
    if(vcqi_object_value("VCQI_LOGOPEN", 1)){
      vcqi_global(VCQI_ERROR, 1)
      vcqi_log_comment("svypd", 1, "Error", errormsgs)
      vcqi_halt_immediately(halt_message = errormsgs)
    } else {
      stop(errormsgs)
    }
  }

  if (!is.null(level)){
    if(level > 50){
      cilevellist <- c(level, (100 - 2*(100-level)))
    } else {
      cilevellist <- level
    }
  }

  if(is.null(level) & is.null(cilevellist)){
    cilevellist <- c(95, 90)
  }

  # ncis <- length(cilevellist)

  # Prepare data frame for each element of cilevellist
  ci_df <- data.frame(level = cilevellist)

  ci_df <- ci_df %>%
    mutate(
      phat = p,
      pqhat = phat * (1-phat),
      se = stderr,
      n = n,
      neff = pqhat/(se^2)
    )

  pstring <- sprintf("%.7f", p)
  sestring <- sprintf("%.7f", stderr)

  # If phat is 0 or 1 to seven digits or if stderr is 0 to seven digits
  # then we have homogeneity; set neff to n
  if(pstring == "1.0000000" | pstring == "0.0000000" | sestring == "0.0000000"){
    ci_df$neff <- n
  }

  ci_df <- ci_df %>%
    mutate(
      df_N = round(n, 1),
      df = ifelse(dof != -999, dof, df_N - 1),
      DEFF = (df_N - 1)/neff
    )

  if(pstring == "1.0000000" | pstring == "0.0000000" | sestring == "0.0000000"){
    ci_df$DEFF <- 1
  }

  ci_df <- ci_df %>%
    mutate(
      ao2 = (100 - level)/100/2,
      zao2 = qnorm(1 - ao2),
      acc = (zao2^2)/2, # Agresti-Coull c
      tdfNao2 = ifelse(df_N > 1, qt(1 - ao2, df_N - 1), NA), # Would use for K&G 1998 neff
      tddfao2 = qt(1 - ao2, df)
    )

  # Adjust effective sample size if user has specified the adjust option
  if(adjust == TRUE){
    ci_df <- ci_df %>%
      mutate(neff = neff * (zao2/tddfao2)^2)

    # For K&G 1998 neff, instead would use:
    # mutate(neff = neff * (tdfNao2/tddfao2)^2)
  }

  # Replace effective sample size with actual sample size if DEFF < 1 and
  # user asked to truncate DEFF to be >= 1
  if(truncate == TRUE & ci_df$DEFF[1] < 1){
    ci_df <- ci_df %>%
      mutate(
        neff = n,
        DEFF = 1
      )
  }

  # Wald ----

  if (method == "Wald"){
    # If p is 0 or 1 or stderr = 0, skip Wald calculation and go to
    # Clopper-Pearson

    # if(p == 0 | p == 1 | stderr == 0){
    if (pstring == "1.0000000" | pstring == "0.0000000" | sestring == "0.0000000"){
      method <- "Clopper-Pearson"
    } else {
      ci_df <- ci_df %>%
        mutate(lcb_2sided = phat - abs(zao2 * sqrt(pqhat/neff)),
               ucb_2sided = phat + abs(zao2 * sqrt(pqhat/neff)))
    }
  } # end Wald

  # Logit ----
  if (method == "Logit"){

    # If p is 0 or 1 or stderr = 0, skip Logit calculation and go to
    # Clopper-Pearson

    #if(p == 0 | p == 1 | stderr == 0){
    if (pstring == "1.0000000" | pstring == "0.0000000" | sestring == "0.0000000"){
      method <- "Clopper-Pearson"
    } else {
      ci_df <- ci_df %>%
        mutate(
          term1 = log(phat/(1-phat)),
          term2 = zao2/sqrt(neff*pqhat),
          combo1 = term1 - term2,
          combo2 = term1 + term2,
          lcb_2sided = exp(combo1)/(1 + exp(combo1)),
          ucb_2sided = exp(combo2)/(1 + exp(combo2))
        ) %>%
        select(-c(term1, term2, combo1, combo2))
    }
  } # end Logit

  # Wilson ----
  if (method == "Wilson"){
    ci_df <- ci_df %>%
      mutate(
        term1 = phat + ((zao2)^2)/(2*neff),
        term2 = zao2 * sqrt((pqhat/neff) + ((zao2)^2)/((2*neff)^2)),
        term3 = 1 + ((zao2)^2)/neff,
        lcb_2sided = (term1 - term2)/term3,
        ucb_2sided = (term1 + term2)/term3
      ) %>% select(-c(term1, term2, term3))
  } # end Wilson

  # Jeffreys ----
  if (method == "Jeffreys"){
    ci_df <- ci_df %>%
      mutate(
        x = phat*neff,
        alpha1 = x + 0.5,
        beta1 = neff - x + 0.5,
        lcb_2sided = qbeta(ao2, alpha1, beta1),
        ucb_2sided = qbeta(1 - ao2, alpha1, beta1),
        lcb_2sided = ifelse(phat == 0, 0, lcb_2sided),
        ucb_2sided = ifelse(phat == 1, 1, ucb_2sided)
      ) %>% select(-c(x, alpha1, beta1))
  } # end Jeffreys

  # Agresti-Coull ----
  if (method %in% c("Agresti", "Agresti-Coull")){
    ci_df <- ci_df %>%
      mutate(
        xtilde  = phat * neff + acc,
        ntilde  = neff + 2 * acc,
        ptilde  = xtilde/ntilde,
        pqtilde = ptilde * (1 - ptilde),

        lcb_2sided = ptilde - zao2 * sqrt(pqtilde/ntilde),
        ucb_2sided = ptilde + zao2 * sqrt(pqtilde/ntilde)
      ) %>% select(-c(xtilde, ntilde, ptilde, pqtilde))
  } # end Agresti-Coull

  # Fleiss ----
  if (method %in% c("Fleiss", "Wilsoncc")){
    ci_df <- ci_df %>%
      mutate(
        term1l = 2*neff*phat + zao2^2 - 1,
        term2l = zao2*sqrt(zao2^2 - ( 2 + (1/neff)) + 4*phat*(neff*(1-phat)+1)),
        term3  = 2*(neff+zao2^2),
        lcb_2sided = (term1l - term2l)/term3,
        term1u = term1l + 2,
        term2u = zao2*sqrt(zao2^2 + ( 2 - (1/neff)) + 4*phat*(neff*(1-phat)-1)),
        ucb_2sided = (term1u + term2u)/term3,
        lcb_2sided = ifelse(phat == 0, 0, lcb_2sided),
        ucb_2sided = ifelse(phat == 1, 1, ucb_2sided)
      ) %>% select(-c(term1l, term2l, term1u, term2u, term3))

  } # end Fleiss

  # Clopper-Pearson ----
  if (method %in% c("Clopper-Pearson", "Clopper", "Exact")){

    # If the sample proportion is 0 or 1,
    # or if the standard error is zero (meaning all clusters have the same
    # observed proportion) then consider the effective sample size to be equal
    # to the actual sample size
    if (round(p, 10) == 0 | round(p, 10) == 1 | round(stderr, 10) == 0){
      ci_df <- ci_df %>%
        mutate(neff = n,
               DEFF = 1)
    }

    ci_df <- ci_df %>%
      mutate(
        x = phat*neff,
        v1 = 2*x,
        v2 = 2*(neff-x+1),
        v3 = 2*(x+1),
        v4 = 2*(neff-x)) %>%
      rowwise() %>%
      mutate(
        v1 = max(v1, 2e-10),
        v1 = min(v1, 2e+17),
        v2 = max(v2, 2e-10),
        v2 = min(v2, 2e+17),
        v3 = max(v3, 2e-10),
        v3 = min(v3, 2e+17),
        v4 = max(v4, 2e-10),
        v4 = min(v4, 2e+17)) %>%
      ungroup() %>%
      mutate(
        fao2 = qf(ao2, v1, v2),
        lcb_2sided = (v1*fao2)/(v2 + v1*fao2),
        f1mao2  = qf(1-ao2, v3, v4),
        ucb_2sided = (v3*f1mao2)/(v4 + v3*f1mao2),
        # If v4 is very small, the UCB ratio is set to missing instead of 1
        # so check for this condition here
        ucb_2sided = ifelse(v4 <= 2e-10, 1, ucb_2sided)
      ) %>%
      select(-c(x, v1, v2, v3, v4, fao2, f1mao2))
  } # end Clopper-Pearson

  # Replace infinitesimal values of lcb with 0
  ci_df <- ci_df %>%
    mutate(
      lcb_2sided = ifelse(abs(lcb_2sided) <= 1e-10, 0, lcb_2sided)
    )

  # saveRDS(ci_df, file = paste0(VCQI_OUTPUT_FOLDER, "/testcicalc", RI_DOSE_LIST[d], ".rds"))
  ci_df

}
