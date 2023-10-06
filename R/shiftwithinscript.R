#' Shift dates within a dose list
#'
#' @param input A list of doses to shift evidence within
#' @param shifttype Name of the dose list (string)
#' @param s Source of the vaccination dates (string)
#' @param vlist Names of variables to keep (vector)
#' @param dat Dataset with vaccination date variables
#'
#' @return A list of variable names and a dataset
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import haven

# shiftwithin version R version 1.02 - Biostat Global Consulting - 2022-10-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-11  1.00      Mia Yu          Original R version
# 2022-10-05  1.01      Mia Yu          Package version
# 2022-10-19  1.02      Mia Yu          Add variable labels
# *******************************************************************************

shiftwithinscript <- function(input,shifttype,s,vlist,dat){
  if(length(input) > 1){
    print(c(paste0("Make any replacements within ", str_to_upper(shifttype)," : "), input))
    SHIFTI <- get("SHIFTI",envir = .GlobalEnv)
    c <- "card"
    if(s == "register"){
      c <- "reg"
    }

    for (i in 1:(length(input)-1)){
      for (j in (i+1):length(input)){
        dosei = input[i]
        dosej = input[j]

        doseidate <- rlang::sym(paste0(dosei,"_",s,"_date"))
        doseitick <- rlang::sym(paste0(dosei,"_",s,"_tick"))
        dosejdate <- rlang::sym(paste0(dosej,"_",s,"_date"))
        dosejtick <- rlang::sym(paste0(dosej,"_",s,"_tick"))
        dat <- mutate(dat,tempvar1 =
                        ifelse(((is.na(!!doseidate) & !(!!doseitick %in% 1)) &
                                  (!is.na(!!dosejdate) | !!dosejtick %in% 1)) %in% TRUE,1,0))
        dat <- dat %>% mutate(!!doseidate := if_else(tempvar1 == 1, !!dosejdate, !!doseidate))
        dat <- dat %>% mutate(!!doseitick := ifelse(tempvar1 == 1, !!dosejtick, !!doseitick))
        dat <- dat %>% mutate(!!dosejdate := if_else(tempvar1 == 1, NA_Date_, !!dosejdate))
        dat <- dat %>% mutate(!!dosejtick := ifelse(tempvar1 == 1, NA, !!dosejtick))

        if(all(dat$tempvar1 == 0,na.rm = TRUE)){
          dat <- select(dat, -c(tempvar1))
        } else{
          times <- " times "
          if (sum(dat$tempvar1,na.rm = TRUE) == 1){times <- " time "}
          rsum <- sum(dat$tempvar1)
          print(paste0("The ",s," date for dose ",dosej," was shifted to dose ",dosei," ",rsum,times, " due to SHIFTWITHIN option."))
          vcqi_log_comment(VCP, 4, "Data", paste0("The ",s," date for dose ",dosej," was shifted to dose ",dosei," ",rsum,times, " due to SHIFTWITHIN option."))
          rm(rsum) %>% suppressWarnings()

          dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                          label = paste0(dosej, " date and tick moved to ", dosei,
                                                         " from doses provided in ", str_to_upper(shifttype), " list")) %>% suppressWarnings()

          names(dat)[which(names(dat) == "tempvar1")] <- paste0("shift_wi_",dosej,"2",dosei,"_",c,"_",SHIFTI)
          vlist <- c(vlist,paste0("shift_wi_",dosej,"2",dosei,"_",c,"_",SHIFTI))
        }

      }
    }
    #return both vlist and dat
    result <- list(varlist = vlist,dataset = dat)
    return(result)
  } else{
    result <- list(varlist = vlist,dataset = dat)
    return(result)
  }
}
