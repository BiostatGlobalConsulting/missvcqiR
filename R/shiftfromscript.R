#' Shift dates from SHIFTFROM to SHIFTTO list
#'
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

# shiftfrom version R version 1.03 - Biostat Global Consulting - 2022-10-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-12  1.00      Mia Yu          Original R version
# 2022-07-21  1.01      Mia Yu          Fixed the problem that it doesn't shift
# 2022-10-05  1.02      Mia Yu          Package version
# 2022-10-19  1.03      Mia Yu          Add variable labels
# *******************************************************************************

#If a later dose is received but previous dose missing in shiftto list
#Keep previous missing doses blank
#Create variable that will show this option


shiftfromscript <- function(s, vlist, dat){
  dat <- mutate(dat, still_fill = 1)

  shiftto <- get("shiftto",envir = .GlobalEnv)
  shiftfrom <- get("shiftfrom",envir = .GlobalEnv)
  SHIFTI <- get("SHIFTI",envir = .GlobalEnv)

  a <- c()
  for (v in seq_along(shiftto)){
    a <- c(shiftto[v],a)
  }

  #Create new variable that shows list of doses that are still missing
  #This list will only include doses after the last received dose
  #Any previous missing doses are not included

  dat <- mutate(dat, empty_list = "")
  for (v in seq_along(a)){
    date <- rlang::sym(paste0(a[v],"_",s,"_date"))
    tick <- rlang::sym(paste0(a[v],"_",s,"_tick"))
    dat  <- mutate(dat,
                   still_fill = ifelse((!is.na(!!date) | !!tick %in% 1),
                                       0, still_fill))
    dat  <- mutate(dat,
                   empty_list = ifelse(still_fill == 1,
                                       paste0(a[v],"_",empty_list),empty_list))
  }

  print(c("Make replacements from ",shiftfrom," to ",shiftto))

  #Create local to show card or register
  c <- "card"
  if (s == "register"){c <- "reg"}

  for (i in 1:length(shiftto)){
    dosei = shiftto[i]
    dat <- mutate(dat,
                  !!paste0("include_",dosei) := ifelse(str_detect(empty_list,dosei) %in% TRUE, 1, 0))

    for (j in 1:length(shiftfrom)){
      dosej = shiftfrom[j]
      datei <- rlang::sym(paste0(dosei,"_",s,"_date"))
      ticki <- rlang::sym(paste0(dosei,"_",s,"_tick"))
      datej <- rlang::sym(paste0(dosej,"_",s,"_date"))
      tickj <- rlang::sym(paste0(dosej,"_",s,"_tick"))
      include <- rlang::sym(paste0("include_",dosei))
      dat <- mutate(dat, tempvar1 =
                      ifelse(((is.na(!!datei) & !(!!ticki %in% 1)) & (!is.na(!!datej) | !!tickj %in% 1) & !!include %in% 1) %in% TRUE,
                             1, 0))
      dat <- mutate(dat, !!datei := if_else(tempvar1 == 1, !!datej, !!datei))
      dat <- mutate(dat, !!ticki := ifelse(tempvar1 == 1, !!tickj, !!ticki))
      dat <- mutate(dat, !!datej := if_else(tempvar1 == 1, NA_Date_, !!datej))
      dat <- mutate(dat, !!tickj := ifelse(tempvar1 == 1, NA, !!tickj))

      if(all(dat$tempvar1 == 0,na.rm = TRUE)){
        dat <- select(dat, -c(tempvar1))
      } else{
        times <- " times "
        if (sum(dat$tempvar1,na.rm = TRUE) == 1){times <- " time "}
        rsum <- sum(dat$tempvar1)
        print(paste0("The ",s," date for dose ",dosej," was shifted to dose ",dosei," ",rsum,times, " due to SHIFTWITHIN option."))
        vcqi_log_comment(VCP, 4, "Data", paste0("The ",s," date for dose ",dosej," was shifted to dose ",dosei," ",rsum,times, " due to SHIFTWITHIN option."))

        dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                        label = paste0(str_to_title(s), " - ", dosej, " date and tick moved to ",
                                                       dosei, " from SHIFTFROM list")) %>% suppressWarnings()

        names(dat)[which(names(dat) == "tempvar1")] <- paste0("shift",dosej,"2",dosei,"_",c,"_",SHIFTI)
        vlist <- c(vlist,paste0("shift",dosej,"2",dosei,"_",c,"_",SHIFTI))
      }
    }
  }

  #return both vlist and dat
  result <- list(varlist = vlist,dataset = dat)
  return(result)
}


