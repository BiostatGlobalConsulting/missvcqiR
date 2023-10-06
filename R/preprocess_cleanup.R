#' Clean up dates for doses in SHIFTTO list (and SHIFTFROM list) as required
#'
#' @param s Source of the vaccination dates (string)
#' @param dat Dataset with vaccination date variables
#'
#' @return A list of variable names and a dataset
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import haven
#' @import stringr

# preprocess_cleanup version R version 1.06 - Biostat Global Consulting - 2022-10-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-11  1.00      Mia Yu          Original R version
# 2022-07-21  1.01      Mia Yu          Bugs fixed
# 2022-07-22  1.02      Mia Yu          Bugs fixed
# 2022-09-23  1.03      Mia Yu          Updated to allow shiftfrom being optional
# 2022-10-05  1.04      Mia Yu          Package version
# 2022-10-11  1.05      Mia Yu          Update the usage of vcqi_object_exists
# 2022-10-19  1.06      Mia Yu          Add variable labels
# *******************************************************************************

preprocess_cleanup <- function(s,dat){

  dropdup <- get("dropdup",envir = .GlobalEnv)
  shiftto <- get("shiftto",envir = .GlobalEnv)
  shiftfrom <- get("shiftfrom",envir = .GlobalEnv)
  SHIFTI <- get("SHIFTI",envir = .GlobalEnv)

  # Clean up ticks
  print("Remove any ticks if a date is present...")
  if (!is.null(shiftfrom)){
    doses <- c(shiftto,shiftfrom)
  } else{
    doses <- c(shiftto)
  }


  for (v in seq_along(doses)){
    change <- paste0("mutate(dat,",paste0(doses[v],"_",s,"_tick"), "= ifelse(!is.na(",paste0(doses[v],"_",s,"_date"),"), NA, ", paste0(doses[v],"_",s,"_tick"),"))")
    dat <- eval(rlang::parse_expr(change))
  }

  #Wipe out dates if they are the same
  #Do not set the tick mark
  #Only do this if user specified dropdup == 1
  #This will be done within the shiftto, shiftfrom and between shiftto and shiftfrom
  #The earliest dose will keep the date

  if (dropdup == 1){
    wipeout <- c()
    print("Remove any duplicate dates...")
    for (b in seq_along(shiftto)){
      for (v in seq_along(shiftto)){
        if (shiftto[b] != shiftto[v]){
          wipevar <- paste0(shiftto[v],"_",shiftto[b])
          if(!(wipevar %in% wipeout)){
            wipeout <- c(wipeout, paste0(shiftto[b],"_",shiftto[v]))
            bdatevar <- rlang::sym(paste0(shiftto[b],"_",s,"_date"))
            vdatevar <- rlang::sym(paste0(shiftto[v],"_",s,"_date"))
            dat <- mutate(dat, tempvar1 =
                            ifelse((is.na(!!bdatevar) & is.na(!!vdatevar)) %in% TRUE , 0,
                                   ifelse((!!bdatevar == !!vdatevar) %in% TRUE, 1,0)))

            dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                            label = paste0(str_to_title(s), " - ", shiftto[b], " has the same date as ",
                                                           shiftto[v], ", ", shiftto[v],"  is wiped out")) %>% suppressWarnings()

            names(dat)[which(names(dat) == "tempvar1")] <- paste0(shiftto[b],"_date_eq_",shiftto[v],"_",s,"_",SHIFTI)
          }
        }
      }
    }

    #Check for same dates within shiftfrom
    if (!is.null(shiftfrom)){
      for (b in seq_along(shiftfrom)){
        for (v in seq_along(shiftfrom)){
          if (shiftfrom[b] != shiftfrom[v]){
            wipevar <- paste0(shiftfrom[v],"_",shiftfrom[b])
            if(!(wipevar %in% wipeout)){
              wipeout <- c(wipeout, paste0(shiftfrom[b],"_",shiftfrom[v]))
              bdatevar <- rlang::sym(paste0(shiftfrom[b],"_",s,"_date"))
              vdatevar <- rlang::sym(paste0(shiftfrom[v],"_",s,"_date"))
              dat <- mutate(dat, tempvar1 =
                              ifelse((is.na(!!bdatevar) & is.na(!!vdatevar)) %in% TRUE , 0,
                                     ifelse((!!bdatevar == !!vdatevar) %in% TRUE, 1,0)))

              dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                              label = paste0(str_to_title(s), " - ", shiftfrom[b], " has the same date as ",
                                                             shiftfrom[v], ", ", shiftfrom[v],"  is wiped out")) %>% suppressWarnings()

              names(dat)[which(names(dat) == "tempvar1")] <- paste0(shiftfrom[b],"_date_eq_",shiftfrom[v],"_",s,"_",SHIFTI)
            }
          }
        } #end of v loop
      } #end of b loop
    }

    #Now check for same dates between shiftto and shiftfrom
    if (!is.null(shiftfrom)){
      for (b in seq_along(shiftto)){
        for (v in seq_along(shiftfrom)){
          wipevar <- paste0(shiftfrom[v],"_",shiftto[b])
          if(!(wipevar %in% wipeout)){
            wipeout <- c(wipeout, paste0(shiftto[b],"_",shiftfrom[v]))
            bdatevar <- rlang::sym(paste0(shiftto[b],"_",s,"_date"))
            vdatevar <- rlang::sym(paste0(shiftfrom[v],"_",s,"_date"))
            dat <- mutate(dat, tempvar1 =
                            ifelse((is.na(!!bdatevar) & is.na(!!vdatevar)) %in% TRUE , 0,
                                   ifelse((!!bdatevar == !!vdatevar) %in% TRUE, 1,0)))

            dat$tempvar1 <- haven::labelled(dat$tempvar1,
                                            label = paste0(str_to_title(s), " - ", shiftto[b], " has the same date as ",
                                                           shiftfrom[v], ", ", shiftfrom[v],"  is wiped out")) %>% suppressWarnings()

            names(dat)[which(names(dat) == "tempvar1")] <- paste0(shiftto[b],"_date_eq_",shiftfrom[v],"_",s,"_",SHIFTI)
          }
        } #end of v loop
      } #end of b loop
    }

    for (w in seq_along(wipeout)){
      word <- unlist(strsplit(wipeout[w],"_"))
      b <- word[1]
      v <- word[2]
      eqvar <- rlang::sym(paste0(b,"_date_eq_",v,"_",s,"_",SHIFTI))
      vartocheck <- get(paste0(b,"_date_eq_",v,"_",s,"_",SHIFTI),dat)
      change <- paste0("mutate(dat, ",paste0(v,"_",s,"_date")," = if_else(",paste0(b,"_date_eq_",v,"_",s,"_",SHIFTI)," == 1, NA_Date_, ",paste0(v,"_",s,"_date"),"))")
      dat <- eval(rlang::parse_expr(change))
      #Only keep the variable if applies to doses in dataset
      if (all(vartocheck == 0,na.rm = TRUE)){
        dat <- select(dat, -c(!!eqvar))
      } else {
        times <- " times."
        if (sum(vartocheck,na.rm = TRUE) == 1){times <- " time."}
        r <- sum(vartocheck)
        print(paste0("The ",s," date for dose ",b," and ",v," are the same ",r,times, " Only dose ",b," is kept due to DROPDUP option."))
        vcqi_log_comment(VCP, 4, "Data", paste0("The ",s," date for dose ",b," and ",v," are the same ",r,times, " Only dose ",b," is kept due to DROPDUP option."))
      }
    }
  }
  vlist <- names(dat)

  #return both vlist and dat
  result <- list(varlist = vlist,dataset = dat)
  return(result)

}
