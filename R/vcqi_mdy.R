#' Parse date from individual month, day, and year components like lubridate::mdy, returning NA if any element is missing
#'
#' @param m A character or numeric vector indicating month, NAs allowed
#' @param d A character or numeric vector indicating day, NAs allowed
#' @param y A character or numeric vector indicating year, NAs allowed
#'
#' @return A vector of dates
#'
#' @import lubridate
#'
#' @examples
#' vcqi_mdy(1,2,2022)
#' vcqi_mdy(10,NA,2022)
#'
#' month <- c(1,2,4,5)
#' date <- c(12,14,16,NA)
#' year <- rep(2022,4)
#' vcqi_mdy(month,date,year)

# VCQI_mdy R version 1.03 - Biostat Global Consulting - 2023-08-31
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-28  1.00      Mia Yu          Original version
# 2022-10-04  1.01      Mia Yu          Package version
# 2022-10-28  1.02      Mia Yu          Add part to convert to numeric
# 2023-08-31  1.03      Caitlin Clary   Pad year to two or four digits depending
#                                       on input year length
# *******************************************************************************

vcqi_mdy <- function(m,d,y){
  tempm <- as.numeric(m)
  tempd <- as.numeric(d)
  tempy <- as.numeric(y)
  tempm <- ifelse(is.na(tempm), 99, tempm)
  tempd <- ifelse(is.na(tempd), 99, tempd)

  pad_m <- sprintf("%02d", tempm)
  pad_d <- sprintf("%02d", tempd)

  pad_y2 <- sprintf("%02d", tempy) # Year padded to two digits
  pad_y4 <- sprintf("%04d", tempy) # Year padded to four digits

  pad_y <- sapply(
    seq_along(tempy),
    function(x) ifelse(str_length(tempy[x]) <= 2, pad_y2[x], pad_y4[x]),
    simplify = TRUE)

  suppressWarnings(mdy(paste0(pad_m, pad_d, pad_y)))
}
