is.none <- function(x) {
  ifelse(x == "None", TRUE, FALSE)
}

convertNoneToNA <- function(x) {
  ifelse (!lubridate::is.Date(x) && is.none(x), NA, x)
}

#' Convert string into date
#'
#' @param argString A string to convert into date
#' @param y A date format with default "%Y-%m-%d %H:%M:%S"
#' @return The string converted into a date
#' @examples
#' toDate("2021-03-07", "%Y-%m-%d")

toDate <- function(argString, dateFormat = "%Y-%m-%d") {
  # ifelse(is.na(argString),
  #        NA,
         as.Date(argString,
                 format = dateFormat)
  #)

}

maxDate <- function(dateVec) {
  return (max(dateVec, na.rm=TRUE))
}

minDate <- function(dateVec) {
  return (min(dateVec, na.rm=TRUE))
}

# isWIP <- function(x, loop_date=loopdt) {
#   stopifnot(is.Date(loop_date))
#   # Assert for loop_date being a date.
#   ifelse(x["Created"] < loop_date && is.na(x["Closed"]),
#          TRUE,
#          ifelse(!is.na(x["Closed"])  && x["Closed"] > loop_date,
#                 TRUE,
#                 FALSE))
# }

# getWIPInDays <- function(x, loop_date="dt") {
#   stopifnot(is.Date(loop_date))
#   ifelse(x["Created"] < loop_date && is.na(x["Closed"]),
#          getAgeInDays(x["Created"], loop_date), #getAgeInDays(x["Created"]),
#          ifelse(!is.na(x["Closed"]) && x["Closed"] > loop_date,
#                 getAgeInDays(x["Created"],
#                              loop_date), #getAgeInDays(x["Created"], x["Closed"]),
#                 0))
# }



# getSumOfWIPInDays <- function(x, loop_date) {
#   mapply(
#     FUN = getWIPInDays,
#     created = tib_wip$crdt,
#     closed = tib_wip$cldt,
#     loop_date = as.Date(loop_date)
#   )
# }

#' Tested using
#' tib$WIPDAYS_2021_02_21 <- apply(tib, 1, getWIPInDays, ymd("2021-02-21"))
#' Tested using tib$WIP_2021_02_21 <- apply(tib, 1, isWIP, ymd("2021-02-21"))

getAgeInDays <- function(beginDate, endDate=Sys.Date()) {
  stopifnot(exprs = {lubridate::is.Date(beginDate);
    lubridate::is.Date(endDate)})
  ## ifelse to handle anomalous condition of Closed Date < Created Date
  ifelse(beginDate > endDate, 0,
         as.numeric(difftime(endDate, beginDate, units="days")))
}


readDataset <- function(fileName, sep = '|') {
  stopifnot(file.exists(fileName)) # Will stop of the file doesn't exist
  # Loading the tab separated file
  readr::read_delim(fileName, sep,
             escape_double = FALSE,
             trim_ws = TRUE)
}


readCSVDataset <- function(fileName, sep='|') {
  stopifnot(file.exists(fileName))
  df <- read.csv2(fileName, header=TRUE, sep=sep)
  df
}


