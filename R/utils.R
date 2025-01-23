# utils.R
# File contains utility functions for the mtrxrlib package.
# Rajiv Gangadharan <rajiv.gangadharan@gmail.com>
# Copyright 2017 Rajiv Gangadharan <rajiv.gangadharan@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#'
#' Check if the parameter is None or not
#' @name is.none
#' @description Purpose is to handle None values inserted into the data files due to python scripts
#' @param x variable for which None value has to be handled
#' @return TRUE or FALSE based of if the Value is None
is.none <- function(x) {
  ifelse(x == "None", TRUE, FALSE)
}

convertNoneToNA <- function(x) {
  ifelse (!lubridate::is.Date(x) && is.none(x), NA, x)
}

#' Convert string into date
#' @name toDate
#' @title Takes a string and then converts into date
#' @description
#' Takes a string and then converts into date, utility function to be used in datamanip.R by many functions.
#' @details
#' kes a string and then converts into date, utility function to be used in datamanip.R by many functions.
#' @param argString A string to convert into date
#' @param dateFormat A valid date format
#' @return The string converted into a date
toDate <- function(argString, dateFormat = "%Y-%m-%d") {
  as.Date(argString,
          format = dateFormat)
}

maxDate <- function(dateVec) {
  return (max(dateVec, na.rm = TRUE))
}

minDate <- function(dateVec) {
  return (min(dateVec, na.rm = TRUE))
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

#' @name getAgeInDays
#' @param beginDate the begining date
#' @param endDate defaults to system date
#' @title gets the age in days from the begining and closing date
#' @description Gets the age of the work item in days
#' @seealso  compute.CycleTime
#' @export
getAgeInDays <- function(beginDate, endDate = Sys.Date()) {
  stopifnot(exprs = {
    lubridate::is.Date(beginDate)

    lubridate::is.Date(endDate)
  })
  ## ifelse to handle anomalous condition of Closed Date < Created Date
  ifelse(beginDate > endDate, 0,
         as.numeric(difftime(endDate, beginDate, units = "days")))
}

#' @name get.Age.In.Days
#' @param beginDate the begining date
#' @param endDate defaults to system date
#' @title gets the age in days from the begining and closing date
#' @description Gets the age of the work item in days
#' @seealso  compute.CycleTime
#' @export
get.Age.In.Days <- getAgeInDays

readDataset <- function(fileName, sep = '|') {
  if (!file.exists(fileName)) {
    stop(sprintf("The file '%s' does not exist. Please check the file path.", fileName))
  }
  # Will stop of the file doesn't exist
  # Loading the tab separated file
  readr::read_delim(fileName, sep,
                    escape_double = FALSE,
                    trim_ws = TRUE)
}


readCSVDataset <- function(fileName, sep = '|') {
  stopifnot(file.exists(fileName))
  df <- read.csv2(fileName, header = TRUE, sep = sep)
  df
}

handle_invalid_date <- function(date_string, format = "%Y-%m-%d", default = NA) {
  # Try to parse the date
  parsed_date <- as.Date(date_string, format = format)
  
  # Check if the date is valid
  if (is.na(parsed_date)) {
    warning(sprintf("Invalid date: '%s'. Returning default value.", date_string))
    return(default) # Return the default value if the date is invalid
  }
  
  return(parsed_date) # Return the valid date
}
