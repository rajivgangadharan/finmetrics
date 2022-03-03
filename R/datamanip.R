#' get.FilteredTibble(fileName="../Datasets/Opx_2021-06-17_Delivery.csv") %>%   addColCycleTime() %>% computeWeeklyFloorDates() %>% compute.Week()
#' get.FilteredTibble(fileName="../Datasets/Opx_2021-06-17_Delivery.csv") %>%  addColCycleTime() %>% computeWeeklyFloorDates() %>% computePriorityBasedWeeklyClosureAggregates()


#' Generate a tibble from a file filtered on a date
#' Accepts a data file appropriately formatted which contains the Created Date,
#' closed date, updated date of jira issues and a cut off date and returns
#' a tibble.
#' #'
#' get.FilteredTibble(fileName="data/example_delivery.csv")
#' get.FilteredTibble(fileName="data/example_delivery.csv", sep='\\t')
#' get.FilteredTibble(fileName="data/example_delivery.csv", sep=",", date_from=as.Date("2021-03-03"))
#' @name get.FilteredTibble
#' @author "Rajiv Gangadharan"
#' @return tib A tibble filtered on the cut off date. Closed Date is >= date_from
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble
#' @param fileName Input file name
#' @param sep seperator
#' @param date_from cut off date for the data frame
#' @param col_created_on Column name of the created column
#' @param col_updated_date Column name of the updated column
#' @param col_closed_date Column name of the closed date column
#' @export
get.FilteredTibble <- function(fileName,
                              sep = '\t',
                              date_from = Sys.Date() - months(18),
                              col_created_on = "Created",
                              col_updated_date = "Updated",
                              col_closed_date = "Closed") {
  tib <- readDataset(fileName, sep) %>% tibble::as_tibble()
  tib[, col_closed_date] <- apply(tib[, col_closed_date],
                                  1,
                                  convertNoneToNA)
  tib <- tib %>%
    mutate(
      crdt = toDate(tib[[col_created_on]],
                    dateFormat = "%Y-%m-%d"),
      updt = toDate(tib[[col_updated_date]],
                    dateFormat = "%Y-%m-%d"),
      cldt = toDate(tib[[col_closed_date]],
                    dateFormat = "%Y-%m-%d")
    )
  tib %>%
    dplyr::filter(tib[[col_created_on]] >= date_from) %>%
    dplyr::filter(is.na(cldt) | cldt >= date_from)
      # Added to filter out anomalies when Created > Closed Dates
}

#'
#' get.FilteredTibble(fileName="example_delivery.csv") %>% get.ClosedCases()
#' @name get.ClosedCases
#' @param tib_df Input data frame which has unclosed cases as well
#' @param col_closed_date Column which has the closed date
#' @title Takes a tibble and returns a list of closed cases
#' @description Filters out all cases other than closed cases
#' @seealso  compute.CycleTime
#' @export
get.ClosedCases <- function(tib_df, col_closed_date = "cldt") {
  dss <- tib_df[,c(col_closed_date)]
  tib_df[complete.cases(dss),]
}

#' @name exclude.OpenCases
#' @title Alias for get.ClosedCases()
#' @param tib_df Input data frame which has unclosed cases as well
#' @param col_closed_date Column which has the closed date
#' @title Takes a tibble and returns a list of closed cases
#' @description
#' Filters out all cases other than closed cases
#' and Alias of get.ClosedCases
#' @export
exclude.OpenCases <- get.ClosedCases


#' Adds a column to the input tibble with the calculated Cycle Time
#' Accepts a tibble which has the created date and the closed date as the
#' columns and calculates the cycle time from it and adds it as another
#' column.
#' @name compute.CycleTime
#' @title Adds a column to the tibble with the cycle time and returns a tibble
#' @return A tibble with a new column called "cylt" having the cycle time
#' @seealso getClosedCases()
#' @export compute.Cycle
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble
#' @param tib_df Tibble containing the created date and closed date
#' @param col_created_on String which has the name of the created_on column defaults to "crdt"
#' @param col_closed_date String which has the name of the closed_date column defaults to "cldt"
#' @export compute.CycleTime
compute.CycleTime <- function(tib_df,
                             col_created_on = "crdt",
                             col_closed_date = "cldt") {
  tib_df %>% mutate(cylt =
                      as.numeric(getAgeInDays(tib_df[[col_created_on]],
                                              tib_df[[col_closed_date]])))
}

#' @name compute.Week
#' @title Adds a week number to the tibble and returns
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom lubridate isoweek
#' @importFrom lubridate floor_date
#' @description
#' Adds a column called Week using the the Floor Date
#' derived from Closed date
#' @param tib_df Input tibble with the Closed Date
#' @param col_closed_date The Column name which has the closed date
#' @export
compute.Week <- function(tib_df, col_closed_date = "cldt") {
  stopifnot(exprs = {tibble::is_tibble(tib_df);
    is.character(col_closed_date)})

  computeWeeklyFloorDates <-  function()
      tib_df %>%
      dplyr::mutate(FloorDate =
               lubridate::floor_date(tib_df[[col_closed_date]],
                                     unit = "weeks",
                                     week_start = 7))
  tib_df <- computeWeeklyFloorDates()
  tib_df %>% mutate(Week = lubridate::isoweek(tib_df$FloorDate))
}

#' get.FilteredTibble(fileName="data/example_delivery.csv") %>%  get.ClosedCases() %>% compute.CycleTime() %>% compute.Week() %>% compute.PriorityBased.ClosureAggregates()
#'
#' @name compute.PriorityBased.ClosureAggregates
#' @title Generates a tibble with the Date, Priority and Number of Work Items
#' Closed
#' @description Receives a tibble with complete cases (closed cases) and then
#' generates a tibble with the Date, Priority and Number of Work Items
#' Closed
#' @param tib_df Tibble containing complete cases
#' @param col_date Column Name of with the Floor Date derived from
#' the Closed Date
#' @param col_priority Column name of the priority of the closed items
#' @seealso compute.Week()
#' @export
compute.PriorityBased.ClosureAggregates <-
  function(tib_df) {
    if (! hasName(tib_df, "FloorDate") && ! hasName(tib_df, "Priority"))
      stop("Expected names not found.")
  tib_df <- tib_df  %>%
    dplyr::group_by(FloorDate, Priority)  %>%
    dplyr::summarise(NumClosed = dplyr::n(), .groups = "drop")
  colnames(tib_df) <- c("FloorDate", "Priority", "NumClosed")
  tib_df
}

#' Computes the Closure aggregates for every week. Grouping is
#' done for every week followed by summarisation of the Number closed
#' @name compute.Weekly.ClosureAggregates
#' @title Computes closure aggregates for every week
#' @return tib_df the aggregated tibble
#' @param tib_df Unaggregated tibble  with the week number as a variable
#' @param col_week A column which has the week number as the variable
compute.Weekly.ClosureAggregates <-
  function(tib_df, col_week = "Week") {
    stopifnot(tibble::is_tibble(tib_df))
    if (! hasName(tib_df, "Week"))
      stop("Expected name Week not found.")
    tib_df <- tib_df  %>%
      dplyr::group_by(Week)  %>%
      dplyr::summarise(NumClosed = dplyr::n())
    colnames(tib_df) <- c("Week", "NumClosed")
    tib_df
  }


#' Computes the Closure aggregates for every week. Grouping is
#' done for every Floor Date followed by summarisation of the
#' Number closed
#' @name compute.FloorDateBased.ClosureAggregates
#' @title Computes closure aggregates for every FloorDate
#' @return tib_df the aggregated tibble
#' @param tib_df Unaggregated tibble  with the week number as a variable
#' @param col_dt A column which has the FloorDate as the variable

compute.FloorDateBased.ClosureAggregates <-
  function(tib_df, col_dt = "FloorDate") {
    stopifnot(tibble::is_tibble(tib_df))
    tib_df <- tib_df  %>%
      dplyr::group_by(tib_df[[col_dt]])  %>%
      dplyr::summarise(NumClosed = dplyr::n())
    colnames(tib_df) <- c("FloorDate", "NumClosed")
    tib_df
  }


#' @name compute.ClosureAggregates
#' @title Closure Aggregates based on Floor Date
#' @description takes a tibble with a Week Column and a Floor date Column and then returns an aggregated tibble
#' @returns tibble with Week, FloorDate and NumClosed Columns
#' @param tib_df Tibble with Week and Floor Date Information
#' @param col_dt Date to be aggregated on
#' @param col_week Week to be aggregated on
compute.ClosureAggregates <-
  function(tib_df, col_dt = "FloorDate", col_week = "Week") {
    stopifnot(tibble::is_tibble(tib_df))
    tib_df <- tib_df  %>%
      dplyr::group_by(tib_df[[col_week]], tib_df[[col_dt]])  %>%
      dplyr::summarise(NumClosed = dplyr::n(), .groups = "drop")
    colnames(tib_df) <- c("Week", "FloorDate","NumClosed")
    tib_df
  }


#'
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>%  compute.CycleTime() %>% compute.Week() %>% compute.ClosureAggregates() %>% compute.NumWeeksForClosureCount()
#' @name compute.NumWeeksForClosureCount
#' @title Computes the number of weeks with a particular closure count
#' @description Can be used for finding out how predictable the delivery is
#' @param tib_df The Tibble which has "Number of Weeks" for each "Closure Count
#' @param col_numclosed The column name of the tibble which has the
#' number of closed items
#' @export
compute.NumWeeksForClosureCount <- function(tib_df,
                                           col_numclosed = "NumClosed") {
  stopifnot(tibble::is_tibble(tib_df))
  tib_df <-
    tib_df %>%
    dplyr::group_by(tib_df[[col_numclosed]]) %>%
    dplyr::summarise(NumWeeks = dplyr::n())
  colnames(tib_df) <- c("NumClosed", "NumWeeks")
  tib_df
}

#' @name get.WIPInDays
#' @title Returns the WIP at a particular date for a created and closed date
#' @description Accepts the created and closed dates and returns the age in
#' days for a particular date which is provided.
#' @param created The Created Date
#' @param closed The Closed Date
#' @param loop_date The date on which the age in days have to be computed
#' @return Returns a numeric age in days
get.WIPInDays <- function(created, closed, loop_date = Sys.Date()) {
  stopifnot(lubridate::is.Date(loop_date))
  # Will Abort if the input date is not a Date.
  isWIP <- function() {
    ifelse(
      created < loop_date && is.na(closed),
      TRUE,
      ifelse(!is.na(closed)  && closed > loop_date,
             TRUE,
             FALSE)
    )
  }
  ifelse(isWIP(), getAgeInDays(created, loop_date), 0)
}

#' @name compute.WIP
#' @title Takes a tibble and returns a tibble with wip for each date
#' @description Takes a tibble and returns a tibble with wip for each date
#' @param tib Tibble containing Open and Closed Dates.
#' @param col_created_date Column name with created date (default: crdt)
#' @param col_closed_date Column name with closed date (default: cldt)
#' @return Tibble with Date (Created Date) and Sum of WIP In Days
#' @importFrom lubridate is.Date
#' @export
compute.WIP <- function(tib, col_created_date = "crdt",
                       col_closed_date = "cldt") {
  # Create a date vector from the tibble
  stopifnot(lubridate::is.Date(tib[[col_created_date]]))
  #dt_vec <- sort(unique(tib[[col_closed_date]]))
  dt_vec <- sort(unique(tib[[col_created_date]]))
  wip_vec <- lapply(dt_vec, FUN=sum.WIPInDays, tib=tib)
  wip_tibble <- tibble::tibble(dt_vec, wip_vec)
  names(wip_tibble) <- c("Date", "WIPInDays")
  wip_tibble
}

sum.WIPInDays <-
  function(dt,
           tib,
           col_created_date = "crdt",
           col_closed_date = "cldt"
           ) {
    stopifnot (lubridate::is.Date(tib[[col_created_date]]) &&
                 lubridate::is.Date(tib[[col_closed_date]]))
    stopifnot(tibble::is_tibble(tib))
    sum(mapply(
      FUN = get.WIPInDays,
      created = tib[[col_created_date]],
      closed = tib[[col_closed_date]],
      loop_date = dt
    ))
  }

#' @name constuct.WIPTibble
#' @description Constructs a tibble based on various parameters
#' @return A tibble
#' @title Constructs a tibble based on a data file, date from and type to be used for computing the WIP
#' @importFrom dplyr filter %>%
#' @param tib tibble to compute the WiP for every day
#' @export construct.WIPTibble
construct.WIPTibble <-  function(tib) {
      tib %>% finmetrics::compute.WIP()
}
