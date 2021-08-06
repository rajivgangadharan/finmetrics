#' get.FilteredTibble(fileName="../Datasets/Opx_2021-06-17_Delivery.csv") %>%   addColCycleTime() %>% computeWeeklyFloorDates() %>% compute.Week()
#' get.FilteredTibble(fileName="../Datasets/Opx_2021-06-17_Delivery.csv") %>%  addColCycleTime() %>% computeWeeklyFloorDates() %>% computePriorityBasedWeeklyClosureAggregates()


#' Generate a tibble from a file filtered on a date
#' Accepts a data file appropriately formatted which contains the Created Date,
#' closed date, updated date of jira issues and a cut off date and returns
#' a tibble.
#' @name get.FilteredTibble
#' @author "Rajiv Gangadharan"
#' @return tib A tibble filtered on the cut off date. Closed Date is >= date_from
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble
#' @examples
#' get.FilteredTibble(fileName="data/example_delivery.csv")
#' get.FilteredTibble(fileName="data/example_delivery.csv", sep="\t")
#' get.FilteredTibble(fileName="data/example_delivery.csv", sep=",", date_from=as.Date("2021-03-03"))
#' @usage
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>% compute.CycleTime()
#' @export
get.FilteredTibble <- function(fileName,
                              sep = '\t',
                              date_from = Sys.Date() - months(6),
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
  tib %>% dplyr::filter(tib[[col_created_on]] >= date_from)
}


#' @name get.ClosedCases
#' @title Takes a tibble and returns a list of closed cases
#' @description Filters out all cases other than closed cases
#' @aliases excludeOpenCases
#' @examples
#' get.FilteredTibble(fileName="example_delivery.csv") %>% get.ClosedCases()
#' @seealso  compute.CycleTime
#' @export
get.ClosedCases <- function(tib_df, col_closed_date = "cldt") {
  dss <- tib_df[,c(col_closed_date)]
  tib_df[complete.cases(dss),]
}

#' @name exclude.OpenCases
#' @title Alias for get.ClosedCases()
#' @description Alias of get.ClosedCases
#' @aliases
#' get.ClosedCases
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
#' @param tdf Tibble containing the created date and closed date
#' @param col_created_on String which has the name of the created_on column
#' defaults to "crdt"
#' @param col_closed_date String which has the name of the closed_date column
#' defaults to "cldt"
#' @examples
#' compute.CycleTime(tdf, col_created_on = "Created", col_closed_date = "Closed")
#' compute.CycleTime(tdf)
#' @usage
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>% compute.CycleTime(tdf)
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
#' @description Adds a column called Week using the the Floor Date
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
                                     week_start = 1))
  tib_df <- computeWeeklyFloorDates()
  tib_df %>% mutate(Week = lubridate::isoweek(tib_df$FloorDate))
}

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
#' @usage
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>%  get.ClosedCases() %>% compute.CycleTime() %>% compute.Week() %>% compute.PriorityBased.ClosureAggregates()
compute.PriorityBased.ClosureAggregates <-
  function(tib_df, col_date = "FloorDate", col_priority ="Priority") {
  tib_df <- tib_df  %>%
    dplyr::group_by(tib_df[[col_date]], tib_df[[col_priority]])  %>%
    dplyr::summarise(NumClosed = dplyr::n(), .groups = "drop")
  colnames(tib_df) <- c("Date", "Priority", "NumClosed")
  tib_df
}


compute.Weekly.ClosureAggregates <-
  function(tib_df, col_week = "Week") {
    stopifnot(tibble::is_tibble(tib_df))
    tib_df <- tib_df  %>%
      dplyr::group_by(tib_df[[col_week]])  %>%
      dplyr::summarise(NumClosed = dplyr::n())
    colnames(tib_df) <- c("Week", "NumClosed")
    tib_df
  }

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


#' @name compute.NumWeeksForClosureCount
#' @title Computes the number of weeks with a particular closure count
#' @description Can be used for finding out how predictable the delivery is
#' @param tib_df The Tibble which has "Number of Weeks" for each "Closure Count
#' @param col_numclosed The column name of the tibble which has the
#' number of closed items
#' @usage
#' get.FilteredTibble(fileName="data/example_delivery.csv") %>%  compute.CycleTime() %>% compute.Week() %>% compute.ClosureAggregates() %>% compute.NumWeeksForClosureCount()

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
#'
#' @return Tibble with Date (Created Date) and Sum of WIP In Days
#'
#' @importFrom lubridate is.Date
#'
#'
#' @export
compute.WIP <- function(tib, col_created_date = "crdt",
                       col_closed_date = "cldt") {
  # Create a date vector from the tibble
  stopifnot(lubridate::is.Date(tib[[col_created_date]]))
  dt_vec <- sort(unique(tib[[col_closed_date]]))
  # Iterate over the date vector and on each day sum up the WIP in Days
  wip_vec <-
    foreach::foreach (dt = dt_vec) %dopar% {
      sum(mapply(
        FUN = get.WIPInDays,
        created = tib[[col_created_date]],
        closed = tib[[col_closed_date]],
        loop_date = dt
      ))
    }
  wip_tibble <- tibble::tibble(dt_vec, wip_vec)
  names(wip_tibble) <- c("Date", "WIPInDays")
  wip_tibble
}
