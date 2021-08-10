#' Closed Cases Data
#'
#' This is output from get.ClosedCases() and gives the complete cases for
#' issues. It drops the rows having NA in the Closed column
#'
#' @format A tibble with the variables
#' \describe{
#' \item{Key}{Issue ID from JIRA}
#' \item{Type}{}
#' \item{Status}{}
#' \item{Priority}{}
#' \item{Created}{Data on which created}
#' \item{Updated}{Updated date}
#' \item{Closed}{Closed Date}
#' \item{crdt}{Formatted Created Date}
#' \item{updt}{Formatted Updated Date}
#' \item{cldt}{Formatted Closed Date}
#' }
#' @source Exported as tsv from jira.
"tib_complete_cases"
