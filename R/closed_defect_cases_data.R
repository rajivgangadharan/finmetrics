#' Closed Delivery Cases Data
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
#' \item{Origin}{From where?, e.g. External Customer, Development}
#' \item{Source}{The Source of this issue e.g QA Campaign, Development}
#' \item{Category}{The Product Category, e.g: Opics Derivatives}
#' \item{Severity}{}
#' \item{Verified}{Date Time on which verified}
#' \item{Resolution}{Final Resolution, e.g: Done}
#' \item{crdt}{Formatted Created Date}
#' \item{updt}{Formatted Updated Date}
#' \item{cldt}{Formatted Closed Date}
#' }
#' @source Exported as tsv from jira.
"closed_defect_cases"
