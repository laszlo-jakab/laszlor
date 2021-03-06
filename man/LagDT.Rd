% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LagDT.R
\name{LagDT}
\alias{LagDT}
\title{Generate leads/lags of variable(s) in data.table object}
\usage{
LagDT(dt, x, shift.by = 1, date.id = "date", panel.id = NULL)
}
\arguments{
\item{dt}{A data.table object.}

\item{x}{Variable for which leads/lags}

\item{shift.by}{Number of time periods by which to lead/lag x. Leads should be supplied as negative integers, lags as positive integers. A vector of values can be supplied. Problems may arise if incrementing the date variable by '1' does not correspond to a one period difference. The only exception from this rule is 'yearmon' class dates, which LagDT detects and handles such that shift.by = k results in a k-month lag, even though a single month corresponds to a 1/12 increment in this class. Note that the function throws a warning if the class of the date object supplied is not 'Date' (where shift.by = 1 corresponds to a one day lag) or 'yearmon' (where shift.by = 1 corresponds to a one month lag).}

\item{date.id}{Name of the date variable, supplied as a string in quotes. Defaults to 'date'.}

\item{panel.id}{Name of the panel variable(s), if any.}
}
\value{
The supplied data.table, with the requested lags/leads as additional columns.
}
\description{
Works by making a copy of the data.table, shifting the date, and merging back.
Can handle panel data, shifting multiple variables at once, as well as a list
of shift dates (multiple shift lengths are handled through a simple loop).
Note that
}
\examples{
DT <- data.table(
  caldt = seq.Date(from = as.Date("2018-01-01"), length.out = 10, by = "day"),
  x = 1:10)
LagDT(DT, "x", date.id = "caldt")
LagDT(DT, "x", shift.by = c(-1, 1, 3), date.id = "caldt")
DT[, y := 101:110]
LagDT(DT, c("x", "y"), shift.by = 1:3, date.id = "caldt")
DT.panel <- data.table(
  fund = rep(1:2, each = 5),
  caldt = rep(seq.Date(from = as.Date("2018-01-01"),
                       length.out = 5, by = "day"), 2),
  x = c(paste0("fund1x", 1:5), paste0("fund2x", 1:5)),
  y = c(paste0("fund1y", 101:105), paste0("fund2y", 101:105)))
LagDT(DT.panel, c("x", "y"),
      shift.by = 1:3, date.id = "caldt", panel.id = "fund")
}
