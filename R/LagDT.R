#' Generate leads/lags of variable(s) in data.table object
#'
#' Works by making a copy of the data.table, shifting the date, and merging back.
#' Can handle panel data, shifting multiple variables at once, as well as a list
#' of shift dates (multiple shift lengths are handled through a simple loop).
#' Note that
#'
#' @param dt A data.table object.
#' @param x Variable for which leads/lags
#' @param shift.by Number of time periods by which to lead/lag x. Leads should be supplied as negative integers, lags as positive integers. A vector of values can be supplied. Problems may arise if incrementing the date variable by '1' does not correspond to a one period difference. The only exception from this rule is 'yearmon' class dates, which LagDT detects and handles such that shift.by = k results in a k-month lag, even though a single month corresponds to a 1/12 increment in this class. Note that the function throws a warning if the class of the date object supplied is not 'Date' (where shift.by = 1 corresponds to a one day lag) or 'yearmon' (where shift.by = 1 corresponds to a one month lag).
#' @param date.id Name of the date variable, supplied as a string in quotes. Defaults to 'date'.
#' @param panel.id Name of the panel variable(s), if any.
#'
#' @return The supplied data.table, with the requested lags/leads as additional columns.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' DT <- data.table(
#'   caldt = seq.Date(from = as.Date("2018-01-01"), length.out = 10, by = "day"),
#'   x = 1:10)
#' LagDT(DT, "x", date.id = "caldt")
#' LagDT(DT, "x", shift.by = c(-1, 1, 3), date.id = "caldt")
#' DT[, y := 101:110]
#' LagDT(DT, c("x", "y"), shift.by = 1:3, date.id = "caldt")
#' DT.panel <- data.table(
#'   fund = rep(1:2, each = 5),
#'   caldt = rep(seq.Date(from = as.Date("2018-01-01"),
#'                        length.out = 5, by = "day"), 2),
#'   x = c(paste0("fund1x", 1:5), paste0("fund2x", 1:5)),
#'   y = c(paste0("fund1y", 101:105), paste0("fund2y", 101:105)))
#' LagDT(DT.panel, c("x", "y"),
#'       shift.by = 1:3, date.id = "caldt", panel.id = "fund")

LagDT <- function(dt, x,
                  shift.by = 1,
                  date.id = "date",
                  panel.id = NULL) {

  # input must be a data.table
  if (!"data.table" %in% class(dt)) {
    stop("dt must be a data.table object")
  }

  # make sure date variable is present
  if (!date.id %in% names(dt)) {
    stop("date.id not found in dt")
  }

  # make sure panel.id variable is present if supplied
  if (!is.null(panel.id)) {
    if (!panel.id %in% names(dt)) {
      stop("panel.id supplied not found in dt")
    }
  }

  # shift.by cannot be zero
  if (any(shift.by %in% 0)) {
    stop("Lag/lead values must be different from zero")
  }

  # raise a warning if class of date variable is not "Date" or "yearmon"
  if (!class(dt[, .SD, .SDcols = date.id][[date.id]]) %in% c("Date", "yearmon")) {
    warning("The date variable you fed the function is not of class 'Date' (standard date format) or 'yearmon' (from the zoo package). I have not tested the function on such date formats. As long as adding '1' to your date shifts time by one period, you should be fine. Otherwise, the function will shift time by more or less than you intended.")
  }

  # loop through shift extents
  for (s in shift.by) {
    # name lag/lead variables to be generated
    # lags: add ".l" followed by number of periods shifted
    if (s > 0) {
      x.shift <- paste0(x, ".l", s)
      # leads: add ".f" followed by
    } else {
      x.shift <- paste0(x, ".f", abs(s))
    }

    # handle yearmon class differently
    if (class(dt[, .SD, .SDcols = date.id][[date.id]]) == "yearmon") {
      s <- s/12
    }

    # make a copy of the data table with the variable(s) to be shifted
    dt.shift <- copy(dt[, .SD, .SDcols = c(panel.id, date.id, x)])[
      # shift date
      , eval(date.id) := get(date.id) + s]
    # rename variable
    setnames(dt.shift, c(panel.id, date.id, x.shift))

    # merge back to master dataset
    #dt <- merge(dt, dt.shift, by = c(panel.id, date.id), all.x = TRUE)
    nm <- names(dt)
    dt <- dt.shift[dt, on = c(panel.id, date.id)]
    # order columns so lagged variable is at the end
    setcolorder(dt, c(nm, x.shift))
  }

  # return dataset with lags added back in
  return(dt)
}
