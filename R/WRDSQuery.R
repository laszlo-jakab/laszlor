#' Download data from WRDS
#'
#' @param wrds.query A string containing the query for the WRDS server.
#' @param nobs The number of records to download (default is all).
#' @param con The name of the connection to use (defaults to wrds).
#' @param dt.format Should the returned dataset be a data.table? Default is yes.
#'
#' @return The downloaded dataset. By default, in data.table format.
#'
#' @export
#'
#' @importFrom RJDBC dbSendQuery
#' @importFrom DBI dbFetch
#' @importFrom data.table setDT

WRDSQuery <- function(wrds.query, nobs = -1, con = wrds, dt.format = TRUE) {
  # send query
  db.query <- dbSendQuery(con, wrds.query)
  # download data
  dt <- dbFetch(db.query, n = nobs)
  # clear query
  dbClearResult(db.query)
  # convert to data.table (default behavior)
  if (dt.format) {
    dt <- setDT(dt)
  }
  # return downloaded data
  return(dt)
}
