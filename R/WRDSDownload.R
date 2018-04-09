#' Download data from WRDS
#'
#' Makes a query for the WRDS server, and saves the resulting dataset to disk.
#'
#' @param qstr A vector of three strings: (i) name of source dataset, (ii) query for data to be pulled, (iii) query for variable names or supplied variable names.
#' @param nobs Number of observations to download (default is all).
#' @param con Name of the connection (default wrds).
#' @param out.dir Path to directory for saving downloaded dataset.
#' @param dt.format Should the saved dataset be in data.table format? Default is TRUE.
#'
#' @export
#'
#' @importFrom data.table setnames

WRDSDownload <- function(qstr,
                         nobs = -1,
                         con = wrds,
                         out.dir = data.dir,
                         dt.format = TRUE) {
  # print name of query to console
  print(paste("Performing query", qstr[2], "..."))

  # retrieve data
  dt <- WRDSQuery(qstr[2])

  # is the query for all variables in the dataset?
  q.all.vars <- grepl("select \\*", qstr[2])

  # if yes, then grab variable names from database and assign to dataset
  if (q.all.vars == TRUE) {
    dt.names <- WRDSQuery(qstr[3], dt.format = FALSE)
    dt.names <- gsub("\\s+", "", dt.names$`Column Name`)
  } else {
  # otherwise, use user-specified list of variable names
    dt.names <- unlist(strsplit(qstr[3], ","))
    dt.names <- gsub("\\s+", "", dt.names)
  }

  # set clean variable names
  if (dt.format) {
    setnames(dt, dt.names)
  } else {
    names(dt) <- dt.names
  }

  # save data
  saveRDS(dt, file.path(out.dir, paste0(qstr[1], ".Rds")))
  print("...done")
}
