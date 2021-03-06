% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WRDSQuery.R
\name{WRDSQuery}
\alias{WRDSQuery}
\title{Download data from WRDS}
\usage{
WRDSQuery(wrds.query, nobs = -1, con = wrds, dt.format = TRUE)
}
\arguments{
\item{wrds.query}{A string containing the query for the WRDS server.}

\item{nobs}{The number of records to download (default is all).}

\item{con}{The name of the connection to use (defaults to wrds).}

\item{dt.format}{Should the returned dataset be a data.table? Default is yes.}
}
\value{
The downloaded dataset. By default, in data.table format.
}
\description{
Download data from WRDS
}
