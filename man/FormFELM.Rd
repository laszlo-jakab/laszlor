% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FormFELM.R
\name{FormFELM}
\alias{FormFELM}
\title{Generate felm formula}
\usage{
FormFELM(y, x, fe, iv, cl)
}
\arguments{
\item{y}{LHS variable}

\item{x}{RHS variables}

\item{fe}{FE to be projected out}

\item{iv}{Formula for IV specification}

\item{cl}{Variables to cluster by}
}
\value{
felm formula
}
\description{
Generate felm formula
}
\examples{
y <- "mpg"
x <- c("wt", "wt + qsec")
fe <- "cyl"
iv <- "0"
cl <- "cyl"
mtcars.models <- FormFELM(y, x, fe, iv, cl)
res <- lapply(mtcars.models, felm, mtcars)
lapply(res, summary)
}
