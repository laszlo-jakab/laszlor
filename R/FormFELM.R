#' Generate felm formula
#'
#' @param y LHS variable
#' @param x RHS variables
#' @param fe FE to be projected out
#' @param iv Formula for IV specification
#' @param cl Variables to cluster by
#'
#' @return felm formula
#'
#' @export
#'
#' @examples
#' y <- "mpg"
#' x <- c("wt", "wt + qsec")
#' fe <- "cyl"
#' iv <- "0"
#' cl <- "cyl"
#' mtcars.models <- FormFELM(y, x, fe, iv, cl)
#' res <- lapply(mtcars.models, felm, mtcars)
#' lapply(res, summary)

FormFELM <- function(y, x, fe, iv, cl) {
  # LHS ~ RHS
  base.form <- paste(y, x, sep = " ~ ")
  # LHS ~ RHS | FE | IV | clusters
  full.form <- paste(base.form, fe, iv, cl, sep = " | ")
  # convert to formula
  full.list <- lapply(full.form, as.formula)
  return(full.list)
}
