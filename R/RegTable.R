#' Convert List of Results to Regression Table
#'
#' @param reg.list A list of regression outputs (e.g. lm objects). Should be more than one regression object.
#' @param digits Number of digits to display for coefficients, standard errors. T-stats are forced to be displayed as two digits.
#' @param fe.list Optional matrix/data.table of fixed effect labels to be added after the coefficients.
#' @param coef.lab.dt Optional data.table of coefficient labels. Must include the following columns: (i) old.name: variable names in the dataset, (ii) new.name: variable labels to display. Regression table coefficients will be displayed in the order of the variables in this table. The table of coefficient labels can include more variables than what is included in the given regression table.
#' @param col.names Optional column names to display. Default is to label columns as (1), (2), (3), etc.
#' @param print.tstat Should t-stats be displayed instead of standard errors?
#' @param sig.levels Levels for which significance stars should be assigned, if any. Default is 1\%, 5\%, 10\%.
#' @param sig.symbols Significance stars to be displayed.
#' 
#' @return A data.table of regression 
#' 
#' @export
#' 
#' @import data.table
#'
#' @examples
#' reg.models <- c("mpg ~ disp", "mpg ~ disp + hp", "mpg ~ disp + hp + wt")
#' reg.results <- lapply(reg.models, function(x) lm(x, mtcars))
#' RegTable(reg.results)
#' coef.lab.dt <- data.table(
#'   old.name = c("(Intercept)", "disp", "hp", "wt"),
#'   new.name = c("Intercept", "Displacement", "Horsepower", "Weight"))
#' RegTable(reg.results, coef.lab.dt = coef.lab.dt, print.tstat = TRUE)
#' RegTable(reg.results, coef.lab.dt = coef.lab.dt[2:nrow(coef.lab.dt)])

RegTable <- function(reg.list,
                     digits = 3,
                     fe.list = NULL,
                     coef.lab.dt = NULL,
                     col.names = NULL,
                     print.tstat = FALSE,
                     sig.levels = c(0.01, 0.05, 0.1),
                     sig.symbols = c("***", "**", "*")) {

  # format each individual regression as a data.table of coefs
  reg.dt.list <- lapply(reg.list, RegToDT,
                        digits = digits,
                        print.tstat = print.tstat,
                        sig.levels = sig.levels,
                        sig.symbols = sig.symbols)

  # construct the reg table
  reg.num <- 1
  reg.table <- reg.dt.list[[1]]
  reg.dt.list[[1]] <- NULL

  setnames(reg.table, "Estimate", paste0("(", reg.num,")"))
  # loop through reg outputs, merging them into the table
  while (length(reg.dt.list) > 0) {
    reg.table <- merge(
      reg.table, reg.dt.list[[1]], on = "Variable", all = TRUE, sort = FALSE)
    reg.num <- reg.num + 1
    setnames(reg.table, "Estimate", paste0("(", reg.num,")"))
    reg.dt.list[[1]] <- NULL
  }

  # coefficients and sd
  coef.sd <- reg.table[!Variable %in%
    c("Observations", "$R^2$", "$R^2$ (proj. model)", "F (first stage)")]
  coef.sd[
    # group by variable
    , coef.grp := gsub("^_SE_", "", Variable)][
    # identify if row is a standard deviation (or tstat)
    , is.SE := grepl("^_SE", Variable)]

  # observations, R2
  obs.r2 <- reg.table[Variable %in%
    c("Observations", "$R^2$", "$R^2$ (proj. model)", "F (first stage)")]

  # custom coefficient labels and ordering
  if (!is.null(coef.lab.dt)) {
    coef.lab.dt[, position := .I]
    coef.sd <- coef.sd[
      # add in custom labels
      coef.lab.dt, on = c(coef.grp = "old.name"), nomatch = 0][
      # update labels
      , Variable := new.name]
    # custom sort
    setkey(coef.sd, position, is.SE)
  }
  # format variable labels
  coef.sd[is.SE == TRUE, Variable := ""]
  coef.sd <- coef.sd[, 1:(ncol(obs.r2))]

  # add fixed effect labels
  if (!is.null(fe.list)) {
    colnames(fe.list) <- names(obs.r2)
    obs.r2 <- rbind(fe.list, obs.r2)
  }

  # paste together output
  out.dt <- rbind(coef.sd, obs.r2)
  # replace NAs with blank
  out.dt[is.na(out.dt)] <- ""

  # remove unnecessary label
  setnames(out.dt, "Variable", "")
  # name columns if supplied
  if (!is.null(col.names)) {
    setnames(out.dt, c("", col.names))
  }

  # output table
  return(out.dt)
}
