#' Output Regression Results to data.table
#'
#' @param reg.model Regression model, e.g. an lm object.
#' @param digits Number of digits to round coefficients, standard errors.
#' @param print.tstat Should t-stats be displayed instead of standard errors?
#' @param sig.levels Significance levels based on which stars should be assigned, if any.
#' @param sig.symbols Significance symbols to assign. Should be in descending order of significance.
#'
#' @return A data.table of regression coefficients, standard errors (or t-stats), R-squared, number of observations. Standard errors (or t-stats) are labelled starting with "_SE_". If available (e.g. for an felm object), the R-squared of the projected model is displayed, or, if a 2SLS model is estimated via felm, the F-statistic of the first stage (instead of R-squareds in this case).
#' 
#' @export
#'
#' @examples
#' reg.results <- lm(mpg ~ disp + hp, mtcars)
#' RegToDT(reg.results)

RegToDT <- function(reg.model,
                    digits = 3,
                    print.tstat = FALSE,
                    sig.levels = c(0.01, 0.05, 0.1),
                    sig.symbols = c("***", "**", "*")) {

    # reg summary
    reg.model.sum <- summary(reg.model)
    reg.model.vars <- rownames(reg.model.sum$coefficients)
    if (any(grepl("^_SE_", reg.model.vars))) {
      stop("Please do not start variable names with _SE_")
    }
    reg.model.coef <- reg.model.sum$coefficients[, 1]
    reg.model.se   <- reg.model.sum$coefficients[, 2]
    reg.model.t    <- reg.model.sum$coefficients[, 3]
    reg.model.p    <- reg.model.sum$coefficients[, 4]

    # number of obs
    out.N <- NA
    try(
      out.N <- format(reg.model$N, big.mark = ","), silent = TRUE
      )
    try(
      out.N <- format(nobs(reg.model), big.mark = ","), silent = TRUE
      )

    # R-squared
    out.r2 <- formatC(
      round(reg.model.sum$r.squared, digits), digits, format = "f")

    # R-squared of projected model, if available
    if (!is.null(reg.model.sum$P.r.squared)) {
      out.r2.proj <- formatC(
        round(reg.model.sum$P.r.squared, digits), digits, format = "f")
    }

    # first stage F-stat, if available
    if (!is.null(reg.model.sum$E.fstat)) {
      out.Fstat <- formatC(
        round(reg.model.sum$E.fstat[["F"]], 1), 1, format = "f")
    }

    # coefficients, with significance stars
    out.coef <- paste0(formatC(
      round(reg.model.coef, digits), digits, format = "f"),
      PvalToStar(reg.model.p, sig.levels, sig.symbols))

    if (!print.tstat) {
      # standard errors, with parentheses
      out.se <- paste0("(", formatC(
        round(reg.model.se, digits), digits, format = "f"), ")")
    } else {
      # t-stats, with parentheses
      out.se <- paste0("(", formatC(
        round(reg.model.t, 2), 2, format = "f"), ")")
    }

    # add a space after each variable label
    out.vars <- c(rbind(reg.model.vars, paste0("_SE_", reg.model.vars)))

    # put together the table
    out.body <- c(rbind(out.coef, out.se), out.N)
    out.lab  <- c(out.vars, "Observations")

    # if first stage F-stat is available, report it instead of R^2
    if (!is.null(reg.model.sum$E.fstat)) {
      out.body <- c(out.body, out.Fstat)
      out.lab  <- c(out.lab, "F (first stage)")
    # if no first stage F-stat, report R^2
    } else {
      out.body <- c(out.body, out.r2)
      out.lab  <- c(out.lab, "$R^2$")
      # add R^2 of projected model if available
      if (!is.null(reg.model.sum$P.r.squared)) {
        out.body <- c(out.body, out.r2.proj)
        out.lab  <- c(out.lab, "$R^2$ (proj. model)")
      }
    }

    # output
    out <- data.table(Variable = out.lab, Estimate = out.body)
    return(out)
  }
