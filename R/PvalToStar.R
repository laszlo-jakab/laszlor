#' Assign significance stars to p-values
#'
#' @param p.val Vector of p-values to which we want significance stars assigned.
#' @param sig.levels Optional vector containing significance thresholds. Default is 1\%, 5\%, and 10\% significance.
#' @param sig.symbols Vector of symbols to denote significance. Symbols must be provided in decreasing order of significance.
#'
#' @return A vector of significance stars.
#' 
#' @export
#'
#' @examples
#' PvalToStar(c(0.009, 0.049, 0.09, 0.11))
#'
#' my.levels <- c(0.001, 0.01, 0.05)
#' my.symbols <- c("a", "b", "+")
#' PvalToStar(c(0.009, 0.049, 0.09, 0.11), my.levels, my.symbols)

PvalToStar <- function(p.val,
                       sig.levels = c(0.01, 0.05, 0.1),
                       sig.symbols = c("***", "**", "*")) {
  # make sure breakpoints are in ascending order
  sig.levels <- sort(sig.levels)
  # placeholder for significance
  star.out <- rep("", length(p.val))
  # assign stars
  star.out[p.val < sig.levels[3]] <- sig.symbols[3]
  star.out[p.val < sig.levels[2]] <- sig.symbols[2]
  star.out[p.val < sig.levels[1]] <- sig.symbols[1]
  return(star.out)
}
