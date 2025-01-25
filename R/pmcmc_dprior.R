#' Prior Function for PMCMC
#'
#' A prior probability density model implemented as a Csnippet for use in `pomp`-based PMCMC simulations. 
#' This snippet defines a uniform prior over specific bounds for the parameters and returns the log-probability (`lik`).
#' 
#' The following parameter restrictions are imposed:
#' - `logsigma`: Must lie within \([ \log(0.01) - 10, 10 ]\)
#' - `logeta`: Must lie within \([ \log(0.01) - 10, 10 ]\)
#' - `alpha`: Must lie within \( (0, 10] \)
#' 
#' Parameters outside these bounds will result in a log-probability of `-INFINITY`, while parameters within the bounds will return a log-probability of `0`.
#'
#' @return A Csnippet defining the prior function.
#' @importFrom pomp Csnippet
library(pomp)
pmcmc_dprior <- Csnippet("
  if (
    logsigma >= (log(0.01) - 10) && logsigma <= 10 &&  // Restrict logsigma
    logeta >= (log(0.01) - 10) && logeta <= 10 &&      // Restrict logeta
    alpha > 0 && alpha <= 10                           // Restrict alpha
  ) {
    lik = 0;  // Log-probability = 0 for uniform distribution within bounds
  } else {
    lik = -INFINITY;  // Log-probability = -Inf for out-of-bounds parameters
  }
")
