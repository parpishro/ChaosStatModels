#' State Transition Model for PMCMC
#'
#' This snippet defines the state transition model for a particle Markov chain Monte Carlo (PMCMC) 
#' algorithm, implemented using a `pomp`-compatible Csnippet.
#'
#' @details
#' The state transition model is specified as:
#' - The process noise is modeled as `e`, drawn from a normal distribution with mean 0 and standard deviation `eta`.
#' - The state update equation is:
#'   \deqn{x = \frac{\alpha}{1 + x^2} - 2.8 + e}
#'
#' Here, `logeta` is the logarithm of the standard deviation `eta` for the process noise.
#'
#' This code snippet can be incorporated into a `pomp` object for PMCMC simulations.
#'
#' @importFrom pomp Csnippet
#' 
#' @export
pmcmc_step <- Csnippet("
  double eta = exp(logeta);
  double e = rnorm(0, eta);
  x = alpha / (1 + x * x) - 2.8 + e;
")
