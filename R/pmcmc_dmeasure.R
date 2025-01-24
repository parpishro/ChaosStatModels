#' Measurement Density for PMCMC
#'
#' A measurement density model implemented as a Csnippet for use in `pomp`-based PMCMC simulations. 
#' The model computes the likelihood (`lik`) of the observed data (`obs`) given the hidden state (`x`) 
#' and the measurement noise.
#'
#' @param logsigma Logarithm of the measurement noise standard deviation (`sigma`).
#' @param give_log Logical indicator; if `TRUE`, the log-likelihood is returned.
#' @return A Csnippet defining the measurement density model.
#' @importFrom pomp Csnippet
pmcmc_dmeasure <- Csnippet("
  double sigma = exp(logsigma);
  lik = dnorm(obs, x, sigma, give_log);
")
