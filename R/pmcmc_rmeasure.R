#' Measurement Model for PMCMC
#'
#' A measurement model implemented as a Csnippet for use in `pomp`-based PMCMC simulations. 
#' The model generates observations (`obs`) based on the hidden state (`x`) and measurement noise.
#'
#' @param logsigma Logarithm of the measurement noise standard deviation (`sigma`).
#' @return A Csnippet defining the measurement model.
#' @importFrom pomp Csnippet
pmcmc_rmeasure <- Csnippet("
  double sigma = exp(logsigma);
  obs = rnorm(x, sigma);
")
