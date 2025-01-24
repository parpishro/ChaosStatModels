# prior function
library(pomp)
pmcmc_dprior <- Csnippet("
  if (
    logsigma >= (log(0.01) - 10) && logsigma <= 10) &&  // Restrict logsigma
    logeta >= (log(0.01) - 10) && logeta <= 10) &&      // Restrict logeta
    alpha > 0 && alpha <= 10                          // Restrict alpha
  ) {
    lik = 0;  // Log-probability = 0 for uniform distribution within bounds
  } else {
    lik = -INFINITY;  // Log-probability = -Inf for out-of-bounds parameters
  }
")
