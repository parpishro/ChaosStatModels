#' Perform MCMC Sampling for Synthetic Likelihood Estimation
#'
#' This function implements an MCMC sampler for synthetic likelihood estimation, 
#' using a Metropolis-Hastings algorithm with an optional adaptive proposal distribution.
#'
#' @param object An object of class `synlik` containing the synthetic likelihood model.
#' @param initPar A numeric vector of initial parameter values for the MCMC sampler.
#' @param niter An integer specifying the number of MCMC iterations to perform.
#' @param nsim An integer specifying the number of simulations to generate at each MCMC step.
#' @param propCov A covariance matrix or numeric vector for the proposal distribution. If a vector is provided, 
#'   it will be converted into a diagonal matrix.
#' @param burn An integer specifying the number of burn-in iterations (default: 0).
#' @param priorFun A function that evaluates the log-prior density given parameter values. 
#'   Defaults to a function returning 0 (flat prior).
#' @param targetRate A numeric value specifying the target acceptance rate for adaptive proposals (default: `NULL`).
#' @param control A list of control parameters for the MCMC sampler. Defaults include:
#'   - `theta`: Step size for adaptation (default: 0.5).
#'   - `adaptStart`: Iteration at which adaptation starts (default: 0).
#'   - `adaptStop`: Iteration at which adaptation stops (default: `niter + burn`).
#'   - `verbFreq`: Frequency of verbose output (default: 500).
#'   - `verbose`: Logical, whether to print progress (default: `FALSE`).
#' @param ... Additional arguments (not used directly).
#'
#' @return An object of class `smcmc`, which contains:
#' - `initPar`: Initial parameter values.
#' - `niter`: Total number of iterations performed.
#' - `nsim`: Number of simulations per iteration.
#' - `propCov`: Proposal covariance matrix.
#' - `burn`: Number of burn-in iterations.
#' - `priorFun`: Prior function.
#' - `targetRate`: Target acceptance rate.
#' - `control`: Control parameters.
#' - `accRate`: Final acceptance rate.
#' - `chains`: Matrix of sampled parameter values.
#' - `llkChain`: Log-likelihood values at each step.
#'
#' @details
#' The function adapts the proposal covariance matrix during the sampling process if 
#' `targetRate` is provided. It uses synthetic likelihood evaluation via the `slik` function, 
#' which calculates the log-likelihood based on the synthetic likelihood model defined in `object`.
#'
#' @examples
#' # Example usage (assuming `my_synlik_object` is a valid synlik object):
#' init_par <- c(alpha = 0.5, beta = 0.1)
#' prop_cov <- diag(c(0.01, 0.01))
#' result <- sl_mcmc(
#'   object = my_synlik_object,
#'   initPar = init_par,
#'   niter = 10000,
#'   nsim = 100,
#'   propCov = prop_cov
#' )
#'
#' @importFrom synlik .ctrlSetup .adaptChol slik
#' @export
sl_mcmc <- function(
    object, initPar, niter, nsim, propCov, burn = 0, 
    priorFun = function(param) 0, targetRate = NULL, control = list()
) {
  y <- object@data
  N <- niter + burn
  ctrl <- list(
    theta = 0.5, adaptStart = 0, adaptStop = N, verbFreq = 500, verbose = FALSE
  )
  ctrl <- synlik:::.ctrlSetup(innerCtrl = ctrl, outerCtrl = control)
  
  if (is.matrix(propCov) == FALSE) {
    propCov <- diag(propCov)
  }
  if (nrow(propCov) != ncol(propCov)) {
    stop("propCov should be a square matrix")
  }
  if (nrow(propCov) != length(initPar)) {
    stop("nrow(propCov) != length(initPar)")
  }
  fixPar <- which(diag(propCov) == 0)
  anyFix <- (length(fixPar) > 0)
  savedCov <- propCov
  if (anyFix) {
    diag(propCov)[fixPar] <- 0.1
  }
  cholFact <- t(chol(unname(propCov)))
  nPar <- length(initPar)
  parNames <- names(initPar)
  currPar <- unname(initPar)
  propPar <- numeric(nPar)
  mcmcSample <- matrix(NA, niter, nPar)
  llkChain <- numeric(niter)
  currLogLik <- propLogLik <- tmpLik <- -10^99
  currPrior <- priorFun(initPar)
  accept <- 0
  unifVar <- runif(N)
  
  storeIndex <- 1
  for (ii in 1:N) {
    pert <- rnorm(nPar)
    propPar <- currPar + as.vector(cholFact %*% pert)
    if (anyFix) {
      propPar[fixPar] <- currPar[fixPar]
    }
    names(propPar) <- parNames
    propPrior <- priorFun(propPar)
    names(propPar) <- parNames
    if (is.finite(propPrior)) {
      propLogLik <- try(slik(object, param = propPar, nsim = nsim), silent = TRUE)
      if (!is.numeric(propLogLik) || !is.finite(propLogLik)) {
        propLogLik <- -Inf
      }
      
      likDiff <- propLogLik - currLogLik + propPrior -
        currPrior
      alpha <- min(1, exp(likDiff))
      if (!is.finite(alpha)) {
        alpha <- ifelse(likDiff >= 0, 1, 0)
      }
      if (likDiff > log(unifVar[ii])) {
        currPar <- propPar
        currPrior <- propPrior
        currLogLik <- propLogLik
        if (ii > burn) {
          accept <- accept + 1
        }
      }
    } else {
      alpha <- 0
    }
    if (ii > burn) {
      mcmcSample[storeIndex, ] <- currPar
      llkChain[storeIndex] <- currLogLik
      storeIndex <- storeIndex + 1
    }
    if (!is.null(targetRate) && (ii >= ctrl$adaptStart) &&
        (ii <= ctrl$adaptStop)) {
      cholFact <- synlik:::.adaptChol(
        nPar = nPar, iter = ii, S = cholFact,
        U = pert, gamma = ctrl$theta, alpha = alpha,
        accRate = targetRate
      )
    }
    
    if (ctrl$verbose == TRUE && (ii > burn) && !(ii %% ctrl$verbFreq)) {
      tmp <- colMeans(mcmcSample[1:ii, ], na.rm = TRUE)
      names(tmp) <- names(object@param)
      cat(paste(
        "Empirical posterior means at iteration",
        ii - burn, "\n"
      ))
      print(tmp)
    }
  }
  
  return(new("smcmc", object,
             initPar = initPar, niter = as.integer(niter),
             nsim = as.integer(nsim), propCov = propCov, burn = as.integer(burn),
             priorFun = priorFun, targetRate = targetRate, 
             control = control, accRate = accept / niter, chains = mcmcSample,
             llkChain = llkChain
  ))
}
