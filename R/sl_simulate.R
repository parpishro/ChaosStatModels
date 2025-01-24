#' Simulate Fast Subsystems of the Rulkov Map
#'
#' This function simulates fast subsystems of the Rulkov map based on the given parameters, 
#' burn-in period, number of observations, and additional arguments.
#'
#' @param param A named numeric vector containing model parameters. 
#'   Must include:
#'   - `alpha`: A numeric value controlling the subsystem dynamics.
#'   - `logsigma`: Log-transformed standard deviation for observational noise.
#'   - `logeta`: Log-transformed standard deviation for process noise.
#' @param nsim An integer specifying the number of simulation trajectories to generate.
#' @param extraArgs A list of additional arguments:
#'   - `nBurn`: An integer specifying the burn-in period (number of iterations to discard).
#'   - `nObs`: An integer specifying the number of observations to generate.
#'   - `randInit` (optional): A logical value indicating whether to randomly initialize trajectories (default: `TRUE`).
#'   - `initVal` (optional): A numeric value for the initial value of the fast subsystem (overrides `randInit`).
#' @param ... Additional arguments passed to the function (not used directly).
#'
#' @return A matrix of simulated trajectories with dimensions `nsim` x (`nBurn` + `nObs`). 
#'   Each row corresponds to one trajectory.
#'
#' @examples
#' # Define parameters
#' params <- c(alpha = 4, logsigma = log(0.5), logeta = log(0.1))
#' 
#' # Define additional arguments
#' extraArgs <- list(nBurn = 100, nObs = 200, randInit = TRUE)
#' 
#' # Simulate 10 trajectories
#' simulations <- sl_simulate(params, nsim = 10, extraArgs = extraArgs)
#' 
#' @export
sl_simulate <- function(param, nsim, extraArgs, ...) {
  nBurn <- extraArgs$nBurn  # Burn-in period
  nObs <- extraArgs$nObs  # Number of observations to generate 
  
  # Handle initialization
  randInit <- ifelse(is.null(extraArgs$randInit), TRUE, extraArgs$randInit)
  
  # Extract parameters
  alpha <- param['alpha']
  sigma <- exp(param['logsigma'])
  y <- -2.8
  eta <- exp(param['logeta'])
  
  # Simulation function for one trajectory
  sim <- function() {
    x_vec <- numeric(nBurn + nObs)
    x_obs <- numeric(nBurn + nObs)
    x_vec[1] <- ifelse(is.null(extraArgs$initVal), runif(1), extraArgs$initVal)  # Initial value for x (fast subsystem)
    x_obs[1] <- x_vec[1] + sigma*rnorm(1)
    
    for (j in 2:(nBurn + nObs)) {
      x_vec[j] <- alpha/(1 + x_vec[j - 1]^2) + y + eta*rnorm(1)
      x_obs[j] <- x_vec[j] + sigma*rnorm(1)
    }
    return(x_obs)
  }
  
  # Generate multiple trajectories
  x <- replicate(nsim, sim())
  return(t(x))
}
