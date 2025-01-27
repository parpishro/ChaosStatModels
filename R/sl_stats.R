#' Compute Summary Statistics for Simulated Data
#'
#' This function computes various summary statistics from simulated data, including mean, 
#' standard deviation, mean-median difference, autocorrelations, zero crossings, 
#' and other features. Some additional experimental statistics are included as commented-out code.
#'
#' @param x A numeric matrix where each row represents a simulation trajectory. If a vector is provided, 
#'   it will be converted into a single-row matrix.
#' @param extraArgs A list of additional arguments (not currently used directly in this implementation).
#' @param ... Additional arguments passed to the function (not used directly).
#'
#' @return A numeric matrix where each row contains the summary statistics for the corresponding row in `x`.
#'
#' @details
#' The function includes several commented-out lines for computing additional experimental statistics, 
#' such as skewness, kurtosis, nonlinear autoregressive features, and turning points. These lines are 
#' intentionally left as comments to allow flexibility in future experiments.
#'
#' Summary statistics include:
#' - Mean and standard deviation of each trajectory.
#' - Mean-median difference.
#' - Autocorrelation features (using `slAcf` function for a maximum lag of 11).
#' - Count of zero crossings in the trajectory.
#' - Proportion of power in low-frequency bands (from the power spectrum).
#' - Partial autocorrelations at lags 1 and 2.
#' - Logit-transformed low-frequency power ratio (to improve normality).
#'
#' @examples
#' # Example input: simulated trajectories
#' simulated_data <- matrix(rnorm(1000), nrow = 10)  # 10 trajectories, each of length 100
#' 
#' # Compute summary statistics
#' stats <- sl_stats(simulated_data, extraArgs = list())
#' 
#' @importFrom moments skewness kurtosis
#' @export
sl_stats <- function(x, extraArgs, ...) {
  if (!is.matrix(x)) 
    x  <- matrix(x, 1, length(x))  # Ensure input is a matrix
  
  tx <- t(x)
  S  <- cbind(
    t(nlar(tx, lag = c(6, 6, 6, 1, 1), power = c(1, 2, 3, 1, 2))),  # Nonlinear autoregressive features
    rowMeans(x), 
    rowMeans(x) - apply(x, 1, median),  # Mean and mean-median difference
    t(slAcf(tx, max.lag = 11)),  # Autocorrelation features
    apply(t(abs(apply(sign(apply(x, 1, diff)), 2, diff))), 1, sum)/2 # Count of zero crossings
  )  
  
  return(S)
}

#   tx <- t(x)
#   S  <- cbind(
#     rowMeans(x),
#     apply(x, 1, sd),  # Standard deviation
#     #apply(x, 1, moments::skewness),  # Skewness
#     #apply(x, 1, moments::kurtosis),  # Kurtosis
#     #t(nlar(t(x), lag = c(6, 6, 6, 1, 1), power = c(1, 2, 3, 1, 2))),  # Nonlinear autoregressive features
#     rowMeans(x) - apply(x, 1, median),  # Mean and mean-median difference
#     t(slAcf(tx, max.lag = 11)), # Autocorrelation features
#     apply(t(abs(apply(sign(apply(x, 1, diff)), 2, diff))), 1, sum) / 2, # Count of zero crossings
#     # apply(x, 1, function(row) {
#     #   distances <- dist(row)
#     #   sum(distances < 0.1) / length(distances)  # Recurrence density
#     # }),
#     apply(x, 1, function(row) {
#       ps <- abs(fft(row))^2  # Power spectrum
#       sum(ps[2:6]) / sum(ps)  # Proportion of power in low-frequency bands
#     }),
#     #apply(x, 1, function(row) sum(diff(sign(diff(row))) != 0)), # Turning points
#     # apply(x, 1, function(row) { #  Inter-spike interval (example: mean interval above a chosen threshold)
#     #   threshold <- 0.5  # or any suitable value
#     #   spikeInds <- which(row > threshold)
#     #   if (length(spikeInds) < 2) return(NA_real_)
#     #   intervals <- diff(spikeInds)
#     #   mean(intervals)
#     # }),
#     # apply(x, 1, function(row) { # Bursting proportion (fraction of samples above a threshold)
#     #   threshold <- 0.5
#     #   mean(row > threshold)
#     # }),
#     t(apply(x, 1, function(row) { #Partial autocorrelation at lags up to 5
#       pac <- pacf(row, lag.max = 5, plot = FALSE)$acf
#       c(pac[1], pac[2])
#     })),
#     logitLowFreq = apply(x, 1, function(row) { #Logit transform of the low-frequency power ratio (improves normality)
#       ps <- abs(fft(row))^2
#       ratio <- sum(ps[2:6]) / sum(ps)
#       # Simple logit transform; watch out for ratio near 0 or 1
#       log(ratio / (1 - ratio))
#     })
#   )
#   return(S)
# }
