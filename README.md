# ChaosStatModels

These two papers should be read together. They argue that the usual way to perform inference for 
dynamic systems when they are "almost chaotic" may intrinsically be unreliable, and propose 
synthetic likelihood as a feasible alternative.

### 1. **Introduction**
   - Topic: Inferential Methods for Highly Nonlinear State Space Models in Ecology and Epidemiology
   - Importance: challenges in statistical inference due to the chaotic or near-chaotic dynamics
   - Objective: Comparison of two main classes of methods (information reduction and state-space 
   methods) to handle these systems

### 2. **Background and Motivation**
   - Context: Nonlinear dynamics in ecology and epidemiology (e.g., pest species, infectious diseases).
   - Problem: Extreme sensitivity to small changes in system states and parameters
   - Remedy: Two primary approaches:
     - Information reduction methods (e.g., Approximate Bayesian Computation, Synthetic Likelihood)
     - State-space methods (e.g., Particle Markov chain Monte Carlo, Iterated Filtering)

### 3. **Methods Overview**
   - **Information Reduction Methods:**
     - Approximate Bayesian Computation (ABC): a computational method for Bayesian inference that 
     estimates posterior distributions using simulated data and summary statistics, bypassing the 
     need for explicit likelihood calculations
     - Synthetic Likelihood (SL): a method for Bayesian inference that approximates the likelihood 
     function by modeling the summary statistics of the data as a multivariate normal distribution, 
     facilitating parameter estimation when the true likelihood is intractable
   - **State-Space Methods:**
     - Particle Markov chain Monte Carlo (PMCMC): Particle Markov Chain Monte Carlo (PMCMC) is a 
     Bayesian inference method that combines particle filtering with MCMC to estimate posterior 
     distributions in state-space models, enabling inference in complex, time-varying systems where 
     standard MCMC would be inefficient or unfeasible
     - Iterated Filtering (IF): Iterated Filtering (IF) is a method for maximum likelihood 
     estimation in state-space models, where parameter estimates are iteratively updated using a 
     particle filter to maximize the likelihood by progressively narrowing down the parameter space
     - Parameter Cascading: a hierarchical estimation method where parameters are estimated in 
     stages, using estimates from earlier stages to inform subsequent ones, reducing computational 
     complexity and improving efficiency in multi-level or complex models

### 4. **Challenges in Nonlinear State Space Models**
   - Multimodality in the parameter space: Small changes in parameters can lead to drastically 
   different outcomes due to the system's sensitivity, resulting in multiple distinct regions in the
   parameter space where the likelihood is high
   - Particle filters with low process noise fail: Particles, which represent possible states of the
   system, tend to collapse to a few high-likelihood values, causing a loss of diversity. This 
   occurs because, with low process noise, the model becomes overly deterministic, and small 
   inaccuracies in state estimation are not corrected, leading most particles to have negligible 
   weights. As a result, the filter suffers from particle degeneracy, where only a few particles 
   contribute to the estimate, degrading performance and preventing accurate tr
.

### 5. **Comparative Analysis**
   - Results from the simulations and real-world (e.g., Blowflies data, Cholera Epidemics) 
   applications confirms
   - The performance of each method is measured using:
     - Error metrics, e.g., Mean Squared Error
     - Computational efficiency and convergence issues

### 6. **Discussion**
   - Neither method is universally superior
   - Discuss practical implications for researchers in ecology and epidemiology.
   - Use information reduction methods for model development and checking
   - Switch to state-space methods for final inference

