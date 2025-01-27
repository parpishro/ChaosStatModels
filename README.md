# Inference on Non-linear Chaotic Dynamical Systems

This repository contains the implementation and analysis of two statistical inference methods applied to the fast subsystem of the stochastic Rulkov map, a model for neuronal spiking and bursting behavior. The project evaluates the **Synthetic Likelihood (SL)** method and **Particle Markov Chain Monte Carlo (PMCMC)** to assess their performance in parameter estimation under chaotic dynamics and noise.

## File Structure

- **`report.pdf`**: The detailed report analyzing the two methods, including results, methodology, and conclusions.
- **`report.rmd`**: The detailed document and code scripts that create the report pdf. 
- **`code/`**: Contains the implementation of the SL and PMCMC methods functions that are sourced in the report file.
- **`data/`**: Pre-run results that are used in the report file to avoid long knitting times.
- **`exp/`**: Number of few other experiments that I performed on this project.
- **output_log.txt:** a log file to record the parameter estimates for the long code chunks (saved in data).
- **`README.md`**: This file.

## Summary of Results

1. **Accuracy**:
   - SL showed better accuracy for key parameters like α, especially under low noise conditions.
   - PMCMC performed slightly better in estimating noise parameters (η and σ).

2. **Computational Cost**:
   - SL is more computationally efficient than PMCMC, making it suitable for preliminary analyses.
   - PMCMC is more computationally intensive but can offer refined inferences.

3. **Noise Sensitivity**:
   - SL is more robust in low-noise settings.
   - PMCMC is better suited for high-noise scenarios.

## Key Figures and Tables

- **Parameter Estimation**: Neither SL or PMCMC outperforms universally (see Table 1 in the report).
- **Log-likelihood Surface**: Demonstrates the challenges of chaotic inference (Figure 5 and 6).
- **Computation Time**: PMCMC requires significantly more computational resources (Figure 9).

## How to Run

1. Clone the repository.

2. Source the R file from R directory in the report Rmd file (this step is already done if you respect the current file structure).

3. Source the pre-run models from data directory in the Rmd file (this step is already done if you respect the current file structure).

4. Alternatively, set all eval=FALSE in the chunk headers to TRUE (four chunks) to run the models all over again. 
Note that in this case the Rmd file will take hours to generate the pdf but the progress can be tracked using the output_log file)

5. Knit the report rmd file to generate the pdf report with all the results.

## Citation

If you use this project, please cite:
> Pishrobat, Par. "Inference on Non-linear Chaotic Dynamical Systems: Implementation and Evaluation of Woods (2010) and Fasiolo et al. (2016) Methodology." January 27, 2025.

