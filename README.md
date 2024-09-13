# GARCH and tGARCH Models: Impact of Sample Size on Value-at-Risk Estimation

## Project Overview

This project explores the Generalized Autoregressive Conditional Heteroskedasticity (GARCH) and tGARCH models and their effectiveness in financial risk assessment, particularly focusing on the Value-at-Risk (VaR). The goal of this project is to assess the impact of sample size on the precision of these models when used to estimate VaR in financial markets.

## Key Features:
GARCH(1,1) and tGARCH models applied to financial return series.
Monte Carlo simulations used to predict volatility and risk.
Comprehensive analysis of how sample size affects model accuracy.
Visualization of the learning curves and Mean Squared Error (MSE) to illustrate model performance.
Practical insights into the minimum sample size required for reliable risk predictions.

## Files in the Repository

Project_21730.Rmd: The main R Markdown file containing the code and analysis. \
TSLA.csv: Historical Daily returns for Tesla CSV Dataset used for the analysis.  

## Methodology

Data Collection: Historical daily returns for Tesla (TSLA) stock were used for this analysis.
Modeling:
GARCH(1,1) and tGARCH models were implemented using the rugarch R package.
The models were fitted on the Tesla returns data, estimating key parameters such as ω, α, and β for GARCH, and the degrees of freedom (ν) for tGARCH.
Monte Carlo Simulations:
Conducted for multiple sample sizes (T1 = 250, T2 = 1000, T3 = 2000, T4 = 5000).
1000 simulations were run for each sample size to generate synthetic data and simulate portfolio returns.
Value-at-Risk Estimation:
VaR was calculated for both GARCH and tGARCH models.
The MSE was analyzed to quantify how sample size impacts the precision of VaR estimates.
Visualization:
Scatter plots and learning curves were used to illustrate model accuracy and error reduction with increased sample sizes.
## Results

Sample Size Impact: As expected, increasing sample size improves the precision of both GARCH and tGARCH models, reducing estimation errors.
tGARCH Model Performance: The tGARCH model with Student-t distributed residuals better captured the fat-tailed nature of financial returns but required larger sample sizes to achieve comparable precision to GARCH.
VaR Estimation: GARCH tended to underestimate VaR for small sample sizes, while tGARCH provided a more even distribution of risk estimates.

## Required R packages:
rugarch
ggplot2
MonteCarlo
tidyverse


This project provides valuable insights into how sample size affects the precision of risk predictions using GARCH and tGARCH models. By optimizing sample sizes and using appropriate models for non-normal financial data, more accurate risk assessments can be achieved.
