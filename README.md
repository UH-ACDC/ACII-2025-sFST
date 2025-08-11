# Facial Self-Touches in Work

This repository contains the R scripts used to curate data, perform quality control, and generate the final models for the paper **"Facial Self-Touches Are Associated With Mental Stress in Knowledge Work,"** published in *ACII 2025*.

The goal of this repository is to ensure the research is transparent and reproducible, allowing others to build upon these findings.

---

## 🚀 Getting Started

Follow these steps to set up your environment and run the analysis.

### Prerequisites

To run the scripts, you will need the following software installed:

* **R** (version 4.2 or newer is recommended)
* **RStudio** (Recommended as a user-friendly interface for R)

### Installing R Packages

All required R packages are available on the Comprehensive R Archive Network (CRAN). You can install them all by running the following command in your R console:


Packages are available on CRAN and can be installed using a simple call to 
`install.packages()`:

    install.packages('PackageName')
	
	
## Scripts & Analysis Workflow
##### Please run the following scripts sequentially
The analysis is divided into three main scripts. For the results to be generated correctly, please run the following scripts in sequential order.

- `1-descriptive.rmd` This R Markdown notebook performs an initial analysis and generates descriptive plots for the key model predictors.
- `2-self-touches_EDA.R` This R script curates the raw facial self-touch data and performs an exploratory data analysis (EDA), including key visualizations.
- `3-Models-visualizations.rmd` This final R Markdown notebook runs the multiple linear regression models and produces the statistical outputs and visualizations presented in the paper.
 
