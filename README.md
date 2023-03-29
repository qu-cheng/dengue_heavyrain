# dengue_heavyrain

This repository contains data and code for reproducing the analyses in Prior water availability modifies the effect of heavy rainfall on dengue transmission: a time series analysis of passive surveillance data from Guangzhou, China

## Data
This foloder includes only one file *DF_case.csv*, which has 6 columns:
* **OnsetDate**, the date for the onset of symptoms. 
* **CaseCount**, number of cases with symptoms onset on **OnsetDate**
* **Precipitation**, daily total rainfall (mm)
* **Tave**, daily mean temperature (°C)
* **Tmax**, daily maximum temperature (°C)
* **Tmin**, daily minimum temperature (°C)

## Code
This folder contains code for the analyses and figures:
* **0_packages_data_functions.R**: R script to load packages, preprocess data, generate cross-basis functions, and fit the model
* **1_visualize_data.R**: R script to visualize the climate and disease data for Figs 2, 3, and S1-3
* **2_run_model.R: R** script to fit the main models and save the model outputs
* **3_plot_results.R**: R script to visualize the results of the main model for Figs 4, 5, and S4
* **4_Sensitivity_analyses.R**: R script to conduct sensitivity analyses
* **5_plot_sensitivity.R**: R script to visualize the results of the models in the sensitivity analyses for Figs S5-10
