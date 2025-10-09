# Metamodelling of Carbon Fluxes from Crop and Grassland Multi-Model Outputs

**Authors:** Roland Hollós, N. Zrinyi, Z. Barcza, G. Bellocchi, R. Sándor, J. Ruff, N. Fodor  
**Affiliations:** Centre for Agricultural Research (HUN-REN), Eötvös Loránd University, Czech Academy of Sciences, INRAE, University of Pécs  

---

## Overview

This repository contains code and data processing workflows used for the study:

> **"Meta-modelling of carbon fluxes from crop and grassland multi-model outputs"**

The project evaluates and compares **stacking-based meta-modelling techniques** — *Multiple Linear Regression (MLR)*, *Random Forest (RF)*, *XGBoost (XGB)*, and *XGBoost with environmental covariates (XGB+)* — to traditional ensemble approaches like the **Multi-Model Median (MMM)**.  
These models predict three key **carbon flux components** across crop and grassland systems:
- **GPP** – Gross Primary Production  
- **RECO** – Ecosystem Respiration  
- **NEE** – Net Ecosystem Exchange  

The meta-models were tested at multiple long-term field sites, showing improved predictive accuracy and interpretability compared to both individual process-based models and MMM ensembles.

---

## Key Concepts

- **Stacking Ensemble Learning:** Combines predictions from multiple process-based models to train a higher-level “meta-model”.
- **Environmental Covariates:** Incorporation of temperature and precipitation data in XGB+ improves context-specific predictions.
- **SHAP Analysis:** Used for model interpretability — quantifying feature contributions and identifying dominant environmental drivers.
- **Performance Metrics:** RMSE, Bias, and Pearson’s correlation coefficient (*r*) are used for evaluation.

---

## Methodology

### Meta-Models Implemented
| Meta-Model | Description |
|-------------|-------------|
| **MLR** | Linear regression applied to ensemble model outputs |
| **RF** | Random Forest with 1000 trees (using `randomForest` in R) |
| **XGB** | XGBoost meta-model using grid search hyperparameter optimization |
| **XGB+** | Extended XGBoost with temperature and precipitation covariates |

### Training & Validation
- **Train/Test Split:** 70% training, 30% validation  
- **Input Data:** Daily outputs from 23 crop and grassland models (GPP, RECO, NEE)  
- **Sites:** Two croplands (Canada, France) and two grasslands (France, UK)  
- **Evaluation Metrics:** RMSE, Bias, and correlation coefficient (r)

### Tools and Libraries
- R version ≥ 4.2  
- Packages:
  - `randomForest`
  - `xgboost`
  - `SHAPforxgboost`
  - `tidyverse`
  - `caret`

---

## Results Summary

- Meta-models improved explained variance by **10–38.5%** over the MMM.
- **RF, XGB, and XGB+** provided the best performance across fluxes and sites.
- **Bias was nearly eliminated** in all but one cropland site.
- **Temperature** was the dominant driver, while **precipitation** had minor influence.

## Data Availability


> [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5TO4HE](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5TO4HE)


##  Contact

**Corresponding author:**  
 *fodor.nandor@atk.hun-ren.hu*  

For code-related inquiries:  
 *hollos.roland@atk.hun-ren.hu*
