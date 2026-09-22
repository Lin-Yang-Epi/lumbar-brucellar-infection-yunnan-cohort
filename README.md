# Lumbar Brucellar Infection: Dual-Center Retrospective Cohort Analysis

Reproducible R analysis pipeline for a dual-center retrospective cohort study examining diagnostic delay and surgical intervention in patients with lumbar brucellar infection, conducted at two tertiary hospitals in Yunnan Province, China.

This repository accompanies a manuscript submitted to the *Journal of Evaluation in Clinical Practice*.

## Data Availability

Due to patient privacy protections and the terms of the institutional ethics approvals governing this study, the underlying clinical dataset is not publicly available. Data supporting the findings of this study may be requested from the corresponding author, subject to institutional approval. This repository contains the analysis code only.

## Repository Structure

Scripts are numbered in the order they should be run. Each script assumes the analytic dataset is located at `02_clean_data/Analysis_Dataset_Imputed.xlsx` relative to the project root.

| Script | Description |
|---|---|
| `01_finalize_dataset.R` | Standardizes hospital labels, completes structurally-missing values, and assigns standardized category labels. Overwrites the analytic dataset in place (a backup of the prior version is kept automatically). |
| `02_Table1_and_Sankey.R` | Generates the baseline characteristics table, diagnostic method distribution, and the Sankey diagram of initial diagnosis to clinical outcome. |
| `03_RCS_diagnostics_and_variable_recoding.R` | Restricted cubic spline (RCS) nonlinearity diagnosis for five continuous variables; cutpoint determination and bootstrap validation for diagnostic delay; recoding decisions for number of involved spinal segments and baseline temperature; CRP/ESR collinearity assessment; final multivariable logistic regression model. |
| `08_mediation_analysis.R` | Exploratory causal mediation analysis (initial misdiagnosis → diagnostic delay → surgical intervention), with sensitivity analysis for unmeasured confounding. Fully standalone — does not depend on objects from other scripts. |

## Methods Summary

- **Nonlinearity diagnosis**: Restricted cubic splines (`rms` package), with overall and nonlinear associations tested using Wald tests.
- **Cutpoint determination**: Maximally selected rank statistics (`maxstat` package), validated with 1,000 bootstrap resamples.
- **Covariate selection**: Events-per-variable (EPV) principle and prespecified clinical relevance; collinearity assessed via variance inflation factors (`car` package).
- **Center effect**: Standardized mean differences (`tableone` package) to assess baseline balance between study centers; study center included as a covariate in the final model.
- **Mediation analysis**: Causal mediation analysis based on the counterfactual framework (`mediation` package), with sensitivity analysis for unmeasured confounding.

## Software

Analyses were performed in R (version 4.4.1). Key packages: `rms`, `maxstat`, `car`, `mediation`, `tableone`, `gtsummary`, `ggplot2`, `ggalluvial`, `openxlsx`.

## Citation

If you use this code, please cite the associated manuscript (citation to be added upon publication).
