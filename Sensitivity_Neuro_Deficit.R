# ==============================================================================
# Brucellar Spondylitis Two-Center Retrospective Cohort (N = 141)
# Sensitivity analysis: final multivariable model additionally adjusted for
# neurological deficit (a documented surgical indication).
#
# Standalone: reads the finalized dataset directly. Depends only on
# 01_finalize_dataset.R having been run (Neuro_Deficit complete, n = 141).
# Wald 95% CIs are used, matching Table 3.
# ==============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

dir.create("03_tables", showWarnings = FALSE)

dat <- read_excel("02_clean_data/Analysis_Dataset_Imputed.xlsx")

dat$Delay_cat <- factor(ifelse(dat$Delay_to_Dx >= 3, "≥3 months", "<3 months"),
                        levels = c("<3 months", "≥3 months"))

m_neuro <- glm(End_Status ~ Delay_cat + Age + Lesion_Count + Fever_status +
                 Epidural_Abscess + ESR_Base + Hospital + Neuro_Deficit,
               data = dat, family = binomial)

nobs(m_neuro)   # should be 141

s <- summary(m_neuro)$coefficients
s <- s[rownames(s) != "(Intercept)", , drop = FALSE]

table_neuro <- data.frame(
  Variable = rownames(s),
  OR       = exp(s[, "Estimate"]),
  CI_lower = exp(s[, "Estimate"] - 1.96 * s[, "Std. Error"]),
  CI_upper = exp(s[, "Estimate"] + 1.96 * s[, "Std. Error"]),
  P_value  = s[, "Pr(>|z|)"],
  row.names = NULL
) %>%
  mutate(
    Variable = case_when(
      Variable == "Delay_cat≥3 months" ~ "Diagnostic delay ≥3 months (ref: <3 months)",
      Variable == "Age"                ~ "Age (years)",
      Variable == "Lesion_Count"       ~ "Number of involved spinal segments",
      Variable == "Fever_status"       ~ "Fever",
      Variable == "Epidural_Abscess"   ~ "Epidural abscess",
      Variable == "ESR_Base"           ~ "ESR (mm/h)",
      Variable == "HospitalB"          ~ "Center B (ref: Center A)",
      Variable == "Neuro_Deficit"      ~ "Neurological deficit",
      TRUE ~ Variable
    ),
    `Adjusted OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, CI_lower, CI_upper),
    `P value` = ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))
  ) %>%
  select(Variable, `Adjusted OR (95% CI)`, `P value`)

table_neuro

write.xlsx(table_neuro, "03_tables/Table_S_Neuro_Deficit_sensitivity.xlsx")
