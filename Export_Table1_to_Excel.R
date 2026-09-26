# ==============================================================================
# Brucellar Spondylitis Dual-Center Retrospective Cohort (N = 141)
# Table 1: Baseline Clinical Characteristics - Excel Export
#
# Fully standalone: reads the finalized dataset directly and rebuilds Table 1
# on its own. Depends only on 01_finalize_dataset.R having already been run
# once.
# ==============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(gtsummary)
library(openxlsx)

dir.create("03_tables", showWarnings = FALSE)

df <- read_excel("02_clean_data/Analysis_Dataset_Imputed.xlsx")

df_clean <- df %>%
  mutate(
    Epidural_Abscess = replace_na(as.numeric(Epidural_Abscess), 0),
    Psoas_Abscess     = replace_na(as.numeric(Psoas_Abscess), 0),

    End_Status  = factor(End_Status, levels = c(0, 1), labels = c("Conservative", "Surgery")),
    Sex         = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Exposure_Hx = factor(Exposure_Hx, levels = c(0, 1), labels = c("Unclear", "Definite")),
    Fever_status = factor(Fever_status, levels = c(0, 1), labels = c("No", "Yes")),
    Neuro_Deficit    = factor(Neuro_Deficit, levels = c(0, 1), labels = c("No", "Yes")),
    Epidural_Abscess = factor(Epidural_Abscess, levels = c(0, 1), labels = c("No", "Yes")),
    Psoas_Abscess    = factor(Psoas_Abscess, levels = c(0, 1), labels = c("No", "Yes")),
    Misdiagnosis_Flow = factor(Misdiagnosis_Flow)
  )

table1 <- df_clean %>%
  dplyr::select(Age, Sex, Exposure_Hx, Misdiagnosis_Flow, Delay_to_Dx, Fever_Baseline,
         Fever_status, CRP_Base, ESR_Base, Lesion_Count, Neuro_Deficit, Epidural_Abscess,
         Psoas_Abscess, End_Status) %>%
  tbl_summary(
    by = End_Status,
    type = list(
      Fever_status      ~ "categorical",
      Neuro_Deficit     ~ "categorical",
      Epidural_Abscess  ~ "categorical",
      Psoas_Abscess     ~ "categorical"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_categorical() ~ c(0, 2)),
    label = list(
      Age ~ "Age (years)",
      Sex ~ "Sex",
      Exposure_Hx ~ "Exposure History",
      Misdiagnosis_Flow ~ "Initial Diagnosis / Delay Reason",
      Delay_to_Dx ~ "Delay to Diagnosis (months)",
      Fever_Baseline ~ "Temperature (°C)",
      Fever_status ~ "Fever Status",
      CRP_Base ~ "CRP (mg/L)",
      ESR_Base ~ "ESR (mm/h)",
      Lesion_Count ~ "Number of Involved Spinal Segments",
      Neuro_Deficit ~ "Neurological Deficit",
      Epidural_Abscess ~ "Epidural Abscess",
      Psoas_Abscess ~ "Psoas Abscess"
    ),
    missing_text = "Missing"
  ) %>%
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>%
  add_overall() %>%
  bold_labels()

table1

## ---- Export to Excel ----
table1_df <- as_tibble(table1, col_labels = TRUE)

write.xlsx(table1_df, "03_tables/Table_1_baseline_characteristics.xlsx")
