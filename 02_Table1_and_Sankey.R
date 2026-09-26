# ==============================================================================
# Brucellar Spondylitis Dual-Center Retrospective Cohort (N = 141)
# Table 1 (Baseline Characteristics), Diagnostic Method Distribution,
# and Sankey Diagram (Initial Diagnosis -> Final Outcome)
#
# Depends on 01_finalize_dataset.R having already been run once (Hospital
# coded A/B, Epidural_Abscess complete, Misdiagnosis_Flow in standardized
# English labels).
# ==============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(gtsummary)
library(flextable)
library(ggplot2)
library(ggalluvial)
library(RColorBrewer)

dir.create("03_tables",  showWarnings = FALSE)
dir.create("04_figures", showWarnings = FALSE)

df <- read_excel("02_clean_data/Analysis_Dataset_Imputed.xlsx")

## ==============================================================================
## Table 1: Baseline and Treatment Characteristics
## ==============================================================================

df_clean <- df %>%
  mutate(
    # Epidural_Abscess is already complete after finalization; the fill below
    # is a no-op safeguard, not a substantive recoding decision.
    Epidural_Abscess = replace_na(as.numeric(Epidural_Abscess), 0),
    Psoas_Abscess     = replace_na(as.numeric(Psoas_Abscess), 0),

    # Surg_Type is structurally missing for the conservative group (no
    # surgery performed); coding this as its own "No Surgery" level is
    # appropriate and does not conflate missing with a real category.
    Surg_Type = replace_na(as.numeric(Surg_Type), 0),

    # Medical_Regimen: these 15 patients did not receive a standard
    # antimicrobial regimen. This is a genuine clinical category (confirmed
    # non-standard treatment), not missing data, and is coded as its own
    # "Non-standard Regimen" level accordingly.
    Medical_Regimen = replace_na(as.numeric(Medical_Regimen), 0),

    End_Status  = factor(End_Status, levels = c(0, 1), labels = c("Conservative", "Surgery")),
    Sex         = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
    Exposure_Hx = factor(Exposure_Hx, levels = c(0, 1), labels = c("Unclear", "Definite")),
    Fever_status = factor(Fever_status, levels = c(0, 1), labels = c("No", "Yes")),
    Neuro_Deficit    = factor(Neuro_Deficit, levels = c(0, 1), labels = c("No", "Yes")),
    Epidural_Abscess = factor(Epidural_Abscess, levels = c(0, 1), labels = c("No", "Yes")),
    Psoas_Abscess    = factor(Psoas_Abscess, levels = c(0, 1), labels = c("No", "Yes")),
    Misdiagnosis_Flow = factor(Misdiagnosis_Flow),

    Surg_Type = factor(Surg_Type,
                        levels = c(0, 1, 2),
                        labels = c("No Surgery",
                                   "Debridement/Decompression Only",
                                   "Internal Fixation and Fusion")),
    Medical_Regimen = factor(Medical_Regimen,
                              levels = c(0, 1, 2, 3),
                              labels = c("Non-standard Regimen",
                                         "Dual Therapy",
                                         "Aminoglycoside-based Regimen",
                                         "Fluoroquinolone-based Regimen"))
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

# Optional Word export
# table1 %>%
#   as_flex_table() %>%
#   save_as_docx(path = "03_tables/Table1_Baseline_Characteristics.docx")


## ==============================================================================
## Diagnostic Method Distribution (Gold Standard)
## ==============================================================================

df_clean <- df_clean %>%
  mutate(
    Gold_Standard = factor(Gold_Standard,
                            levels = c(1, 2, 3),
                            labels = c("Serology Only",
                                       "Positive Blood Culture",
                                       "Positive Biopsy/mNGS"))
  )

gold_dist <- df_clean %>%
  count(Gold_Standard) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

write.xlsx(gold_dist, "03_tables/Table_S10_diagnostic_method_distribution.xlsx")

table_gold <- df_clean %>%
  dplyr::select(Gold_Standard) %>%
  tbl_summary(
    label = list(Gold_Standard ~ "Diagnostic Method"),
    missing_text = "Missing"
  ) %>%
  bold_labels()

table_gold


## ==============================================================================
## Table 2 (part 2): Medical Regimen and Surgical Type, stratified by outcome
## ==============================================================================

table2_treatment <- df_clean %>%
  dplyr::select(Medical_Regimen, Surg_Type, End_Status) %>%
  tbl_summary(
    by = End_Status,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ c(0, 2)),
    label = list(
      Medical_Regimen ~ "Medical Regimen",
      Surg_Type ~ "Surgical Type"
    ),
    missing_text = "Missing"
  ) %>%
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>%
  add_overall() %>%
  bold_labels()

table2_treatment

# Table 2 in the manuscript combines table_gold (diagnostic method, not
# stratified by outcome) with table2_treatment (medical regimen and
# surgical type, stratified by outcome) as two stacked sections; assembled
# manually when formatting the manuscript table.

# Optional Word export
# table2_treatment %>%
#   as_flex_table() %>%
#   save_as_docx(path = "03_tables/Table2_Treatment_Outcomes.docx")


## ==============================================================================
## Sankey Diagram: Initial Diagnosis -> Final Outcome
## Uses the standardized English Misdiagnosis_Flow labels produced by
## 01_finalize_dataset.R.
## ==============================================================================

total_N <- nrow(df_clean %>% filter(!is.na(Misdiagnosis_Flow) & !is.na(End_Status)))

left_stats <- df_clean %>%
  filter(!is.na(Misdiagnosis_Flow) & !is.na(End_Status)) %>%
  count(Misdiagnosis_Flow) %>%
  mutate(
    pct = round(n / total_N * 100, 1),
    new_label = case_when(
      Misdiagnosis_Flow == "Misdiagnosed as suspected spinal tuberculosis" ~
        sprintf("Misdiagnosed as\nSpinal TB\nn = %d (%.1f%%)", n, pct),
      Misdiagnosis_Flow == "Misdiagnosed as lumbar degenerative disease" ~
        sprintf("Misdiagnosed as\nDegenerative Disease\nn = %d (%.1f%%)", n, pct),
      Misdiagnosis_Flow == "Brucellar spondylitis suspected at initial presentation" ~
        sprintf("Suspected Brucellar\nSpondylitis\nn = %d (%.1f%%)", n, pct)
    )
  )

right_stats <- df_clean %>%
  filter(!is.na(Misdiagnosis_Flow) & !is.na(End_Status)) %>%
  count(End_Status) %>%
  mutate(
    pct = round(n / total_N * 100, 1),
    new_label = case_when(
      End_Status == "Conservative" ~ sprintf("Conservative\nn = %d (%.1f%%)", n, pct),
      End_Status == "Surgery"      ~ sprintf("Surgery\nn = %d (%.1f%%)", n, pct)
    )
  )

sankey_data <- df_clean %>%
  filter(!is.na(Misdiagnosis_Flow) & !is.na(End_Status)) %>%
  left_join(left_stats %>% dplyr::select(Misdiagnosis_Flow, left_label = new_label), by = "Misdiagnosis_Flow") %>%
  left_join(right_stats %>% dplyr::select(End_Status, right_label = new_label), by = "End_Status") %>%
  mutate(
    left_label  = factor(left_label, levels = left_stats$new_label),
    right_label = factor(right_label, levels = right_stats$new_label)
  ) %>%
  group_by(left_label, right_label) %>%
  summarise(Count = n(), .groups = "drop")

sankey_plot <- ggplot(sankey_data,
                       aes(y = Count, axis1 = left_label, axis2 = right_label)) +
  geom_alluvium(aes(fill = left_label), width = 1/3, alpha = 0.7, curve_type = "cubic") +
  geom_stratum(width = 1/3, fill = "#F0F0F0", color = "grey30", linewidth = 0.5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.5, fontface = "bold", color = "black", lineheight = 0.85) +
  scale_x_discrete(limits = c("Initial Diagnosis", "Final Outcome"), expand = c(0.1, 0.1)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40")
  ) +
  labs(
    title = "Patient Flow: From Initial Diagnosis to Clinical Outcome",
    subtitle = "Tracing the impact of initial misdiagnosis on the necessity of surgical intervention"
  )

ggsave("04_figures/Fig2_Sankey_Diagnosis_to_Outcome.pdf", sankey_plot,
       width = 10, height = 7, units = "in", dpi = 300, device = "pdf")

ggsave("04_figures/Fig2_Sankey_Diagnosis_to_Outcome.tiff", sankey_plot,
       width = 10, height = 7, units = "in", dpi = 600, device = "tiff", compression = "lzw")

sankey_plot
