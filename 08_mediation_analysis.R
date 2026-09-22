# ==============================================================================
# Brucellar Spondylitis Dual-Center Retrospective Cohort (N = 141)
# Mediation Analysis (Misdiagnosis -> Diagnostic Delay -> Surgery)
#
# Fully standalone: does not require any objects from other scripts in this
# repository. Reads the finalized dataset directly and rebuilds everything
# it needs. Depends only on 01_finalize_dataset.R having already been run
# once (Hospital coded A/B, Epidural_Abscess complete, Misdiagnosis_Flow in
# standardized English labels).
#
# Exploratory analysis. Reported in Supplementary Material only, given the
# limited sample size of the "correct suspicion" reference group (n = 14).
# ==============================================================================

library(readxl)
library(dplyr)
library(openxlsx)
library(mediation)

set.seed(2026)

dir.create("03_tables",  showWarnings = FALSE)
dir.create("04_figures", showWarnings = FALSE)

dat <- read_excel("02_clean_data/Analysis_Dataset_Imputed.xlsx")

table(dat$Misdiagnosis_Flow, useNA = "ifany")

## ---- Group coding ----
CORRECT_LABEL <- "Brucellar spondylitis suspected at initial presentation"
TB_LABEL      <- "Misdiagnosed as suspected spinal tuberculosis"
DEG_LABEL     <- "Misdiagnosed as lumbar degenerative disease"

dat$Misdx_binary <- factor(
  ifelse(dat$Misdiagnosis_Flow == CORRECT_LABEL, "Correct_suspicion", "Misdiagnosed"),
  levels = c("Correct_suspicion", "Misdiagnosed")
)

dat_tb_vs_correct <- dat %>%
  filter(Misdiagnosis_Flow %in% c(TB_LABEL, CORRECT_LABEL)) %>%
  mutate(Misdx_tb = factor(ifelse(Misdiagnosis_Flow == CORRECT_LABEL,
                                    "Correct_suspicion", "TB_misdiagnosis"),
                            levels = c("Correct_suspicion", "TB_misdiagnosis")))

dat_deg_vs_correct <- dat %>%
  filter(Misdiagnosis_Flow %in% c(DEG_LABEL, CORRECT_LABEL)) %>%
  mutate(Misdx_deg = factor(ifelse(Misdiagnosis_Flow == CORRECT_LABEL,
                                     "Correct_suspicion", "Degenerative_misdiagnosis"),
                             levels = c("Correct_suspicion", "Degenerative_misdiagnosis")))

## ---- 8.1 Overall: Misdiagnosed (TB or degenerative) vs. correct suspicion ----
model_m_overall <- lm(Delay_to_Dx ~ Misdx_binary + Age + Lesion_Count + Fever_status +
                         Epidural_Abscess + ESR_Base + Hospital, data = dat)

model_y_overall <- glm(End_Status ~ Misdx_binary + Delay_to_Dx + Age + Lesion_Count +
                          Fever_status + Epidural_Abscess + ESR_Base + Hospital,
                        data = dat, family = binomial)

med_overall <- mediate(model_m_overall, model_y_overall,
                        treat = "Misdx_binary", mediator = "Delay_to_Dx",
                        boot = TRUE, sims = 2000)

model_y_overall_probit <- glm(End_Status ~ Misdx_binary + Delay_to_Dx + Age +
                                 Lesion_Count + Fever_status + Epidural_Abscess +
                                 ESR_Base + Hospital,
                               data = dat, family = binomial(link = "probit"))
med_overall_probit <- mediate(model_m_overall, model_y_overall_probit,
                               treat = "Misdx_binary", mediator = "Delay_to_Dx",
                               boot = TRUE, sims = 2000)
sens_overall <- medsens(med_overall_probit, rho.by = 0.05, effect.type = "indirect")

## ---- 8.2 TB misdiagnosis vs. correct suspicion ----
model_m_tb <- lm(Delay_to_Dx ~ Misdx_tb + Age + Lesion_Count + Fever_status +
                    Epidural_Abscess + ESR_Base + Hospital, data = dat_tb_vs_correct)

model_y_tb <- glm(End_Status ~ Misdx_tb + Delay_to_Dx + Age + Lesion_Count +
                     Fever_status + Epidural_Abscess + ESR_Base + Hospital,
                   data = dat_tb_vs_correct, family = binomial)

med_tb <- mediate(model_m_tb, model_y_tb,
                   treat = "Misdx_tb", mediator = "Delay_to_Dx",
                   boot = TRUE, sims = 2000)

model_y_tb_probit <- glm(End_Status ~ Misdx_tb + Delay_to_Dx + Age + Lesion_Count +
                            Fever_status + Epidural_Abscess + ESR_Base + Hospital,
                          data = dat_tb_vs_correct, family = binomial(link = "probit"))
med_tb_probit <- mediate(model_m_tb, model_y_tb_probit,
                          treat = "Misdx_tb", mediator = "Delay_to_Dx",
                          boot = TRUE, sims = 2000)
sens_tb <- medsens(med_tb_probit, rho.by = 0.05, effect.type = "indirect")

## ---- 8.3 Degenerative-disease misdiagnosis vs. correct suspicion ----
model_m_deg <- lm(Delay_to_Dx ~ Misdx_deg + Age + Lesion_Count + Fever_status +
                     Epidural_Abscess + ESR_Base + Hospital, data = dat_deg_vs_correct)

model_y_deg <- glm(End_Status ~ Misdx_deg + Delay_to_Dx + Age + Lesion_Count +
                      Fever_status + Epidural_Abscess + ESR_Base + Hospital,
                    data = dat_deg_vs_correct, family = binomial)

med_deg <- mediate(model_m_deg, model_y_deg,
                    treat = "Misdx_deg", mediator = "Delay_to_Dx",
                    boot = TRUE, sims = 2000)

model_y_deg_probit <- glm(End_Status ~ Misdx_deg + Delay_to_Dx + Age + Lesion_Count +
                             Fever_status + Epidural_Abscess + ESR_Base + Hospital,
                           data = dat_deg_vs_correct, family = binomial(link = "probit"))
med_deg_probit <- mediate(model_m_deg, model_y_deg_probit,
                           treat = "Misdx_deg", mediator = "Delay_to_Dx",
                           boot = TRUE, sims = 2000)
sens_deg <- medsens(med_deg_probit, rho.by = 0.05, effect.type = "indirect")

## ---- Summary table ----
extract_mediate <- function(med_obj, label, n) {
  data.frame(
    Analysis        = label,
    N               = n,
    ACME_est        = med_obj$d.avg,
    ACME_CI_lower   = med_obj$d.avg.ci[1],
    ACME_CI_upper   = med_obj$d.avg.ci[2],
    ACME_P          = med_obj$d.avg.p,
    ADE_est         = med_obj$z.avg,
    ADE_CI_lower    = med_obj$z.avg.ci[1],
    ADE_CI_upper    = med_obj$z.avg.ci[2],
    ADE_P           = med_obj$z.avg.p,
    Total_est       = med_obj$tau.coef,
    Total_CI_lower  = med_obj$tau.ci[1],
    Total_CI_upper  = med_obj$tau.ci[2],
    Total_P         = med_obj$tau.p,
    Prop_Mediated   = med_obj$n.avg,
    Prop_CI_lower   = med_obj$n.avg.ci[1],
    Prop_CI_upper   = med_obj$n.avg.ci[2],
    Prop_P          = med_obj$n.avg.p
  )
}

mediation_summary <- bind_rows(
  extract_mediate(med_overall, "Misdiagnosed (TB or degenerative) vs. correct suspicion", 141),
  extract_mediate(med_tb,      "TB misdiagnosis vs. correct suspicion", 91),
  extract_mediate(med_deg,     "Degenerative misdiagnosis vs. correct suspicion", 64)
)

write.xlsx(mediation_summary, "03_tables/Table_S8_mediation_analysis_summary.xlsx")

## ---- Sensitivity analysis (unmeasured confounding, rho at ACME = 0) ----
sensitivity_summary <- data.frame(
  Analysis   = c("Overall", "TB vs correct", "Degenerative vs correct"),
  Rho_at_ACME_zero = c(sens_overall$err.cr.d[1], sens_tb$err.cr.d[1], sens_deg$err.cr.d[1])
)

write.xlsx(sensitivity_summary, "03_tables/Table_S9_mediation_sensitivity_rho.xlsx")

## ---- Sensitivity plots ----
# devAskNewPage(ask = FALSE) is the definitive fix: plot.medsens() can
# attempt to draw more than one page (e.g. separate panels for the control
# and treatment conditions) even when sens.par = "rho" is specified. Without
# this line, R pauses for an interactive keypress between pages, which
# hangs a non-interactive save-to-file block and can time out IDE plot
# viewers. With ask = FALSE, all pages render without pausing; only the
# last page drawn is retained in a single-frame TIFF file, which is
# sufficient here since the control- and treatment-condition sensitivity
# curves are essentially identical in a model without a treatment-mediator
# interaction.
devAskNewPage(ask = FALSE)

tiff("04_figures/Fig_Medsens_Overall.tiff", width = 5, height = 4, units = "in",
     res = 600)
plot(sens_overall, sens.par = "rho")
dev.off()

tiff("04_figures/Fig_Medsens_TB.tiff", width = 5, height = 4, units = "in",
     res = 600)
plot(sens_tb, sens.par = "rho")
dev.off()

tiff("04_figures/Fig_Medsens_Degenerative.tiff", width = 5, height = 4, units = "in",
     res = 600)
plot(sens_deg, sens.par = "rho")
dev.off()

devAskNewPage(ask = TRUE)
