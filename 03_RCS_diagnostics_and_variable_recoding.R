# ==============================================================================
# Brucellar Spondylitis Dual-Center Retrospective Cohort (N = 141)
# Step 1-5: RCS Nonlinearity Diagnosis, Cutpoint Determination,
#           Segment/Temperature Recoding Decision, CRP-ESR Collinearity
# ==============================================================================

## ---- 0. Setup --------------------------------------------------------------
library(readxl)
library(dplyr)
library(rms)
library(maxstat)
library(car)
library(ggplot2)
library(openxlsx)

set.seed(2026)

dir.create("03_tables",  showWarnings = FALSE)
dir.create("04_figures", showWarnings = FALSE)

dat <- read_excel("02_clean_data/Analysis_Dataset_Imputed.xlsx")


## ==============================================================================
## Step 1: RCS Nonlinearity Diagnosis
## ==============================================================================

dd <- datadist(dat[, c("Delay_to_Dx", "Lesion_Count", "Fever_Baseline",
                        "CRP_Base", "ESR_Base")])
options(datadist = "dd")

fit_delay  <- lrm(End_Status ~ rcs(Delay_to_Dx, 3),    data = dat)
fit_lesion <- lrm(End_Status ~ rcs(Lesion_Count, 4),   data = dat)
fit_fever  <- lrm(End_Status ~ rcs(Fever_Baseline, 4), data = dat)
fit_crp    <- lrm(End_Status ~ rcs(CRP_Base, 4),       data = dat)
fit_esr    <- lrm(End_Status ~ rcs(ESR_Base, 4),       data = dat)

extract_anova <- function(fit, varname, knots) {
  a <- anova(fit)
  data.frame(
    Variable      = varname,
    Knots         = knots,
    Chisq_Overall = a[1, "Chi-Square"],
    df_Overall    = a[1, "d.f."],
    P_Overall     = a[1, "P"],
    Chisq_Nonlin  = a[2, "Chi-Square"],
    df_Nonlin     = a[2, "d.f."],
    P_Nonlinear   = a[2, "P"]
  )
}

rcs_results <- bind_rows(
  extract_anova(fit_delay,  "Delay_to_Dx",    3),
  extract_anova(fit_lesion, "Lesion_Count",   4),
  extract_anova(fit_fever,  "Fever_Baseline", 4),
  extract_anova(fit_crp,    "CRP_Base",       4),
  extract_anova(fit_esr,    "ESR_Base",       4)
)

# Knot-number sensitivity check (3 vs 4 knots) for Lesion_Count and Fever_Baseline
fit_lesion3 <- lrm(End_Status ~ rcs(Lesion_Count, 3),   data = dat)
fit_fever3  <- lrm(End_Status ~ rcs(Fever_Baseline, 3), data = dat)

rcs_sensitivity <- bind_rows(
  extract_anova(fit_lesion3, "Lesion_Count",   3),
  extract_anova(fit_fever3,  "Fever_Baseline", 3)
)

write.xlsx(
  list(RCS_Nonlinearity_Main = rcs_results,
       RCS_Knot_Sensitivity  = rcs_sensitivity),
  "03_tables/Table_S1_RCS_nonlinearity_diagnosis.xlsx"
)

# RCS plots (TIFF, 600 dpi, LZW compression)
save_rcs_plot <- function(fit, varname, xlab, filename) {
  pred <- rms::Predict(fit, name = varname)
  p <- ggplot(pred) +
    labs(x = xlab, y = "log odds (surgical intervention)") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  ggsave(filename, p, width = 5, height = 4, dpi = 600,
         units = "in", device = "tiff", compression = "lzw")
}

save_rcs_plot(fit_delay,  "Delay_to_Dx",    "Delay to diagnosis (months)",
              "04_figures/Fig_RCS_Delay_to_Dx.tiff")
save_rcs_plot(fit_lesion, "Lesion_Count",   "Number of involved spinal segments",
              "04_figures/Fig_RCS_Lesion_Count.tiff")
save_rcs_plot(fit_fever,  "Fever_Baseline", "Baseline body temperature (°C)",
              "04_figures/Fig_RCS_Fever_Baseline.tiff")
save_rcs_plot(fit_crp,    "CRP_Base",       "Baseline CRP (mg/L)",
              "04_figures/Fig_RCS_CRP_Base.tiff")
save_rcs_plot(fit_esr,    "ESR_Base",       "Baseline ESR (mm/h)",
              "04_figures/Fig_RCS_ESR_Base.tiff")


## ==============================================================================
## Step 2: Diagnostic Delay - Cutpoint Determination and Bootstrap Validation
## ==============================================================================

mstat <- maxstat.test(as.factor(End_Status) ~ Delay_to_Dx, data = dat,
                       smethod = "Wilcoxon", pmethod = "exactGauss")

cutpoint_est  <- as.numeric(mstat$estimate)
cutpoint_stat <- as.numeric(mstat$statistic)
cutpoint_p    <- mstat$p.value

n_boot <- 1000
boot_cutpoints <- rep(NA_real_, n_boot)

for (i in seq_len(n_boot)) {
  boot_idx <- sample(seq_len(nrow(dat)), replace = TRUE)
  boot_dat <- dat[boot_idx, ]
  boot_fit <- tryCatch(
    maxstat.test(as.factor(End_Status) ~ Delay_to_Dx, data = boot_dat,
                 smethod = "Wilcoxon", pmethod = "exactGauss"),
    error = function(e) NULL
  )
  if (!is.null(boot_fit)) {
    boot_cutpoints[i] <- as.numeric(boot_fit$estimate)
  }
}

boot_summary <- data.frame(
  N_Valid_Iterations = sum(!is.na(boot_cutpoints)),
  Min     = min(boot_cutpoints, na.rm = TRUE),
  Q1      = quantile(boot_cutpoints, 0.25, na.rm = TRUE),
  Median  = median(boot_cutpoints, na.rm = TRUE),
  Mean    = mean(boot_cutpoints, na.rm = TRUE),
  Q3      = quantile(boot_cutpoints, 0.75, na.rm = TRUE),
  Max     = max(boot_cutpoints, na.rm = TRUE),
  CI_2.5  = quantile(boot_cutpoints, 0.025, na.rm = TRUE),
  CI_97.5 = quantile(boot_cutpoints, 0.975, na.rm = TRUE)
)

cutpoint_summary <- data.frame(
  Method                    = "Maximally selected rank statistics (Wilcoxon)",
  Estimated_Cutpoint_Months = cutpoint_est,
  Test_Statistic_M          = cutpoint_stat,
  P_value                   = cutpoint_p,
  Rounded_Clinical_Cutpoint_Months = 3,
  Bootstrap_Median          = boot_summary$Median,
  Bootstrap_CI_2.5          = boot_summary$CI_2.5,
  Bootstrap_CI_97.5         = boot_summary$CI_97.5
)

write.xlsx(
  list(Cutpoint_Summary               = cutpoint_summary,
       Bootstrap_Distribution_Summary = boot_summary),
  "03_tables/Table_S2_Delay_cutpoint_determination.xlsx"
)

tiff("04_figures/Fig_Bootstrap_Cutpoint_Distribution.tiff",
     width = 5, height = 4, units = "in", res = 600)
hist(boot_cutpoints, breaks = 30,
     main = "", xlab = "Bootstrap-estimated cutpoint (months)",
     col = "grey80", border = "white")
abline(v = cutpoint_est, col = "red", lwd = 2, lty = 2)
dev.off()

# Final coded variable
dat$Delay_cat <- factor(ifelse(dat$Delay_to_Dx >= 3, "≥3 months", "<3 months"),
                         levels = c("<3 months", "≥3 months"))


## ==============================================================================
## Step 3: Number of Involved Spinal Segments - Recoding Decision
## ==============================================================================
# RCS analysis (Step 1) showed no significant overall or nonlinear association
# between Lesion_Count and surgical risk, at both 4 and 3 knots.
# RCS-based recategorization is therefore not supported by the data.

lesion_decision <- data.frame(
  Variable = "Lesion_Count",
  P_Overall_4knots   = rcs_results$P_Overall[rcs_results$Variable == "Lesion_Count"],
  P_Nonlinear_4knots = rcs_results$P_Nonlinear[rcs_results$Variable == "Lesion_Count"],
  P_Overall_3knots   = rcs_sensitivity$P_Overall[rcs_sensitivity$Variable == "Lesion_Count"],
  P_Nonlinear_3knots = rcs_sensitivity$P_Nonlinear[rcs_sensitivity$Variable == "Lesion_Count"],
  Decision = "No significant nonlinearity at either knot specification; RCS-based recategorization not supported. Variable retained without recoding."
)

write.xlsx(lesion_decision, "03_tables/Table_S3_Lesion_Count_decision.xlsx")


## ==============================================================================
## Step 4: Baseline Body Temperature - Recoding Decision and Quadratic Check
## ==============================================================================
# RCS analysis (Step 1) showed no significant overall or nonlinear association
# between Fever_Baseline and surgical risk, at both 4 and 3 knots.
# A quadratic term is fitted as a confirmatory secondary check for a U-shaped
# relationship.

fit_fever_lin  <- glm(End_Status ~ Fever_Baseline, data = dat, family = binomial)
fit_fever_quad <- glm(End_Status ~ Fever_Baseline + I(Fever_Baseline^2),
                       data = dat, family = binomial)

lrt_fever <- anova(fit_fever_lin, fit_fever_quad, test = "LRT")

fever_decision <- data.frame(
  Variable = "Fever_Baseline",
  P_Overall_4knots    = rcs_results$P_Overall[rcs_results$Variable == "Fever_Baseline"],
  P_Nonlinear_4knots  = rcs_results$P_Nonlinear[rcs_results$Variable == "Fever_Baseline"],
  P_Overall_3knots    = rcs_sensitivity$P_Overall[rcs_sensitivity$Variable == "Fever_Baseline"],
  P_Nonlinear_3knots  = rcs_sensitivity$P_Nonlinear[rcs_sensitivity$Variable == "Fever_Baseline"],
  Quadratic_Term_Coef = coef(fit_fever_quad)["I(Fever_Baseline^2)"],
  Quadratic_LRT_P     = lrt_fever$`Pr(>Chi)`[2],
  Decision = "No significant nonlinearity (RCS) and no significant quadratic term; U-shaped association not supported. Variable retained as a simple clinical dichotomy."
)

write.xlsx(fever_decision, "03_tables/Table_S4_Fever_Baseline_decision.xlsx")


## ==============================================================================
## Step 5: CRP / ESR Collinearity Assessment
## ==============================================================================

# 5.1 Distribution check
shapiro_crp <- shapiro.test(dat$CRP_Base)
shapiro_esr <- shapiro.test(dat$ESR_Base)

dat$log_CRP <- log(dat$CRP_Base)
shapiro_log_crp <- shapiro.test(dat$log_CRP)

distribution_check <- data.frame(
  Variable  = c("CRP_Base", "log_CRP_Base", "ESR_Base"),
  Shapiro_W = c(shapiro_crp$statistic, shapiro_log_crp$statistic, shapiro_esr$statistic),
  Shapiro_P = c(shapiro_crp$p.value, shapiro_log_crp$p.value, shapiro_esr$p.value)
)

# 5.2 Extreme-value identification (CRP)
crp_extremes <- dat %>%
  arrange(desc(CRP_Base)) %>%
  select(Study_ID, CRP_Base, ESR_Base, Delay_to_Dx, End_Status) %>%
  slice(1:10)

# 5.3 Primary collinearity test: Variance Inflation Factor (VIF)
vif_model <- glm(End_Status ~ CRP_Base + ESR_Base + Age + Sex,
                  data = dat, family = binomial)
vif_values <- vif(vif_model)

vif_table <- data.frame(
  Variable = names(vif_values),
  VIF      = as.numeric(vif_values)
)

# 5.4 Sensitivity analysis excluding the two clinically confirmed CRP extremes
# (both attributable to documented abscess formation; retained in the primary
# analysis, excluded here only to test robustness)
dat_sens <- dat %>% filter(!(CRP_Base %in% c(1800.0, 760.0)))

pearson_sens <- cor.test(log(dat_sens$CRP_Base), dat_sens$ESR_Base, method = "pearson")

fit_crp_sens <- lrm(End_Status ~ rcs(CRP_Base, 4), data = dat_sens)
a_crp_sens   <- anova(fit_crp_sens)

crp_sensitivity <- data.frame(
  Analysis   = "CRP RCS with the two extreme values excluded",
  N          = nrow(dat_sens),
  P_Overall  = a_crp_sens[1, "P"],
  P_Nonlinear = a_crp_sens[2, "P"],
  Pearson_r_logCRP_ESR = pearson_sens$estimate,
  Pearson_P  = pearson_sens$p.value
)

write.xlsx(
  list(Distribution_Check        = distribution_check,
       CRP_Extreme_Values        = crp_extremes,
       VIF_Assessment            = vif_table,
       Sensitivity_Excl_Extremes = crp_sensitivity),
  "03_tables/Table_S5_CRP_ESR_collinearity.xlsx"
)


## ==============================================================================
## Session Info (reproducibility record)
## ==============================================================================
writeLines(capture.output(sessionInfo()), "03_tables/sessionInfo.txt")


## ==============================================================================
## Step 7: Final Multivariable Model - Table 3 (Univariable + Multivariable)
## ==============================================================================

univar_vars <- c("Delay_cat", "Age", "Lesion_Count", "Fever_status",
                  "Epidural_Abscess", "ESR_Base", "Hospital")

get_univar_or <- function(varname, data) {
  f <- as.formula(paste("End_Status ~", varname))
  m <- glm(f, data = data, family = binomial)
  s <- summary(m)$coefficients
  rows <- rownames(s)[rownames(s) != "(Intercept)"]

  data.frame(
    Variable = rows,
    Coef     = s[rows, "Estimate"],
    SE       = s[rows, "Std. Error"]
  ) %>%
    mutate(
      OR       = exp(Coef),
      CI_lower = exp(Coef - 1.96 * SE),
      CI_upper = exp(Coef + 1.96 * SE),
      P_value  = s[rows, "Pr(>|z|)"]
    ) %>%
    select(Variable, OR, CI_lower, CI_upper, P_value)
}

univar_table <- bind_rows(lapply(univar_vars, get_univar_or, data = dat))

dd <- datadist(dat[, c("Delay_cat", "Age", "Lesion_Count", "Fever_status",
                        "Epidural_Abscess", "ESR_Base", "Hospital")])
options(datadist = "dd")

fit_final <- lrm(End_Status ~ Delay_cat + Age + Lesion_Count + Fever_status +
                    Epidural_Abscess + ESR_Base + Hospital,
                  data = dat, x = TRUE, y = TRUE)

or_table <- data.frame(
  Variable = names(coef(fit_final)),
  Coef     = coef(fit_final),
  SE       = sqrt(diag(vcov(fit_final)))
) %>%
  mutate(
    OR       = exp(Coef),
    CI_lower = exp(Coef - 1.96 * SE),
    CI_upper = exp(Coef + 1.96 * SE),
    Z        = Coef / SE,
    P_value  = 2 * (1 - pnorm(abs(Z)))
  ) %>%
  select(Variable, OR, CI_lower, CI_upper, P_value)

univar_fmt <- univar_table %>%
  mutate(
    Univariable_OR = sprintf("%.2f", OR),
    Univariable_CI = sprintf("%.2f-%.2f", CI_lower, CI_upper),
    Univariable_P  = sprintf("%.3f", P_value)
  ) %>%
  select(Variable, Univariable_OR, Univariable_CI, Univariable_P)

multivar_fmt <- or_table %>%
  filter(Variable != "Intercept") %>%
  mutate(
    Variable = case_when(
      Variable == "Delay_cat=≥3 months" ~ "Delay_cat≥3 months",
      Variable == "Hospital=B"          ~ "HospitalB",
      TRUE ~ Variable
    ),
    Multivariable_OR = sprintf("%.2f", OR),
    Multivariable_CI = sprintf("%.2f-%.2f", CI_lower, CI_upper),
    Multivariable_P  = sprintf("%.3f", P_value)
  ) %>%
  select(Variable, Multivariable_OR, Multivariable_CI, Multivariable_P)

table3_final <- univar_fmt %>%
  left_join(multivar_fmt, by = "Variable")

write.xlsx(table3_final, "03_tables/Table_3_final_univariable_multivariable.xlsx")

# Hospital balance check (SMD) across candidate covariates
smd_vars <- c("Age", "Sex", "Exposure_Hx", "Misdiagnosis_Flow", "Delay_to_Dx",
              "Fever_status", "Lesion_Count", "Epidural_Abscess",
              "Psoas_Abscess", "CRP_Base", "ESR_Base")
smd_factor_vars <- c("Sex", "Exposure_Hx", "Misdiagnosis_Flow",
                      "Fever_status", "Epidural_Abscess", "Psoas_Abscess")

library(tableone)
tab_smd <- CreateTableOne(vars = smd_vars, strata = "Hospital",
                           factorVars = smd_factor_vars, data = dat, test = FALSE)
smd_out <- print(tab_smd, smd = TRUE, printToggle = FALSE)
write.xlsx(as.data.frame(smd_out), "03_tables/Table_S6_Hospital_SMD_balance.xlsx",
           rowNames = TRUE)

# VIF for the final covariate set
vif_final_model <- glm(End_Status ~ Delay_cat + Age + Lesion_Count + Fever_status +
                          Epidural_Abscess + ESR_Base + Hospital,
                        data = dat, family = binomial)
vif_final_table <- data.frame(Variable = names(vif(vif_final_model)),
                               VIF = as.numeric(vif(vif_final_model)))
write.xlsx(vif_final_table, "03_tables/Table_S7_final_model_VIF.xlsx")


## ==============================================================================
## Step 8: Mediation Analysis (Misdiagnosis -> Diagnostic Delay -> Surgery)
## Exploratory analysis. Reported in Supplementary Material only, given the
## limited sample size of the "correct suspicion" reference group (n = 14).
## ==============================================================================

library(mediation)
set.seed(2026)

table(dat$Misdiagnosis_Flow, useNA = "ifany")

## ---- Group coding ----
# Labels below match the standardized English categories written by
# 01_finalize_dataset.R. Run that script first if starting from a fresh copy
# of the raw Excel file.
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

# medsens() requires a probit-link outcome model; refit for the sensitivity
# check only. The ACME/ADE/Total Effect results reported above use the
# logit-link model (model_y_overall), which is the primary analysis.
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
# err.cr.d = value of rho at which the estimated ACME crosses zero
# (Imai, Keele & Yamamoto 2010). A larger |rho| required to null the ACME
# indicates greater robustness to an unmeasured mediator-outcome confounder.
sensitivity_summary <- data.frame(
  Analysis   = c("Overall", "TB vs correct", "Degenerative vs correct"),
  Rho_at_ACME_zero = c(sens_overall$err.cr.d[1], sens_tb$err.cr.d[1], sens_deg$err.cr.d[1])
)

write.xlsx(sensitivity_summary, "03_tables/Table_S9_mediation_sensitivity_rho.xlsx")

## ---- Sensitivity plots ----
# sens.par = "rho" restricts output to a single page (the rho-indexed
# sensitivity curve), matching the rho reported in Table_S9 and avoiding
# the interactive "next plot" prompt that plot.medsens() triggers by
# default when both rho- and R2-indexed panels are requested.
# No compression argument: the cairo backend needed for TIFF LZW
# compression requires XQuartz, which is not assumed to be installed;
# the default quartz backend produces a valid, uncompressed TIFF instead.
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
