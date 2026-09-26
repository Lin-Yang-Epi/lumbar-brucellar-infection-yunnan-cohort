# ==============================================================================
# Dataset Finalization
# Brucellar Spondylitis Dual-Center Retrospective Cohort (N = 141)
#
# Standardizes hospital labels, completes structurally-missing values, and
# assigns unambiguous English category labels for Misdiagnosis_Flow.
# Overwrites the analytic dataset in place; a backup of the pre-finalization
# file is kept in 02_clean_data/archive/.
# ==============================================================================

library(readxl)
library(openxlsx)
library(dplyr)

raw_path <- "02_clean_data/Analysis_Dataset_Imputed.xlsx"

dir.create("02_clean_data/archive", showWarnings = FALSE)
file.copy(raw_path,
          paste0("02_clean_data/archive/Analysis_Dataset_Imputed_backup_",
                 format(Sys.Date(), "%Y%m%d"), ".xlsx"),
          overwrite = FALSE)

dat <- read_excel(raw_path)

## ---- Hospital: rename Region -> Hospital, recode to A / B ----
# Baoshan (n = 24) -> Hospital A; Zhaotong (n = 117) -> Hospital B
# Guarded so the script can be re-run safely against a file that has
# already been through this finalization step (i.e. already has a
# Hospital column instead of Region).
if ("Region" %in% names(dat)) {
  dat <- dat %>% rename(Hospital = Region)
}
if (any(dat$Hospital %in% c("Baoshan", "Zhaotong"))) {
  dat <- dat %>%
    mutate(Hospital = case_when(
      Hospital == "Baoshan"  ~ "A",
      Hospital == "Zhaotong" ~ "B",
      TRUE ~ Hospital
    ))
}

## ---- Epidural abscess: complete structurally-missing values ----
# Blank entries reflect a confirmed absence of epidural abscess on imaging,
# not an unknown status, and are recoded to 0.
dat$Epidural_Abscess[is.na(dat$Epidural_Abscess)] <- 0

## ---- Neurological deficit: complete one blank entry ----
# One blank entry was checked against the source medical record: the patient
# had no neurological deficit, so it is recoded to 0. The check stops the
# script if more than one blank is found.
stopifnot(sum(is.na(dat$Neuro_Deficit)) <= 1)
dat$Neuro_Deficit[is.na(dat$Neuro_Deficit)] <- 0

## ---- Initial misdiagnosis category: standardized English labels ----
# The third category is relabeled from a generic "spinal infection suspected"
# description (ambiguous with tuberculous or pyogenic spondylitis) to state
# explicitly that brucellar spondylitis itself was correctly suspected at
# first presentation.
dat <- dat %>%
  mutate(Misdiagnosis_Flow = case_when(
    Misdiagnosis_Flow == "误诊为疑似脊柱结核"   ~ "Misdiagnosed as suspected spinal tuberculosis",
    Misdiagnosis_Flow == "误诊为腰椎退行性病变" ~ "Misdiagnosed as lumbar degenerative disease",
    Misdiagnosis_Flow == "首诊疑诊脊柱感染"     ~ "Brucellar spondylitis suspected at initial presentation",
    TRUE ~ Misdiagnosis_Flow
  ))

## ---- Verification ----
table(dat$Hospital, useNA = "ifany")
table(dat$Epidural_Abscess, useNA = "ifany")
table(dat$Neuro_Deficit, useNA = "ifany")
table(dat$Misdiagnosis_Flow, useNA = "ifany")

## ---- Overwrite the analytic dataset ----
write.xlsx(dat, raw_path, overwrite = TRUE)
