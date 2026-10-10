# ==============================================================================
# Brucellar Spondylitis Dual-Center Retrospective Cohort (N = 141)
# Step 2: Imputation of missing baseline CRP and ESR
#
# Method: iterative imputation by chained equations with random forest
# regressors (scikit-learn IterativeImputer + RandomForestRegressor),
# max_iter = 30, tol = 1e-3, random seed 42. A single completed dataset is
# produced. Only CRP_Base (n = 10 missing) and ESR_Base (n = 9 missing) are
# written back; the other columns are used as predictors only.
# Imputed CRP is rounded to 1 decimal and ESR to a whole number, as in the
# original run; this rounding is applied to the whole column.
#
# Input : 02_clean_data/Analysis_Dataset_Included.xlsx   (141 included patients)
# Output: 02_clean_data/Analysis_Dataset_Imputed_python_output.xlsx
#
# The output is written to a new file so the analysis dataset
# (Analysis_Dataset_Imputed.xlsx) is never overwritten. After imputation,
# the analysis dataset received manual corrections checked against medical
# records (number of involved segments in 2 patients, baseline temperature in
# 1 patient) and the label standardization in 01_finalize_dataset.R.
# The final block compares the CRP/ESR produced here with the analysis dataset.
#
# Run from the project root:  python 02_imputation.py
# Original run: Python 3.13.15; record the scikit-learn version below.
# ==============================================================================

import os
import sys

import numpy as np
import pandas as pd
import sklearn
from sklearn.experimental import enable_iterative_imputer  # noqa: F401
from sklearn.impute import IterativeImputer
from sklearn.ensemble import RandomForestRegressor

IN_PATH = os.path.join("02_clean_data", "Analysis_Dataset_Included.xlsx")
OUT_PATH = os.path.join("02_clean_data", "Analysis_Dataset_Imputed_python_output.xlsx")
ANALYSIS_PATH = os.path.join("02_clean_data", "Analysis_Dataset_Imputed.xlsx")

SEED = 42
IMPUTE_COLS = ["Age", "Fever_Baseline", "CRP_Base", "ESR_Base",
               "Delay_to_Dx", "Lesion_Count", "Epidural_Abscess"]

print(f"Python {sys.version.split()[0]} | scikit-learn {sklearn.__version__} | pandas {pd.__version__}")

# ---- 1. Read data ----------------------------------------------------------------
df = pd.read_excel(IN_PATH)
missing_cols = [c for c in IMPUTE_COLS if c not in df.columns]
if missing_cols:
    sys.exit(f"Columns not found in {IN_PATH}: {missing_cols}")

# Make sure these columns are numeric (non-numeric entries become missing)
for col in ["CRP_Base", "ESR_Base", "Delay_to_Dx"]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

print(f"\nPatients: {len(df)}")
print("Missing values before imputation:")
print(df[IMPUTE_COLS].isna().sum().to_string())

# ---- 2. Imputation -------------------------------------------------------------------
df_to_impute = df[IMPUTE_COLS].copy()

imputer = IterativeImputer(
    estimator=RandomForestRegressor(random_state=SEED),
    max_iter=30,
    tol=1e-3,
    random_state=SEED,
)
imputed_values = imputer.fit_transform(df_to_impute)
df_imputed = pd.DataFrame(imputed_values, columns=IMPUTE_COLS, index=df.index)
print(f"\nIterations run: {imputer.n_iter_}")

# ---- 3. Write back CRP and ESR with plausible bounds ------------------------------------
df["CRP_Base"] = np.round(df_imputed["CRP_Base"], 1).clip(lower=0.1)
df["ESR_Base"] = np.round(df_imputed["ESR_Base"], 0).clip(lower=1.0)

print("Missing CRP/ESR after imputation:",
      int(df["CRP_Base"].isna().sum()), "/", int(df["ESR_Base"].isna().sum()))

df.to_excel(OUT_PATH, index=False)
print(f"Saved: {OUT_PATH}")

# ---- 4. Check against the analysis dataset ----------------------------------------------
if os.path.exists(ANALYSIS_PATH):
    ana = pd.read_excel(ANALYSIS_PATH)
    if len(ana) == len(df):
        for col in ["CRP_Base", "ESR_Base"]:
            diff = (pd.to_numeric(ana[col], errors="coerce") - df[col]).abs()
            n_diff = int((diff > 1e-6).sum())
            print(f"{col}: {n_diff} of {len(df)} values differ from {ANALYSIS_PATH}"
                  + (f" (largest difference {diff.max():.2f})" if n_diff else " (identical)"))
    else:
        print(f"Row counts differ ({len(ana)} vs {len(df)}); comparison skipped.")
