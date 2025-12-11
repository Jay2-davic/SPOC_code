#!/bin/bash

set -euo pipefail

# for debugging each step
# pause() {
#   echo
#   read -r -p "Paused here. Press Enter to continue, or Ctrl+C to abort..." _
# }

# --- Paths and inputs ---
EXCEL_DIR="data/excel"
META_JSON="data/SPOC_battery.json"           # Step 1 output
EIS_BATCH_JSON="data/SPOC_battery_eis.json"  # Step 2 output

# Use a POSIX path for convenience, convert to Windows before passing to Python
BASE_MPR_POSIX="/c/Users/jimenez45/OneDrive - LLNL/General - High-Throughput Polymer Electrolytes DIW/eis_files"
BASE_MPR_WIN="$(cygpath -w "$BASE_MPR_POSIX")"

# Final autoprocessed cycles JSON (renamed as requested)
AUTOPROCESS_JSON="data/eis_autoprocess.json"

mkdir -p data

echo "Step 1: Cleaning Data"
Rscript data_cleaning/run_pipeline.R "$EXCEL_DIR" "$META_JSON"
echo "Data wrangling historical data complete. Output saved to $META_JSON"

echo "Step 2: Detecting and Cleaning New Files"
# Pass Windows path for the base directory
Rscript eis_processing/process_new_batch.R -j "$META_JSON" -o "$EIS_BATCH_JSON" -b "$BASE_MPR_WIN"
echo "New Data Integrated. Output saved to $EIS_BATCH_JSON"

echo "Step 3: Reading Impedance Data..."
source /c/Users/jimenez45/AppData/Local/miniforge3/etc/profile.d/conda.sh

conda activate "EIS_processing"

echo "Step 3a: Separating The Cycles -> $AUTOPROCESS_JSON"
# Pass Windows path for Python, so os.path.exists works
python eis_processing/cycle_splitter.py "$EIS_BATCH_JSON" "$AUTOPROCESS_JSON" --base-dir "$BASE_MPR_WIN"
rm -f "$EIS_BATCH_JSON"

echo "Step 3b: Fitting and Coefficient Extraction of EIS Files"
# Let autoprocess use default parallelization, or set EIS_AUTOPROC_JOBS if you want a specific level

EIS_POOL_TYPE=thread python eis_processing/autoprocess_EIS.py --json "$AUTOPROCESS_JSON" #--no-skip

echo "EIS processing complete. Output saved to $AUTOPROCESS_JSON"

conda deactivate

echo "Step 4: Calculating Ionic Conductivities and Saving Results"
Rscript eis_processing/calc_IC.R \
  --meta "$META_JSON" \
  --fitted "$AUTOPROCESS_JSON"

echo "Ionic conductivity calculation complete. Output saved to $META_JSON"