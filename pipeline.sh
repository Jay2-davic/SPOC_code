#!/bin/bash
set -euo pipefail

# =============================================================================
# COMBINED POLYMER ELECTROLYTE PIPELINE
# Integrates: EIS Processing → Data Cleaning → ML Dataset Creation → Plotting
# =============================================================================

# --- Paths and inputs ---
EXCEL_DIR="data/excel"
BASE_MPR_POSIX="/c/Users/jimenez45/OneDrive - LLNL/General - High-Throughput Polymer Electrolytes DIW/eis_files"
BASE_MPR_WIN="$(cygpath -w "$BASE_MPR_POSIX" 2>/dev/null || echo "$BASE_MPR_POSIX")"

# Dataset configuration
DATASET_NAME="polymer_dataset"
CONFIG_FILE=""
MANU_DIR="manuscript"
GENERATE_PLOTS=true
SKIP_EIS=false
SKIP_ML=false
EIS_JOBS=4

# File paths
META_JSON="data/SPOC_battery.json"
EIS_BATCH_JSON="data/SPOC_battery_eis.json"
AUTOPROCESS_JSON="data/eis_autoprocess.json"
FINAL_JSON="data/pe_database.json"

mkdir -p data

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
    case $1 in
        -e|--excel-dir) EXCEL_DIR="$2"; shift 2 ;;
        -b|--base-mpr) BASE_MPR_POSIX="$2"; BASE_MPR_WIN="$(cygpath -w "$BASE_MPR_POSIX" 2>/dev/null || echo "$BASE_MPR_POSIX")"; shift 2 ;;
        -d|--dataset) DATASET_NAME="$2"; shift 2 ;;
        -c|--config) CONFIG_FILE="$2"; shift 2 ;;
        -m|--manu-dir) MANU_DIR="$2"; shift 2 ;;
        -p|--plots) GENERATE_PLOTS=true; shift ;;
        --skip-eis) SKIP_EIS=true; shift ;;
        --skip-ml) SKIP_ML=false; shift ;;
        --eis-jobs) EIS_JOBS="$2"; shift 2 ;;
        -h|--help) 
            echo "Usage: ./run_full_pipeline.sh [OPTIONS]"
            echo ""
            echo "OPTIONS:"
            echo "  -d, --dataset NAME    Dataset name"
            echo "  -c, --config FILE     Config file"
            echo "  -m, --manu-dir DIR    Manuscript figures directory"
            echo "  -p, --plots           Generate plots"
            echo "  --skip-eis            Skip EIS processing"
            echo "  --skip-ml             Skip ML dataset creation"
            echo "  --eis-jobs N          Parallel jobs for EIS (default: 4)"
            exit 0 
            ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

echo "=========================================="
echo "Running Polymer Electrolyte Pipeline"
echo "Dataset: $DATASET_NAME | Plots: $GENERATE_PLOTS"
echo "Skip EIS: $SKIP_EIS | Skip ML: $SKIP_ML"
echo "=========================================="

# =============================================================================
# PART 1: EIS PROCESSING
# =============================================================================

# if [ "$SKIP_EIS" = "false" ]; then
    echo "Step 1: Cleaning Excel Data"
    Rscript data_cleaning/run_pipeline.R "$EXCEL_DIR" "$META_JSON"
    echo "Data cleaning complete. Output: $META_JSON"
# 
#     echo "Step 2: Detecting and Cleaning New EIS Files"
#     Rscript eis_processing/process_new_batch.R -j "$META_JSON" -o "$EIS_BATCH_JSON" -b "$BASE_MPR_WIN"
#     echo "New data integrated. Output: $EIS_BATCH_JSON"
# 
#     echo "Step 3: Reading Impedance Data..."
#     source /c/Users/jimenez45/AppData/Local/miniforge3/etc/profile.d/conda.sh
#     conda activate "EIS_processing"
# 
#     echo "Step 3a: Separating The Cycles -> $AUTOPROCESS_JSON"
#     python eis_processing/cycle_splitter.py "$EIS_BATCH_JSON" "$AUTOPROCESS_JSON" --base-dir "$BASE_MPR_WIN"
    # rm -f "$EIS_BATCH_JSON"
# 
#     echo "Step 3b: Fitting and Coefficient Extraction of EIS Files"
#     EIS_POOL_TYPE=thread python eis_processing/autoprocess_EIS.py --json "$AUTOPROCESS_JSON" --jobs "$EIS_JOBS"
#     echo "EIS processing complete. Output: $AUTOPROCESS_JSON"
# 
#     conda deactivate
# 
    echo "Step 4: Calculating Ionic Conductivities"
    Rscript eis_processing/calc_IC.R --sample-file "$META_JSON" --eis-file "$AUTOPROCESS_JSON"
    echo "Ionic conductivity calculation complete. Output: $META_JSON"

    cp "$META_JSON" "$FINAL_JSON"
    echo "Final database ready: $FINAL_JSON"
# fi

# =============================================================================
# PART 2: ML DATASET CREATION AND VISUALIZATION
# =============================================================================

if [ "$SKIP_ML" = "false" ]; then
    echo "Step 5: Creating ML Dataset"
    Rscript analysis/run_pipeline.R "$DATASET_NAME"
    echo "ML dataset creation complete. Output: data/${DATASET_NAME}.csv"
fi

echo "=========================================="
echo "Pipeline Complete!"
echo "=========================================="