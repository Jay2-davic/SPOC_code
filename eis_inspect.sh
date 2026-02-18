#!/usr/bin/env bash
# Minimal runner, hardcoded JSON path
# Usage: ./run_eis.sh

Rscript eis_fitting_viewer.R "data/SPOC_battery.json" "data/eis_autoprocess.json"
