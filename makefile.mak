# =============================================================================
# Polymer Electrolyte Database Analysis Pipeline
# =============================================================================
#
# Usage:
#   make config          # Create example config file
#   make clean_db        # Run database cleaning pipeline
#   make eis             # Run EIS processing pipeline
#   make analysis        # Run grid search analysis
#   make all             # Run complete pipeline
#   make help            # Show this help message

.PHONY: help config clean_db eis analysis all check

# Default target
.DEFAULT_GOAL := help

# Configuration
CONFIG_FILE ?= config.yaml
RSCRIPT := Rscript

# Check if config exists
check:
	@if [ ! -f $(CONFIG_FILE) ]; then \
		echo "Error: $(CONFIG_FILE) not found"; \
		echo "Run 'make config' to create an example configuration"; \
		exit 1; \
	fi

# Create example configuration
config:
	@echo "Creating example configuration..."
	@$(RSCRIPT) -e "source('~/Git/23-spoc_code/PE_plotTemplates/config.R'); create_example_config('$(CONFIG_FILE)')"
	@echo "Created $(CONFIG_FILE)"
	@echo "Please edit this file with your settings before running pipelines"

# Database cleaning pipeline
clean_db: check
	@echo "Running database cleaning pipeline..."
	@$(RSCRIPT) ~/Git/23-spoc_code/PE_plotTemplates/run_database_cleaning.R --config $(CONFIG_FILE)

# EIS processing pipeline
eis: check
	@echo "Running EIS processing pipeline..."
	@$(RSCRIPT) ~/Git/23-spoc_code/EIS_Analysis/run_eis_processing.R --config $(CONFIG_FILE)

# Grid search analysis
analysis: check
	@echo "Running grid search analysis..."
	@$(RSCRIPT) ~/Git/23-spoc_code/PE_plotTemplates/run_gridsearch_analysis.R --config $(CONFIG_FILE)

# Run complete pipeline
all: clean_db eis analysis
	@echo ""
	@echo "=== Complete Pipeline Finished ==="
	@echo ""

# Help message
.PHONY: help config historical clean eis analysis merge all check

.DEFAULT_GOAL := help

CONFIG_FILE ?= config.yaml
RSCRIPT := Rscript

check:
	@if [ ! -f $(CONFIG_FILE) ]; then \
		echo "Error: $(CONFIG_FILE) not found"; \
		echo "Run 'make config' to create configuration"; \
		exit 1; \
	fi

config:
	@echo "Creating configuration template..."
	@$(RSCRIPT) -e "source('shared/config.R'); create_example_config('$(CONFIG_FILE)')"
	@echo "✓ Created $(CONFIG_FILE)"
	@echo "Edit this file before running pipelines"

# ONE-TIME: Process historical data
historical:
	@echo "Processing historical data (excel_file1 & 2)..."
	@$(RSCRIPT) historical/process_historical.R --config $(CONFIG_FILE)
	@echo "✓ historical_samples.json created"
	@echo "⚠️  Only rerun this if historical data changes"

# REGULAR: Process active/updating data
clean: check
	@$(RSCRIPT) data_cleaning/run_pipeline.R --config $(CONFIG_FILE)

# EIS Python processing (separate step)
eis-python: check
	@bash eis_processing/run_python.sh

# EIS R processing (separate step, requires Python first)
eis-r: check
	@$(RSCRIPT) eis_processing/run_pipeline.R --config $(CONFIG_FILE)

# Complete EIS (unified: Python + R in sequence)
eis: check
	@bash eis_processing/run_complete.sh $(CONFIG_FILE)

analysis: check
	@$(RSCRIPT) analysis/run_pipeline.R --config $(CONFIG_FILE)

merge: check
	@echo "Merging all JSON files to master..."
	@$(RSCRIPT) shared/merge_masters.R --config $(CONFIG_FILE)
	@echo "✓ master_database.json updated"

# Complete pipeline (assumes historical already run)
all: clean eis analysis merge
	@echo ""
	@echo "=== Complete Pipeline Finished ==="
	@echo ""

# Initial setup (includes historical processing)
setup: config historical
	@echo ""
	@echo "=== Setup Complete ==="
	@echo "Now run: make all"
	@echo ""

# Incremental merges
merge-clean:
	@$(RSCRIPT) shared/merge_masters.R --config $(CONFIG_FILE) --stage clean

merge-eis:
	@$(RSCRIPT) shared/merge_masters.R --config $(CONFIG_FILE) --stage eis

merge-analysis:
	@$(RSCRIPT) shared/merge_masters.R --config $(CONFIG_FILE) --stage analysis

help:
	@echo "Polymer Electrolyte Database Analysis Pipeline"
	@echo ""
	@echo "Initial Setup:"
	@echo "  make config       - Create configuration template"
	@echo "  make historical   - Process historical data (RUN ONCE)"
	@echo "  make setup        - Complete initial setup (config + historical)"
	@echo ""
	@echo "Regular Pipeline:"
	@echo "  make clean        - Process active data (excel_file3 & 4)"
	@echo "  make eis-python   - Run Python EIS processing (cycle split + autoprocess)"
	@echo "  make eis-r        - Run R EIS integration (requires eis-python first)"
	@echo "  make eis          - Complete EIS (Python + R)"
	@echo "  make analysis     - Run grid search analysis"
	@echo "  make merge        - Merge all JSONs to master"
	@echo "  make all          - Run complete pipeline (clean + eis + analysis + merge)"
	@echo ""
	@echo "Incremental Updates:"
	@echo "  make merge-clean  - Merge only cleaning updates"
	@echo "  make merge-eis    - Merge only EIS updates"
	@echo "  make merge-analysis - Merge only analysis updates"
	@echo ""
	@echo "Examples:"
	@echo "  make setup                    # First time"
	@echo "  make all                      # Regular updates"
	@echo "  make eis-python               # Just Python processing"
	@echo "  make clean && make merge-clean  # Quick data update"