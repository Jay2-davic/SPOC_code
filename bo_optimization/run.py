#%%
import os
import argparse
import pandas as pd

from utils.general_util import log
from utils import config_util
from engine import Engine

# Helper: normalize dataset file path inside the loaded configs
def _normalize_data_paths(configs, base_dir):
    data_cfg = configs.get("data", {})
    if not isinstance(data_cfg, dict):
        return

    # Try these common keys for the dataset CSV path
    for key in ("file_name", "file_path", "csv_path"):
        p = data_cfg.get(key)
        if p:
            p = str(p)
            # Only normalize if relative
            if not os.path.isabs(p):
                data_cfg[key] = os.path.normpath(os.path.join(base_dir, p))
                log.infov(f"Resolved data path for '{key}': {data_cfg[key]}")
    configs["data"] = data_cfg

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

def set_cwd_to_script_dir(enable=True):
    if not enable:
        return
    try:
        os.chdir(SCRIPT_DIR)
        log.infov(f"CWD set to script dir: {SCRIPT_DIR}")
    except Exception as e:
        log.warnv(f"Failed to set CWD to script dir {SCRIPT_DIR}: {e}")

def run_one(config_path, save_dir, args_dict):
    # Load YAML
    configs = config_util.load_configs(config_path)
    # Determine base for resolving relative data paths:
    workdir = args_dict.get("workdir", "")
    yaml_dir = os.path.dirname(os.path.abspath(config_path))
 
    base_for_data = os.path.abspath(workdir) if workdir else yaml_dir
    
    _normalize_data_paths(configs, base_for_data)

    # Pass the YAML path for downstream use
    configs["yaml_path"] = os.path.abspath(config_path)
    # Create and run Engine
    engine = Engine(configs=configs, save_dir=save_dir, args_dict=args_dict)
    engine.run()

def main():
    parser = argparse.ArgumentParser(description="Run experiments from a YAML or a CSV index")
    parser.add_argument("--config_path", type=str, default="", help="Path to a single YAML config")
    parser.add_argument("--csv_path", type=str, default="", help="Path to a CSV listing configs to run")
    parser.add_argument("--save_dir", type=str, default="", help="Root directory for run outputs")
    parser.add_argument("--results-path", type=str, default="", help="Base path to resolve relative YAMLs in CSV rows")
    parser.add_argument("--workdir", type=str, default="", help="Optional base directory to resolve relative paths; if provided, process CWD will be set to this directory")

    args = parser.parse_args()
    set_cwd_to_script_dir(enable=True)

    # If --workdir is provided, set it as process CWD for consistent relative path resolution
    if args.workdir:
        abs_workdir = os.path.abspath(args.workdir)
        if not os.path.isdir(abs_workdir):
            raise NotADirectoryError(f"--workdir does not exist or is not a directory: {abs_workdir}")
        os.chdir(abs_workdir)
        log.infov(f"Process CWD set to workdir: {abs_workdir}")
    
    # Log final CWD
    log.infov(f"Process CWD (effective): {os.getcwd()}")

    # Single YAML mode
    if args.config_path:
        config_path = args.config_path
        # If the provided config_path is relative, resolve it against current CWD,
        # which is either original CWD or --workdir if set.
        if not os.path.isabs(config_path):
            config_path = os.path.abspath(config_path)

        run_one(config_path, args.save_dir, vars(args))
        return

    # CSV mode
    if args.csv_path:
        index_csv = args.csv_path
        if not os.path.isabs(index_csv):
            index_csv = os.path.abspath(index_csv)

        if not os.path.isfile(index_csv):
            raise FileNotFoundError(f"CSV not found: {index_csv}")

        # Your CSV uses semicolons as delimiters
        df = pd.read_csv(index_csv, sep=";")

        # Column that contains YAML paths
        yaml_cols = ["config_yaml"]  # keep it explicit to your CSV schema
        col = next((c for c in yaml_cols if c in df.columns), None)
        if not col:
            raise ValueError(f"No YAML path column found in CSV. Expected one of: {yaml_cols}. Got columns: {list(df.columns)}")

        # Base to resolve relative YAMLs found in the CSV
        base_for_yaml = args.results_path or os.path.dirname(index_csv)
        base_for_yaml = os.path.abspath(base_for_yaml)

        for i, row in df.iterrows():
            yaml_value = str(row[col]).strip()
            if not yaml_value:
                log.warnv(f"Row {i}: empty YAML path, skipping")
                continue

            config_path = yaml_value
            if not os.path.isabs(config_path):
                config_path = os.path.normpath(os.path.join(base_for_yaml, config_path))
            if not os.path.isfile(config_path):
                log.warnv(f"Row {i}: YAML not found after resolution: {config_path}, skipping")
                continue

            try:
                run_one(config_path, args.save_dir, vars(args))
            except Exception as e:
                log.error(f"Row {i}: failed to run {config_path}. Error: {e}")
        return

    raise ValueError("You must provide either --config_path or --csv_path")

#%%
if __name__ == "__main__":
    main()