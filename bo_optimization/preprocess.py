import argparse
import os
import pandas as pd

from utils.general_util import log
from pipeline_utils import scan_experiment_folders, save_csv

def build_runs_csv(index_df):
    """
    Flatten index to a runs CSV with one row per YAML file.
    Columns: ['experiment_id', 'yaml_path', 'results_pickle', 'abs_root'].
    """
    rows = []
    for _, rec in index_df.iterrows():
        yaml_files = rec.get("yaml_files", []) or []
        pickle_files = rec.get("pickle_files", []) or []
        # Try to find a results.pickle in the folder
        results_pickle = ""
        for p in pickle_files:
            if os.path.basename(p) == "results.pickle":
                results_pickle = os.path.abspath(p)
                break

        for i, y in enumerate(yaml_files):
            rows.append({
                "experiment_id": f"{rec['folder']}_{i}",
                "yaml_path": os.path.abspath(y),
                "results_pickle": results_pickle,
                "abs_root": rec.get("abs_root", ""),
            })
    return pd.DataFrame(rows)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--scan_path", required=True, help="Directory to recursively scan for .yaml and .pickle files")
    parser.add_argument("--results-path", dest="results_path", default="", help="Base folder to place outputs")
    parser.add_argument("--output_index", default="", help="Path to save the index CSV, defaults to results_path/experiment_index.csv or scan_path/experiment_index.csv")
    parser.add_argument("--output_runs", default="", help="Optional path to save a flattened runs CSV")
    args = parser.parse_args()

    index_df = scan_experiment_folders(args.scan_path)

    # Decide where to save the index CSV
    if args.output_index:
        index_csv = args.output_index
    else:
        base_dir = args.results_path if args.results_path else args.scan_path
        index_csv = os.path.join(base_dir, "experiment_index.csv")

    save_csv(index_df, index_csv)

    # Optionally, produce a normalized runs CSV
    if args.output_runs:
        runs_df = build_runs_csv(index_df)
        save_csv(runs_df, args.output_runs)
        log.infov(f"Saved runs CSV to {args.output_runs}")

if __name__ == "__main__":
    main()