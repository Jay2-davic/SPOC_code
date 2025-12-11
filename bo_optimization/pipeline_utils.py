import os
import pandas as pd

from utils.general_util import log

def short_folder_name(folder_name, max_length=48):
    if len(folder_name) > max_length:
        return folder_name[-max_length:]
    return folder_name

def scan_experiment_folders(base_path):
    """
    Recursively scan base_path for folders containing .yaml and .pickle files.
    Returns a DataFrame with columns: ['folder', 'yaml_files', 'pickle_files', 'abs_root'].
    """
    records = []
    for root, dirs, files in os.walk(base_path):
        yaml_files = [os.path.join(root, f) for f in files if f.endswith(".yaml")]
        pickle_files = [os.path.join(root, f) for f in files if f.endswith(".pickle")]
        if yaml_files or pickle_files:
            # Use the exact folder name, do not shorten or concatenate
            records.append({
                "folder": os.path.basename(root),           # unchanged folder name
                "yaml_files": yaml_files,
                "pickle_files": pickle_files,
                "abs_root": os.path.abspath(root),
            })
    return pd.DataFrame(records)

def save_csv(df, path):
    parent = os.path.dirname(path)
    if parent:
        os.makedirs(parent, exist_ok=True)
    # If file exists, read it and append
    if os.path.isfile(path):
        try:
            existing_df = pd.read_csv(path)
            # Concatenate and drop duplicates based on all columns
            df = pd.concat([existing_df, df], ignore_index=True)
            df = df.drop_duplicates()
        except Exception as e:
            log.warnv(f"Failed to read existing CSV at {path}: {e}")
            # If reading fails, just save the new df
    df.to_csv(path, index=False)
    log.infov(f"Saved CSV to {path}")