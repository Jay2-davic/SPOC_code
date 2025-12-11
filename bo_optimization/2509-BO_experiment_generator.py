import os
import argparse
import pandas as pd
import yaml  # Requires PyYAML
from datetime import datetime
import random
import string

def generate_experiment_csv(output_dir, base_data_name, base_train_params, model_configs):
    experiment_rows = []
    date_str = datetime.now().strftime("%Y%m%d")

    # Keep ratio as numeric for CSV; derive a filename-safe label for IDs
    ratio_num = float(base_train_params['top_ratio'])
    ratio_label = f"{int(round(ratio_num * 100))}pct"  # e.g., 0.1 -> 10pct

    os.makedirs(output_dir, exist_ok=True)

    for model_name, model_info in model_configs.items():
        params_list = model_info['params']
        acq_list = model_info['acqs']
        for param in params_list:
            for acq in acq_list:
                config = {
                    'data': {'name': base_data_name},
                    'model': {'name': model_name, 'params': param},
                    'train': {**base_train_params, 'acquisition': acq}
                }

                if model_name == "random_forest":
                    param_val = str(param['n_estimators'])
                elif model_name == "gaussian_process":
                    param_val = str(param['kernel']['name'])
                else:
                    param_val = "_".join(
                        f"{k}-{v if not isinstance(v, dict) else v['name']}"
                        for k, v in param.items()
                    )

                rand_tag = ''.join(random.choices(string.ascii_lowercase + string.digits, k=8))
                experiment_id = f"{model_name.upper()}-{date_str}-{rand_tag}-{param_val}_{acq}_{ratio_label}"

                # Write YAML config in the output dir
                yaml_filename = f"{experiment_id}.yaml"
                yaml_abspath = os.path.join(output_dir, yaml_filename)
                with open(yaml_abspath, "w") as f:
                    yaml.dump(config, f, default_flow_style=False)

                row = {
                    "experiment_id": experiment_id,
                    "model_name": model_name,
                    "param_val": param_val,
                    "acquisition": acq,
                    "ratio": ratio_num,  # keep as float so pandas can render decimal comma
                    "config_yaml": yaml_filename
                }
                experiment_rows.append(row)

    df = pd.DataFrame(experiment_rows)

    # Use semicolon as separator and comma as decimal for Excel-friendly CSV in comma-decimal locales
    csv_index_path = os.path.join(output_dir, f"{base_data_name}_allModels.csv")
    df.to_csv(csv_index_path, index=False, sep=';', decimal=',')

    print(f"Wrote {len(df)} experiment YAML files and index CSV to {csv_index_path}")
    return len(df)

def parse_args():
    parser = argparse.ArgumentParser(description="Generate experiment YAML files and CSV index for model/param/acq combinations.")
    parser.add_argument('--data-name', type=str, required=True, help='Name of the dataset')
    parser.add_argument('--output-csv', type=str, required=True, help='Output directory for YAML files and index CSV')
    parser.add_argument('--n-seeds', type=int, default=500, help='Number of seeds')
    parser.add_argument('--n-initial', type=int, default=2, help='Number of initial samples')
    parser.add_argument('--top-ratio', type=float, default=0.1, help='Top ratio')
    parser.add_argument('--scaler', type=str, default='standard', help='Scaler type')
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    output_dir = args.output_csv  # Now interpreted as directory
    base_train_params = {
        "n_seeds": args.n_seeds,
        "n_initial": args.n_initial,
        "top_ratio": args.top_ratio,
        "scaler": args.scaler
    }
    model_configs = {
        "random_forest": {
            "params": [
                {"n_estimators": 25, "n_jobs": -1},
                {"n_estimators": 50, "n_jobs": -1},
                {"n_estimators": 100, "n_jobs": -1}
            ],
            "acqs": ["EI", "PI", "LCB"]
        },
        "gaussian_process": {
            "params": [
                {"kernel": {"name": "RBF"}},
                {"kernel": {"name": "Matern"}},
                {"kernel": {"name": "RQ"}},
                {"kernel": {"name": "ESS"}}
            ],
            "acqs": ["EI", "PI", "LCB"]
        }
    }
    generate_experiment_csv(
        output_dir=output_dir,
        base_data_name=args.data_name,
        base_train_params=base_train_params,
        model_configs=model_configs
    )