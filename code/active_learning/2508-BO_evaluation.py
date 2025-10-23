#%%
import argparse
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import pickle
import yaml
import math
import json
from pathlib import Path
from scipy.interpolate import interp1d

# Ensure relative file operations resolve correctly when running as a script
os.chdir(os.path.dirname(os.path.abspath(__file__)))

# Optional import safety; replace with your actual Dataset import if needed
from datasets.base import Dataset

# =========================
# Safe I/O helpers
# =========================
def load_yaml_safe(path):
    """
    Safely load a YAML file.

    Parameters:
        path (str or Path or None): Path to YAML file.

    Returns:
        dict or None: Parsed YAML content, or None if missing/invalid.
    """
    if not path or (isinstance(path, float) and pd.isna(path)):
        return None
    p = Path(path)
    if not p.is_file():
        return None
    try:
        with p.open("r", encoding="utf-8") as f:
            return yaml.safe_load(f)
    except Exception:
        return None


def load_pickle_safe(path):
    """
    Safely load a pickle file.

    Parameters:
        path (str or Path or None): Path to pickle file.

    Returns:
        Any: Parsed object; if it is a custom object with __dict__, return a dict
        containing a type marker and its attributes. Returns None on error.
    """
    if not path or (isinstance(path, float) and pd.isna(path)):
        return None
    p = Path(path)
    if not p.is_file():
        return None
    try:
        with p.open("rb") as f:
            obj = pickle.load(f)
        if hasattr(obj, "__dict__") and isinstance(obj.__dict__, dict):
            return {"__object_type__": type(obj).__name__, **obj.__dict__}
        return obj
    except Exception:
        return None


# =========================
# Dataframe normalization
# =========================
def flatten_dict_columns(df, dict_cols):
    """
    Flatten JSON-like dict columns into top-level columns.

    Parameters:
        df (pd.DataFrame): Input dataframe.
        dict_cols (list[str]): Column names to flatten.

    Returns:
        pd.DataFrame: Flattened dataframe with duplicate columns removed.
    """
    for col in dict_cols:
        if col in df.columns:
            norm = pd.json_normalize(df[col])
            norm = norm.loc[:, ~norm.columns.duplicated()]
            df = df.drop(columns=[col]).join(norm)
    df = df.loc[:, ~df.columns.duplicated()]
    return df


def strict_load_experiments(
    dataframe_path,
    base_path=None,
    yaml_col="yaml_path",
    results_col="results_path",
):
    """
    Load an experiment registry CSV, parse associated YAML and pickle artifacts,
    optionally rewrite dataset paths, and flatten nested dicts.

    Parameters:
        dataframe_path (str): Path to the experiments CSV.
        base_path (str or None): Base directory to rewrite config["data"]["name"] to a CSV
                                 named "<name_without_ext>_dataset.csv".
        yaml_col (str): Column name with YAML file paths.
        results_col (str): Column name with pickle result file paths.

    Returns:
        pd.DataFrame: Flattened dataframe with metadata and results in columns.

    Raises:
        ValueError: If the CSV is missing required columns.
    """
    df = pd.read_csv(dataframe_path)
    if yaml_col not in df.columns or results_col not in df.columns:
        raise ValueError(f"CSV missing required columns: {yaml_col}, {results_col}")
    # Load artifacts safely
    df["configs_path"] = df['yaml_path'].apply(load_yaml_safe)
    df["results_path"] = df['results_path'].apply(load_pickle_safe)

    # Rewrite data.name to a dataset CSV path if base_path provided and name is relative
    if base_path is not None:
        for idx, row in df.iterrows():
            config = row["configs_path"]
            if isinstance(config, dict):
                data = config.get("data", {})
                data_name = data.get("name")
                if data_name and not os.path.isabs(data_name):
                    base = os.path.splitext(os.path.basename(data_name))[0]
                    new_name = base + "_dataset.csv"
                    full_path = os.path.join(base_path, new_name)
                    config["data"]["name"] = full_path
                    df.at[idx, "configs_path"] = config
                    # Keep side-effect minimal and silent to avoid noisy runs

    # Drop noisy or irrelevant columns
    error_cols = [c for c in df.columns if "error" in c]
    df = df.drop(columns=error_cols, errors="ignore")

    # Flatten nested dicts
    df = flatten_dict_columns(df, ["configs_path", "results_path"])

    # Remove columns starting with 'train.' or 'model.' or ending with 'sec'
    df = df[
        [
            c
            for c in df.columns
            if not (c.startswith("train.") or c.startswith("model.") or c.endswith("sec"))
        ]
    ]

    return df


# =========================
# Dataset helpers
# =========================
def extract_data_name_(data_row, base_path):
    """
    Build the full path of the dataset CSV based on the config 'data.name' field,
    using convention: <base_path>/<name_without_ext>_dataset.csv.

    Parameters:
        data_row (dict or pd.Series): A row with 'data.name' field.
        base_path (str): Base path.

    Returns:
        str: Full path to the dataset CSV.

    Raises:
        KeyError: If 'data.name' is missing from the row.
    """
    if "data.name" not in data_row:
        raise KeyError("Missing 'data.name' in row. Ensure strict_load_experiments was used.")
    name_wo_ext = os.path.splitext(os.path.basename(data_row["data.name"]))[0]
    new_name = name_wo_ext + ".csv"
    full_path = os.path.join(base_path, new_name)
    return full_path


def get_top_indices(data_row, base_path):
    """
    Determine indices of the top items by lowest 'ionic_conductivity_final',
    using the per-row 'top_ratio'.

    Parameters:
        data_row (dict or pd.Series): Row containing 'top_ratio' and 'data.name'.
        base_path (str): Base path used to construct dataset path.

    Returns:
        list[int]: Indices of the top items.

    Raises:
        ValueError: If required columns are missing in the dataset.
    """
    # Read dataset
    data_path = extract_data_name_(data_row, base_path)
    df = pd.read_csv(data_path)

    # Validate required column for ranking
    if "ionic_conductivity_final" not in df.columns:
        raise ValueError(
            f"Dataset at {data_path} missing required column 'ionic_conductivity_final'."
        )

    # Determine how many top items to select
    if "top_ratio" not in data_row or pd.isna(data_row["top_ratio"]):
        raise ValueError("Row is missing 'top_ratio'.")
    top_ratio = float(data_row["top_ratio"])
    if not (0 < top_ratio <= 1):
        raise ValueError(f"Invalid top_ratio={top_ratio}. Must be in (0, 1].")

    n_top = int(math.ceil(len(df) * top_ratio))
    top_indices = df.sort_values("ionic_conductivity_final").head(n_top).index.to_list()
    return top_indices


def load_index_(data_row, base_path):
    """
    Load dataset length N and the number of top items n_top for the given row.

    Parameters:
        data_row (dict or pd.Series): Row containing 'top_ratio' and 'data.name'.
        base_path (str): Base path used to construct dataset path.

    Returns:
        tuple[int, int]: (N, n_top)
    """
    data_path = extract_data_name_(data_row, base_path)
    df = pd.read_csv(data_path)
    N = len(df)
    top_indices = get_top_indices(data_row, base_path)
    n_top = len(top_indices)
    return N, n_top


# =========================
# Metrics computation
# =========================
def TopPercent(x_top_count_runs, n_top, N):
    """
    Compute TopPercent per run across iterations.

    Parameters:
        x_top_count_runs (list[list[int]] or np.ndarray): For each run, a list/array where position j
            holds the count of selected items that are within the top set, at iteration j.
        n_top (int): Number of items in the top set.
        N (int): Total number of iterations to consider for alignment.

    Returns:
        list[list[float]]: Each inner list contains fraction-in-top for each iteration (NaN for missing).
    """
    # Convert to numpy array for shape checking
    x_top_count_runs = np.array(x_top_count_runs)
    # Ensure input is 2D
    if x_top_count_runs.ndim != 2:
        raise ValueError(
            f"TopPercent expects a 2D array or list of lists, got shape {x_top_count_runs.shape}."
        )
    
    data = []
    for run_counts in x_top_count_runs:
        row = []
        for j in range(N):
            if j < len(run_counts):
                row.append(run_counts[j] / n_top)
            else:
                row.append(float('nan'))
        data.append(row)
    return data  # Always a list of lists


def P_rand(n_select, n_top, population_size=None):
    """
    Compute expected TopPercent curve under random selection without replacement.

    Parameters:
        n_select (int): Number of selection steps (iterations).
        n_top (int): Number of top items (M).
        population_size (int or None): Total population size (N). If None, assume N == n_select.

    Returns:
        np.ndarray: Expected TopPercent at each iteration j, relative to n_top.
                    Shape: (n_select,). Values in [0, 1].

    Notes:
        - If population_size is provided, the baseline respects N as dataset size.
        - The baseline represents expected cumulative fraction of top items acquired
          by iteration j divided by n_top.
    """
    if population_size is None:
        population_size = n_select

    x_random = np.arange(n_select)
    M = float(n_top)
    N = float(population_size)

    P = np.empty(n_select, dtype=float)
    E = np.empty(n_select, dtype=float)

    # Probability of selecting a top item at first draw
    P[0] = M / N
    E[0] = P[0]  # expected number of top items selected by iteration 1

    for i in x_random[1:]:
        # Expected remaining top items M - E[i - 1] divided by remaining population N - i
        P[i] = (M - E[i - 1]) / (N - i)
        E[i] = np.sum(P[: (i + 1)])

    # Expected cumulative fraction of top items by iteration i
    return E / M


def EF(top_percent_curve, n_top, population_size):
    """
    Compute Enrichment Factor (EF) by dividing TopPercent by random baseline per iteration.

    Parameters:
        top_percent_curve (np.ndarray or list[float]): TopPercent values per iteration.
        n_top (int): Number of top items.
        population_size (int): Total dataset size N.

    Returns:
        np.ndarray: EF values per iteration. NaNs where top_percent_curve has NaN.
    """
    n_eval = len(top_percent_curve)
    baseline = P_rand(n_eval, n_top, population_size=population_size)
    ef = np.divide(top_percent_curve, baseline, out=np.full_like(baseline, np.nan), where=~np.isnan(top_percent_curve))
    return ef


def AF(top_percent_curve, n_top, population_size, x_grid=None):
    """
    Compute Acceleration Factor (AF) using iteration counts required to reach
    the same TopPercent between random and method curves.

    Parameters:
        top_percent_curve (np.ndarray or list[float]): TopPercent per iteration for a method.
        n_top (int): Number of top items.
        population_size (int): Dataset size N.
        x_grid (np.ndarray or None): TopPercent values to evaluate AF at. If None,
            use the union of method and random top_percent values.

    Returns:
        tuple[np.ndarray, np.ndarray]: (x_grid, af_values)
            x_grid: TopPercent values where AF is computed.
            af_values: iterations_random / iterations_method at each x_grid.

    Notes:
        - Requires that top_percent_curve be non-decreasing to make interpolation meaningful.
          If minor non-monotonicity exists, consider smoothing or monotone interpolation.
        - We interpolate iteration as a function of TopPercent using linear interpolation.
    """
    # Prepare iteration axis
    top_percent_curve = np.asarray(top_percent_curve, dtype=float)
    n_iters = len(top_percent_curve)
    iterations = np.arange(1, n_iters + 1, dtype=float)

    # Compute random baseline TopPercent curve
    top_percent_rs = P_rand(n_iters, n_top, population_size=population_size)

    # Build interpolation functions: iteration = f(top_percent)
    # We rely on linear interpolation and allow extrapolation, but warn if decreasing.
    if np.any(np.diff(top_percent_curve[~np.isnan(top_percent_curve)]) < 0):
        # Non-monotone warning; in production you may want monotone spline or smoothing.
        pass

    # Drop NaNs for interpolation, use only valid pairs
    valid_method = ~np.isnan(top_percent_curve)
    valid_random = ~np.isnan(top_percent_rs)

    interp_top = interp1d(
        top_percent_curve[valid_method], iterations[valid_method], kind="linear",
        fill_value="extrapolate", assume_sorted=False
    )
    interp_top_rs = interp1d(
        top_percent_rs[valid_random], iterations[valid_random], kind="linear",
        fill_value="extrapolate", assume_sorted=False
    )

    # Shared x_grid
    combined_top = np.union1d(top_percent_curve[valid_method], top_percent_rs[valid_random])
    if x_grid is None:
        x_grid = combined_top

    iter_interp = interp_top(x_grid)
    iter_interp_rs = interp_top_rs(x_grid)

    af = np.divide(iter_interp_rs, iter_interp, out=np.full_like(iter_interp, np.nan), where=~np.isnan(iter_interp))
    return x_grid, af


def avg_stats(top_percent_df):
    """
    Compute per-iteration summary statistics across runs.

    Parameters:
        top_percent_df (pd.DataFrame): Rows are runs, columns iter_0..iter_{N-1}.

    Returns:
        dict[str, np.ndarray]: Keys: median, q05, q95, mean, std.
    """
    stats = {
        "median": top_percent_df.median(axis=0, skipna=True).values,
        "q05": top_percent_df.quantile(0.05, axis=0, interpolation="linear").values,
        "q95": top_percent_df.quantile(0.95, axis=0, interpolation="linear").values,
        "mean": top_percent_df.mean(axis=0, skipna=True).values,
        "std": top_percent_df.std(axis=0, skipna=True).values,
    }
    return stats


def calcTopPercent(data_row, base_path):
    N, n_top = load_index_(data_row, base_path)
    top_counts = data_row["top_counts"]
    
    print("DEBUG top_counts type:", type(top_counts))
    print("DEBUG top_counts value:", top_counts)

    # Ensure top_counts is a list of lists
    if isinstance(top_counts, list) and all(isinstance(x, int) for x in top_counts):
        top_counts = [top_counts]

    top_percent_list = TopPercent(top_counts, n_top, N)
    top_percent_df = pd.DataFrame(top_percent_list, columns=[f"iter_{k}" for k in range(N)])
    stats = avg_stats(top_percent_df)
    out = pd.DataFrame({
        "iteration": np.arange(1, N + 1),
        "median": stats["median"],
        "q05": stats["q05"],
        "q95": stats["q95"],
        "mean": stats["mean"],
        "std": stats["std"],
    })
    return out


def calcBaseline(data_row, base_path):
    """
    Compute random baseline TopPercent for plotting.

    Parameters:
        data_row (pd.Series or dict): Experiment row.
        base_path (str): Base path.

    Returns:
        pd.DataFrame: Columns ['iteration', 'top_percent'] with 1-based iteration.
    """
    N, n_top = load_index_(data_row, base_path)
    baseline = P_rand(N, n_top, population_size=N)
    df = {
        "iteration": np.arange(1, N + 1),
        "top_percent": baseline,
    }
    return pd.DataFrame(df)


def calcEF(data_row, base_path):
    """
    Compute EF for summary statistics across iterations.

    Parameters:
        data_row (pd.Series or dict): Experiment row.
        base_path (str): Base path.

    Returns:
        pd.DataFrame: Columns ['iteration', 'median', 'q05', 'q95', 'mean', 'std'].
    """
    top_df = calcTopPercent(data_row, base_path)
    N, n_top = load_index_(data_row, base_path)
    population_size = N

    stats = {
        "iteration": top_df["iteration"].values,
        "median": EF(top_df["median"].values, n_top, population_size),
        "q05": EF(top_df["q05"].values, n_top, population_size),
        "q95": EF(top_df["q95"].values, n_top, population_size),
        "mean": EF(top_df["mean"].values, n_top, population_size),
        "std": EF(top_df["std"].values, n_top, population_size),
    }
    return pd.DataFrame(stats)


def calcAF(data_row, base_path):
    """
    Compute AF curves aligned on the median TopPercent grid.

    Parameters:
        data_row (pd.Series or dict): Experiment row.
        base_path (str): Base path.

    Returns:
        pd.DataFrame: Columns ['TopPercent', 'median', 'q05', 'q95', 'mean', 'std'].
    """
    top_df = calcTopPercent(data_row, base_path)
    N, n_top = load_index_(data_row, base_path)
    population_size = N

    x_grid = np.asarray(top_df["median"].values, dtype=float)

    x_median, y_median = AF(top_df["median"].values, n_top, population_size, x_grid=x_grid)
    _, y_q95 = AF(top_df["q95"].values, n_top, population_size, x_grid=x_grid)
    _, y_q05 = AF(top_df["q05"].values, n_top, population_size, x_grid=x_grid)
    _, y_mean = AF(top_df["mean"].values, n_top, population_size, x_grid=x_grid)
    _, y_std = AF(top_df["std"].values, n_top, population_size, x_grid=x_grid)

    return pd.DataFrame({
        "TopPercent": x_median,
        "median": y_median,
        "q95": y_q95,
        "q05": y_q05,
        "mean": y_mean,
        "std": y_std
    })


# =========================
# Plotting helpers
# =========================
def plot_top_percent(data_row, base_path, ax=None, color="blue", label="TopPercent Median", fontsize=14):
    """
    Plot TopPercent summary statistics and random baseline.

    Returns:
        matplotlib.axes.Axes
    """
    df = calcTopPercent(data_row, base_path)
    df_baseline = calcBaseline(data_row, base_path)

    if ax is None:
        fig, ax = plt.subplots(figsize=(9, 4))

    N = len(df)

    ax.plot(df["iteration"], df["median"], label=label, color=color, linewidth=3)
    ax.fill_between(df["iteration"], df["q05"], df["q95"], color=color, alpha=0.1)

    ax.plot(df_baseline["iteration"], df_baseline["top_percent"], "--", color="black", label="Random Baseline", linewidth=3)

    ax.set_ylabel("Top (%)", fontsize=fontsize)
    ax.set_xlabel("Iteration", fontsize=fontsize)
    ax.set_xlim([1, N])
    ax.set_ylim([0, 1.1])
    ax.legend(loc="upper right", fontsize=fontsize, frameon=True)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.xaxis.set_tick_params(labelsize=fontsize)
    ax.yaxis.set_tick_params(labelsize=fontsize)
    ax.set_title("TopPercent Statistics", fontsize=fontsize)

    return ax


def plot_EF(data_row, base_path, ax=None, color="green", label="EF Median", fontsize=14):
    """
    Plot EF summary statistics with baseline at 1.

    Returns:
        matplotlib.axes.Axes
    """
    df = calcEF(data_row, base_path)

    if ax is None:
        fig, ax = plt.subplots(figsize=(9, 4))

    N = len(df)

    ax.plot(df["iteration"], df["median"], label=label, color=color, linewidth=3)
    ax.fill_between(df["iteration"], df["q05"], df["q95"], color=color, alpha=0.1)

    ax.plot(df["iteration"], np.ones(N), "--", color="black", label="Random Baseline", linewidth=3)

    ax.set_ylabel("EF", fontsize=fontsize)
    ax.set_xlabel("Iteration", fontsize=fontsize)
    ax.set_xlim([1, N])
    ax.set_ylim([0, max(np.nanmax(df['median']), 1) + 1])
    ax.legend(loc="upper right", fontsize=fontsize, frameon=True)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.xaxis.set_tick_params(labelsize=fontsize)
    ax.yaxis.set_tick_params(labelsize=fontsize)
    ax.set_title("Enrichment Factor Statistics", fontsize=fontsize)

    return ax


def plot_AF(data_row, base_path, ax=None, color="purple", label="AF Median", fontsize=14):
    """
    Plot AF versus TopPercent with baseline at 1.

    Returns:
        matplotlib.axes.Axes
    """
    df = calcAF(data_row, base_path)

    x = np.asarray(df["TopPercent"], dtype=float)
    y_median = np.asarray(df["median"], dtype=float)
    y_q05 = np.asarray(df["q05"], dtype=float)
    y_q95 = np.asarray(df["q95"], dtype=float)

    if ax is None:
        fig, ax = plt.subplots(figsize=(9, 4))

    ax.plot(x, y_median, label=label, color=color, linewidth=3)
    ax.fill_between(x, y_q05, y_q95, color=color, alpha=0.1)
    ax.plot(x, np.ones_like(x), "--", color="black", label="AF Baseline", linewidth=3)

    ax.set_ylabel("AF", fontsize=fontsize)
    ax.set_xlabel("Top (%)", fontsize=fontsize)
    ax.set_xlim([float(np.nanmin(x)), float(np.nanmax(x))])
    ax.set_ylim(bottom=0)
    ax.legend(loc="lower right", fontsize=fontsize, frameon=True)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.xaxis.set_tick_params(labelsize=fontsize)
    ax.yaxis.set_tick_params(labelsize=fontsize)
    ax.set_title("Acceleration Factor Statistics", fontsize=fontsize)

    return ax


# =========================
# Output helpers
# =========================
def save_metrics_row(label, top_percent_curve, ef_curve, af_x, af_y, output_csv):
    """
    Append a metrics row to the summary CSV.

    Parameters:
        label (str): Experiment label.
        top_percent_curve (array-like): TopPercent, per iteration.
        ef_curve (array-like): EF, per iteration.
        af_x (array-like): TopPercent x-grid for AF.
        af_y (array-like): AF values on the x-grid.
        output_csv (str): Output CSV path.
    """
    row = {
        "experiment_label": label,
        "top_percent": json.dumps(list(map(lambda v: None if np.isnan(v) else float(v), np.asarray(top_percent_curve)))),
        "enrichment_factor": json.dumps(list(map(lambda v: None if np.isnan(v) else float(v), np.asarray(ef_curve)))),
        "acceleration_factor": json.dumps([
            list(map(lambda v: None if np.isnan(v) else float(v), np.asarray(af_x))),
            list(map(lambda v: None if np.isnan(v) else float(v), np.asarray(af_y))),
        ]),
    }
    header = not os.path.exists(output_csv)
    pd.DataFrame([row]).to_csv(output_csv, mode="a" if not header else "w", header=header, index=False)


# =========================
# Main CLI
# =========================
def main(dataframe_path, base_path, output_dir="analysis_output"):
    """
    Run analysis pipeline over experiments in a registry CSV.

    Parameters:
        dataframe_path (str): Path to the experiment metadata CSV.
        base_path (str): Base path for dataset CSVs.
        output_dir (str): Directory to save outputs.
    """
    np.random.seed(22)
    os.makedirs(output_dir, exist_ok=True)
    df = strict_load_experiments(dataframe_path, base_path)
    output_csv = os.path.join(output_dir, "metrics_summary.csv")

    for i, row in df.iterrows():
        label = str(row.get("experiment_name", f"exp_{i}")).replace("/", "_").replace(" ", "_")

        # Compute TopPercent stats
        top_df = calcTopPercent(row, base_path)

        # EF relative to random baseline
        N, n_top = load_index_(row, base_path)
        population_size = N
        ef_curve = EF(top_df["median"].values, n_top, population_size)

        # AF on median grid
        af_x, af_y = AF(top_df["median"].values, n_top, population_size, x_grid=top_df["median"].values)

        # Save summary
        save_metrics_row(label, top_df["median"].values, ef_curve, af_x, af_y, output_csv)

    print(f"Saved metrics summary to {output_csv}")

#%%
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run strict analysis pipeline for polymer experiments")
    parser.add_argument("--dataframe-path", required=True, help="Path to the experiment metadata CSV")
    parser.add_argument("--base-path", required=True, help="Base path for data files")
    parser.add_argument("--output-dir", default="analysis_output", help="Directory to save output CSVs and plots")
    args = parser.parse_args()

    main(
        dataframe_path=args.dataframe_path,
        base_path=args.base_path,
        output_dir=args.output_dir,
    )