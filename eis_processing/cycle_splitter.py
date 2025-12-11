#!/usr/bin/env python3
"""
Cycle splitter with parallel processing and tqdm progress bar.

CLI:
  python eis_processing/cycle_splitter.py <input_meta_json> <output_master_json> --base-dir <BASE_MPR> [--jobs N] [--no-progress]

Meta JSON shape:
{
  "samples": {
    "<sample_id>": {
      "mpr_files": "<path1;path2>" or "<path1,path2>" or "<path>" or ["<path1>", ...],
      "csv_files": "<path1;path2>" or "<path>" or ["<path1>", ...]  # optional
    }
  }
}

Behavior:
- Reads meta JSON.
- Parses each .mpr into a DataFrame using galvani BioLogic.MPRfile.data, or CSV via pandas.
- Cleans column names.
- Labels cycles using explicit cycle_number/cycle/Ns when present, else by frequency resets starting at 1.
- Splits per run:
  { "eis_results": { "<sample_id>": {
      "pin_0001": { "pin": 1, "file_name": "<file1.mpr>", "fitting": "ok|no|missing|invalid",
                    "cycle_1": { "cycle": 1, "data_array_variables": [...], "data_array_values": [...] },
                    "cycle_2": { ... }
      },
      "pin_0002": { "pin": 2, "file_name": "<file2.mpr>", "fitting": "ok|no|missing|invalid", ... }
  } } }
- Removes source_path and fit_status from cycle entries.
- Summary:
  - Prints and saves at top level: "eis_summary" with samples_analyzed, files_processed, cycles_extracted, and status counts.
- Progress bar via tqdm.
- Parallel processing via multiprocessing (Windows safe).
"""

import argparse
import json
import os
import sys
import time
import tempfile
import re
import multiprocessing as mp
from copy import deepcopy
from typing import List, Dict, Optional, Tuple

import warnings
import numpy as np
import pandas as pd

# Progress bar
try:
    from tqdm.auto import tqdm
except Exception:
    tqdm = None

pd.options.mode.chained_assignment = None  # match original behavior

# galvani is required for .mpr parsing
try:
    from galvani import BioLogic  # pip install galvani
except Exception:
    print("ERROR: galvani is required to parse .mpr files. Install with: pip install galvani", file=sys.stderr)
    raise

# ------------- I/O helpers -------------

def atomic_write_json(path: str, obj: dict, retries: int = 8, sleep: float = 0.25) -> None:
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    fd, tmp = tempfile.mkstemp(suffix=".tmp", dir=os.path.dirname(path) or ".")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(obj, f, indent=2)
            f.flush()
            try:
                os.fsync(f.fileno())
            except Exception:
                pass
        for _ in range(retries):
            try:
                os.replace(tmp, path)
                return
            except PermissionError:
                time.sleep(sleep)
        try:
            if os.path.exists(path):
                try:
                    os.remove(path)
                except PermissionError:
                    time.sleep(sleep)
            os.replace(tmp, path)
            return
        except PermissionError:
            raise
    finally:
        try:
            if os.path.exists(tmp):
                os.remove(tmp)
        except Exception:
            pass

def load_json(path: str) -> dict:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)

def load_or_init_master(path: str) -> dict:
    if not os.path.exists(path):
        return {"eis_results": {}}
    return load_json(path)

# ------------- Path normalization -------------

def normalize_path(p: str, base_dir: str) -> str:
    """
    Normalize incoming path strings:
      - Replace non-breaking spaces with regular spaces
      - Collapse multiple whitespace to a single space
      - Resolve relative paths against base_dir
      - Normalize separators
    """
    if not isinstance(p, str):
        return ""
    s = p.replace("\u00A0", " ").replace("\u2007", " ").replace("\u202F", " ")
    s = re.sub(r"\s+", " ", s).strip()
    if not os.path.isabs(s):
        s = os.path.join(base_dir, s)
    s = os.path.expanduser(s)
    s = os.path.normpath(s)
    return s

# ------------- Column cleaning -------------

def sanitize_columns(df: pd.DataFrame) -> pd.DataFrame:
    cols = df.columns.astype(str)
    cols = cols.str.split("/").str[0].str.strip()
    cols = cols.str.replace(" ", "_", regex=False)
    cols = cols.str.replace("|", "", regex=False)
    cols = cols.str.replace(r"\s*\(\s*Z\s*\)\s*", "", regex=True)
    cols = cols.str.replace("-", "", regex=False)
    cols = cols.str.replace("<", "", regex=False)
    cols = cols.str.replace(">", "_meas", regex=False)
    df.columns = cols
    return df

def ensure_derived_columns(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "Re" in out.columns and "Im" in out.columns:
        re_vals = pd.to_numeric(out["Re"], errors="coerce")
        im_vals = pd.to_numeric(out["Im"], errors="coerce")
        if "Z" not in out.columns:
            out["Z"] = np.sqrt(np.square(re_vals.to_numpy()) + np.square(im_vals.to_numpy()))
        if "Phase" not in out.columns:
            out["Phase"] = np.degrees(np.arctan2(im_vals.to_numpy(), re_vals.to_numpy()))
    return out

# ------------- MPR parsing -------------

def MPR_convert(file_name: str) -> pd.DataFrame:
    warnings.simplefilter(action="ignore", category=FutureWarning)
    file_conversion = BioLogic.MPRfile(file_name)
    df = pd.DataFrame(file_conversion.data)
    df = sanitize_columns(df)
    return df

def _pick_freq_col(df: pd.DataFrame) -> str:
    for name in ("freq", "Frequency", "frequency", "Freq"):
        if name in df.columns:
            return name
    raise RuntimeError("EIS DataFrame missing a frequency column (freq/Frequency)")

# ------------- Cycle labeling -------------

def label_cycles_by_freq_reset(df: pd.DataFrame, tol_ratio: float = 0.01) -> pd.DataFrame:
    freq_col = _pick_freq_col(df)
    f = pd.to_numeric(df[freq_col], errors="coerce").to_numpy()
    n = f.size
    if n == 0 or np.isfinite(f).sum() < 5:
        df["cycle"] = 1
        return df

    fmax = float(np.nanmax(f))
    tol = fmax * float(tol_ratio)

    cycle = np.empty(n, dtype=np.int64)
    c = 1
    cycle[0] = c
    prev = f[0]

    for i in range(1, n):
        fi = f[i]
        if not np.isfinite(fi) or not np.isfinite(prev):
            cycle[i] = c
            prev = fi
            continue

        reset_up_jump = fi > prev + tol
        near_max = (abs(fi - fmax) <= tol) and (prev < (fmax - tol))

        if reset_up_jump or near_max:
            c += 1

        cycle[i] = c
        prev = fi

    df["cycle"] = cycle
    return df

def label_cycles(df: pd.DataFrame) -> pd.DataFrame:
    for col in ("cycle_number", "cycle", "Cycle", "Ns"):
        if col in df.columns:
            c = pd.to_numeric(df[col], errors="coerce").fillna(method="ffill").fillna(method="bfill").astype(int)
            if c.min() == 0:
                c = c + 1
            df["cycle"] = c
            return df
    df = label_cycles_by_freq_reset(df, tol_ratio=0.01)
    return df

# ------------- CSV parsing -------------

def parse_csv(file_path: str) -> pd.DataFrame:
    df = pd.read_csv(file_path)
    if df.empty:
        raise RuntimeError("CSV contained no rows")
    df = sanitize_columns(df)
    return df

# ------------- Cycle building -------------

def _to_values(series: pd.Series) -> list:
    if pd.api.types.is_numeric_dtype(series):
        return pd.to_numeric(series, errors="coerce").tolist()
    return series.astype(str).tolist()

def build_cycles_from_df(df: pd.DataFrame) -> List[Dict]:
    df = ensure_derived_columns(df)
    df = label_cycles(df)

    noisy_cols = [c for c in df.columns if c.lower() == "index" or c.lower().startswith("unnamed:")]
    if noisy_cols:
        df = df.drop(columns=noisy_cols)

    cycles: List[Dict] = []
    for cycle_id, g in df.groupby("cycle", sort=True):
        variables = list(g.columns)
        if "cycle" in variables:
            variables.remove("cycle")
        values = [_to_values(g[c]) for c in variables]
        cycles.append({
            "cycle": int(cycle_id),
            "data_array_variables": variables,
            "data_array_values": values,
        })

    if not cycles:
        raise RuntimeError("No cycles found after labeling")
    return cycles

# ------------- Meta parsing -------------

def split_paths_field(field) -> List[str]:
    """
    Normalize a meta field that may be:
      - a list of paths
      - a single path string
      - a single string containing multiple paths separated by comma or semicolon
    """
    paths: List[str] = []
    if isinstance(field, list):
        paths.extend([p for p in field if isinstance(p, str) and p.strip()])
    elif isinstance(field, str) and field.strip():
        # Split on comma or semicolon, then trim
        parts = re.split(r"[;,]", field)
        for p in parts:
            p = p.strip()
            if p:
                paths.append(p)
    return paths

def parse_inputs_from_meta(meta_json_path: str, base_dir: str) -> List[Dict[str, str]]:
    """
    Return tasks with run indexing:
      { sample_id, pin, file_name, full_path, exists, ext }
    pin starts at 1 per sample and increments per path in declaration order.
    """
    meta = load_json(meta_json_path)
    samples = meta.get("samples") or {}
    tasks: List[Dict[str, str]] = []

    for sample_id, info in samples.items():
        if not isinstance(info, dict):
            continue

        mpr_paths = split_paths_field(info.get("mpr_files"))
        csv_paths = split_paths_field(info.get("csv_files"))

        pin_idx = 1
        for p in mpr_paths + csv_paths:
            full_path = normalize_path(p, base_dir)
            file_name = os.path.basename(full_path)
            ext = os.path.splitext(full_path)[1].lower()
            exists = os.path.exists(full_path)
            tasks.append({
                "sample_id": sample_id,
                "pin": pin_idx,
                "file_name": file_name,
                "full_path": full_path,
                "exists": exists,
                "ext": ext,
            })
            pin_idx += 1

    return tasks

# ------------- Append helpers -------------

def ensure_pin_entry(master: dict, sample_id: str, pin: int) -> Dict:
    eis = master.setdefault("eis_results", {})
    entry = eis.setdefault(sample_id, {})
    key = f"pin_{pin:04d}"
    fentry = entry.setdefault(key, {})
    # Ensure required fields
    fentry.setdefault("pin", pin)
    return fentry

def set_fitting(master: dict, sample_id: str, pin: int, status: str, file_name: Optional[str] = None) -> None:
    fentry = ensure_pin_entry(master, sample_id, pin)
    if file_name is not None:
        fentry["file_name"] = file_name
    fentry["fitting"] = status

def append_cycles_inplace(master: dict, sample_id: str, pin: int, file_name: str, cycles: List[Dict]) -> None:
    """
    Store cycles keyed by cycle number under pin key.
    Keep only cycle and arrays.
    """
    eis = master.setdefault("eis_results", {})
    entry = eis.setdefault(sample_id, {})
    pin_key = f"pin_{pin:04d}"
    fentry: Dict[str, Dict] = entry.get(pin_key, {})
    fentry["pin"] = pin
    fentry["file_name"] = file_name

    # Build deterministic cycle keys using top-level 'cycle', no zero padding per request
    new_fentry: Dict[str, Dict] = {k: v for k, v in fentry.items() if not str(k).startswith("cycle_")}
    for c in cycles:
        cycle_num = int(c.get("cycle", 0))
        key = f"cycle_{cycle_num if cycle_num > 0 else len(new_fentry) + 1}"
        new_fentry[key] = {
            "cycle": cycle_num,
            "data_array_variables": deepcopy(c.get("data_array_variables", [])),
            "data_array_values": deepcopy(c.get("data_array_values", [])),
        }

    entry[pin_key] = new_fentry

# ------------- Workers -------------

def _split_task(task: Dict[str, str]) -> Tuple[str, int, str, Optional[List[Dict]], Optional[str]]:
    try:
        ext = task["ext"]
        if ext == ".mpr":
            cycles = build_cycles_from_df(MPR_convert(task["full_path"]))
        elif ext == ".csv":
            cycles = build_cycles_from_df(parse_csv(task["full_path"]))
        else:
            raise RuntimeError(f"Unsupported extension: {ext}. Provide .mpr or .csv")
        return task["sample_id"], int(task["pin"]), task["file_name"], cycles, None
    except Exception as e:
        return task["sample_id"], int(task["pin"]), task["file_name"], None, f"{type(e).__name__}: {e}"

# ------------- Main orchestration -------------

def run_splitter(meta_json: str,
                 master_json: str,
                 base_dir: str,
                 jobs: Optional[int] = None,
                 use_list: bool = True,  # ignored, kept for CLI compatibility
                 show_progress: bool = True) -> None:
    start_ts = time.time()
    master = load_or_init_master(master_json)

    items = parse_inputs_from_meta(meta_json, base_dir)
    if not items:
        summary = {
            "samples_analyzed": 0,
            "files_processed": 0,
            "cycles_extracted": 0,
            "status_counts": {"ok": 0, "no": 0, "missing": 0, "invalid": 0},
            "run_started_epoch": int(start_ts),
            "run_finished_epoch": int(time.time()),
        }
        master["eis_summary"] = summary
        atomic_write_json(master_json, master)
        print("Summary: samples analyzed=0, files processed=0, cycles extracted=0")
        _print_json_structure_template()
        print("[INFO] No EIS files found in meta JSON", file=sys.stderr)
        return

    # Metrics
    processed_samples: set[str] = set()
    files_processed = 0
    cycles_extracted = 0
    status_counts = {"ok": 0, "no": 0, "missing": 0, "invalid": 0}

    def is_finished(sample_id: str, pin: int, file_name: str) -> bool:
        eis = master.get("eis_results", {})
        entry = eis.get(sample_id, {})
        pin_key = f"pin_{pin:04d}"
        fentry = entry.get(pin_key)
        if not isinstance(fentry, dict):
            return False
        if fentry.get("file_name") != file_name:
            return False
        if any(str(k).startswith("cycle_") for k in fentry.keys()):
            return True
        return False

    processable: List[Dict[str, str]] = []

    # First pass: classify and set fitting for missing or invalid
    for it in items:
        sample_id = it["sample_id"]
        pin = int(it["pin"])
        file_name = it["file_name"]
        full_path = it["full_path"]
        exists = it["exists"]
        ext = it["ext"]

        processed_samples.add(sample_id)

        if not exists:
            set_fitting(master, sample_id, pin, "missing", file_name=file_name)
            status_counts["missing"] += 1
            continue

        if ext not in (".mpr", ".csv"):
            set_fitting(master, sample_id, pin, "invalid", file_name=file_name)
            status_counts["invalid"] += 1
            continue

        if is_finished(sample_id, pin, file_name):
            set_fitting(master, sample_id, pin, "ok", file_name=file_name)
            status_counts["ok"] += 1
            continue

        processable.append(it)

    if not processable:
        atomic_write_json(master_json, master)
        finish_ts = time.time()
        summary = {
            "samples_analyzed": len(processed_samples),
            "files_processed": files_processed,
            "cycles_extracted": cycles_extracted,
            "status_counts": status_counts,
            "run_started_epoch": int(start_ts),
            "run_finished_epoch": int(finish_ts),
        }
        master["eis_summary"] = summary
        atomic_write_json(master_json, master)
        print(f"Summary: samples analyzed={summary['samples_analyzed']}, files processed={summary['files_processed']}, cycles extracted={summary['cycles_extracted']}")
        _print_json_structure_template()
        print("[INFO] All files already split or non-processable.", file=sys.stderr)
        return

    if jobs is None:
        env_jobs = os.getenv("EIS_SPLIT_JOBS")
        jobs = int(env_jobs) if env_jobs else (os.cpu_count() or 1)
    jobs = max(1, int(jobs))

    pbar = None
    if tqdm is not None and show_progress:
        pbar = tqdm(total=len(processable), desc="Splitting EIS files", unit="file", leave=False)

    checkpoint_interval = 10
    processed_since_flush = 0

    if jobs == 1:
        for t in processable:
            sample_id, pin, file_name, cycles, err = _split_task(t)
            if err:
                set_fitting(master, sample_id, pin, "invalid", file_name=file_name)
                status_counts["invalid"] += 1
                print(f"[WARN] Parser failed for {file_name}: {err}", file=sys.stderr)
            else:
                append_cycles_inplace(master, sample_id, pin, file_name, cycles)
                set_fitting(master, sample_id, pin, "ok", file_name=file_name)
                files_processed += 1
                cycles_extracted += len(cycles)
                status_counts["ok"] += 1

            if pbar is not None:
                pbar.update(1)

            processed_since_flush += 1
            if processed_since_flush % checkpoint_interval == 0:
                atomic_write_json(master_json, master)

        atomic_write_json(master_json, master)

    else:
        ctx = mp.get_context("spawn")  # Windows safe
        chunksize = max(1, len(processable) // (jobs * 4))
        with ctx.Pool(processes=jobs) as pool:
            for sample_id, pin, file_name, cycles, err in pool.imap_unordered(_split_task, processable, chunksize=chunksize):
                if err:
                    set_fitting(master, sample_id, pin, "invalid", file_name=file_name)
                    status_counts["invalid"] += 1
                    print(f"[WARN] Parser failed for {file_name}: {err}", file=sys.stderr)
                else:
                    append_cycles_inplace(master, sample_id, pin, file_name, cycles)
                    set_fitting(master, sample_id, pin, "ok", file_name=file_name)
                    files_processed += 1
                    cycles_extracted += len(cycles)
                    status_counts["ok"] += 1

                if pbar is not None:
                    pbar.update(1)

                processed_since_flush += 1
                if processed_since_flush % checkpoint_interval == 0:
                    atomic_write_json(master_json, master)

        atomic_write_json(master_json, master)

    if pbar is not None:
        pbar.close()

    # Final pass: mark "no" for existing supported pins with no cycles
    for it in items:
        if it["exists"] and it["ext"] in (".mpr", ".csv"):
            fentry = ensure_pin_entry(master, it["sample_id"], int(it["pin"]))
            has_cycles = any(str(k).startswith("cycle_") for k in fentry.keys())
            if not has_cycles and fentry.get("fitting") not in ("missing", "invalid"):
                set_fitting(master, it["sample_id"], int(it["pin"]), "no", file_name=it["file_name"])
                status_counts["no"] += 1

    finish_ts = time.time()
    summary = {
        "samples_analyzed": len(processed_samples),
        "files_processed": files_processed,
        "cycles_extracted": cycles_extracted,
        "status_counts": status_counts,
        "run_started_epoch": int(start_ts),
        "run_finished_epoch": int(finish_ts),
    }
    master["eis_summary"] = summary
    atomic_write_json(master_json, master)

    print(f"Summary: samples analyzed={summary['samples_analyzed']}, files processed={summary['files_processed']}, cycles extracted={summary['cycles_extracted']}")
    print(f"Status counts: ok={status_counts['ok']}, no={status_counts['no']}, missing={status_counts['missing']}, invalid={status_counts['invalid']}")
    _print_json_structure_template()

def _print_json_structure_template() -> None:
    """
    Print a copyable JSON structure template that matches the refactored format.
    """
    template = {
        "eis_results": {
            "<sample_id>": {
                "pin_0001": {
                    "pin": 1,
                    "file_name": "file1.mpr",
                    "cycle_1": {
                        "cycle": 1,
                        "data_array_variables": ["freq", "Re", "Im", "Z", "Phase"],
                        "data_array_values": [
                            [100000.0, 90000.0, 80000.0],
                            [0.123, 0.140, 0.160],
                            [-0.045, -0.050, -0.055],
                            [0.131, 0.148, 0.170],
                            [-19.7, -20.1, -20.3]
                        ]
                    },
                    "cycle_2": {
                        "cycle": 2,
                        "data_array_variables": ["freq", "Re", "Im", "Z", "Phase"],
                        "data_array_values": [
                            [100000.0, 90000.0, 80000.0],
                            [0.120, 0.137, 0.155],
                            [-0.043, -0.049, -0.053],
                            [0.127, 0.145, 0.164],
                            [-19.6, -20.0, -20.2]
                        ]
                    },
                    "fitting": "ok"
                },
                "pin_0002": {
                    "pin": 2,
                    "file_name": "file2.mpr",
                    "cycle_1": { "cycle": 1, "data_array_variables": [], "data_array_values": [] },
                    "fitting": "no"
                }
            }
        },
        "eis_summary": {
            "samples_analyzed": 3,
            "files_processed": 5,
            "cycles_extracted": 128,
            "status_counts": { "ok": 5, "no": 1, "missing": 2, "invalid": 0 },
            "run_started_epoch": 1733968123,
            "run_finished_epoch": 1733968187
        }
    }
    print("\nJSON structure template:")
    print(json.dumps(template, indent=2))

def main() -> None:
    parser = argparse.ArgumentParser(description="Split raw EIS files (.mpr or pre-split .csv) into cycles and append into master JSON.")
    parser.add_argument("input_meta_json", help="Meta JSON listing EIS files per sample")
    parser.add_argument("output_master_json", help="Master JSON to append cycles into")
    parser.add_argument("--base-dir", required=True, help="Base directory of raw EIS files")
    parser.add_argument("--jobs", type=int, help="Processes to use, default is max CPUs")
    parser.add_argument("--no-progress", action="store_true", help="Disable progress bar")
    args = parser.parse_args()

    mp.freeze_support()
    run_splitter(
        meta_json=args.input_meta_json,
        master_json=args.output_master_json,
        base_dir=args.base_dir,
        jobs=args.jobs,
        use_list=True,               # ignored, kept for compatibility
        show_progress=not args.no_progress,
    )

if __name__ == "__main__":
    main()
