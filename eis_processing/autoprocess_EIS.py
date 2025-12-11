#autoprocess_EIS.py

import argparse
import json
import os
import sys
import multiprocessing as mp
import threading
import re
import time
from typing import Any, Dict, List, Optional, Tuple, Set
import queue
import uuid
from tqdm.auto import tqdm

# Local import for your existing fit function
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from autoprocess_functions import fit_cycle_from_cycle_obj  # or wherever it lives

STATUS_QUEUE = None

def _run_status_updater(stop_event, status_q, worker_bars, pid_to_slot, free_slots):
    """
    Continuously drain the status queue and refresh worker status lines.
    Runs until stop_event is set.
    """
    while not stop_event.is_set():
        _drain_status_queue(status_q, worker_bars, pid_to_slot, free_slots)
        time.sleep(0.1)  # 100 ms refresh cadence

def _init_worker(status_queue):
    """
    Called in each worker process. Wires a global status queue and
    limits BLAS threads to avoid oversubscription stalls.
    """
    global STATUS_QUEUE
    STATUS_QUEUE = status_queue

    # Thread limiting, helps prevent stalls with numpy/scipy BLAS
    os.environ.setdefault("OPENBLAS_NUM_THREADS", "1")
    os.environ.setdefault("MKL_NUM_THREADS", "1")
    os.environ.setdefault("NUMEXPR_NUM_THREADS", "1")


def atomic_write_json(path: str, obj: dict, retries: int = 8, backoff_sec: float = 0.25, fallback_path: Optional[str] = None) -> bool:
    """
    Try to atomically write JSON to 'path', retrying on Windows lock.
    Fall back to writing a checkpoint file if replace fails.
    Returns True on success, False if only fallback succeeded.
    """
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    tmp = f"{path}.{uuid.uuid4().hex}.tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(obj, f, indent=2)
        f.flush()
        try:
            os.fsync(f.fileno())
        except Exception:
            pass

    for attempt in range(retries):
        try:
            os.replace(tmp, path)
            return True
        except PermissionError:
            # Target locked, wait and retry
            time.sleep(backoff_sec * (attempt + 1))
        except Exception:
            time.sleep(backoff_sec * (attempt + 1))

    # Fallback: write to a checkpoint path if provided, else side-by-side .checkpoint
    try:
        cp = fallback_path or f"{path}.checkpoint"
        os.makedirs(os.path.dirname(cp) or ".", exist_ok=True)
        try:
            shutil.copyfile(tmp, cp)
        except Exception:
            os.replace(tmp, cp)
        print(f"[WARN] Atomic replace failed for {path}, wrote checkpoint to {cp}", file=sys.stderr)
    finally:
        try:
            os.remove(tmp)
        except Exception:
            pass
    return False

def delete_if_exists(path: Optional[str]) -> None:
    try:
        if path and os.path.exists(path):
            os.remove(path)
            print(f"[INFO] Removed intermediate file {path}", file=sys.stderr)
    except Exception as exc:
        print(f"[WARN] Could not remove {path}: {exc}", file=sys.stderr)


def build_cycle_index_filter(spec: Optional[str]) -> Optional[Set[int]]:
    """
    Accepts:
      - None or "all" -> no filter
      - "1" -> single index
      - "1,3,5" -> set
      - "1-3" -> inclusive range
    """
    if spec is None:
        return None
    s = spec.strip().lower()
    if s in ("", "all"):
        return None
    indices: Set[int] = set()
    for part in s.split(","):
        part = part.strip()
        if not part:
            continue
        if "-" in part:
            a, b = part.split("-", 1)
            a = int(a.strip()); b = int(b.strip())
            lo, hi = (a, b) if a <= b else (b, a)
            indices.update(range(lo, hi + 1))
        else:
            indices.add(int(part))
    return indices

def _parse_cycle_index_from_key(key: Any) -> Optional[int]:
    """
    Parse a cycle number from keys like 'cycle_1', 'cycle 2', 'cycle-3'.
    """
    s = str(key).lower()
    m = re.search(r'\d+', s)
    return int(m.group(0)) if m else None


def _parse_cycle_index_from_key(key: Any) -> Optional[int]:
    """
    Fallback: derive cycle number from key names like 'cycle_1'
    """
    s = str(key).lower()
    for prefix in ("cycle_", "cycle-", "cycle "):
        if s.startswith(prefix):
            try:
                return int(s.replace(prefix, "", 1))
            except ValueError:
                return None
    try:
        return int(s)
    except ValueError:
        return None
      
def _format_task_label(kind, sample_id, fname, pointer):
    # Short label to keep lines readable
    ptr = str(pointer)
    return f'{sample_id} | {fname} | {kind}:{ptr}'

def _drain_status_queue(status_q, worker_bars, pid_to_slot, free_slots):
    """
    Drain pending worker status events and refresh fixed lines.
    Safe against queue empties and proxy EOF.
    """
    while True:
        try:
            event = status_q.get_nowait()
        except queue.Empty:
            break
        except (EOFError, OSError):
            # Manager proxy can close unexpectedly, stop draining
            break
        except Exception:
            # Be defensive, treat any proxy error as empty
            break

        evt_type, pid, kind, sample_id, fname, pointer = event

        # Assign a stable slot for this worker pid
        if pid not in pid_to_slot:
            if free_slots:
                pid_to_slot[pid] = free_slots.pop(0)
            else:
                pid_to_slot[pid] = 0
        slot = pid_to_slot[pid]

        if evt_type == "start":
            label = _format_task_label(kind, sample_id, fname, pointer)
            worker_bars[slot].set_description_str(f'Worker {slot+1}: {label}')
            worker_bars[slot].refresh()
        elif evt_type == "end":
            worker_bars[slot].set_description_str(f'Worker {slot+1}: idle')
            worker_bars[slot].refresh()


DEBUG_DASH = os.getenv("EIS_DASHBOARD_DEBUG") == "1"

def _fit_task(task):
    kind, sample_id, fname, pointer, cycle_obj = task
    try:
        if STATUS_QUEUE is not None:
            STATUS_QUEUE.put(("start", os.getpid(), kind, sample_id, fname, pointer))
        if DEBUG_DASH:
            print(f"[DASH] start pid={os.getpid()} {sample_id} {fname} {pointer}", file=sys.stderr)
    except Exception:
        pass

    try:
        fit = fit_cycle_from_cycle_obj(cycle_obj)
        return kind, sample_id, fname, pointer, fit, None
    except Exception as exc:
        return kind, sample_id, fname, pointer, None, str(exc)
    finally:
        try:
            if STATUS_QUEUE is not None:
                STATUS_QUEUE.put(("end", os.getpid(), kind, sample_id, fname, pointer))
            if DEBUG_DASH:
                print(f"[DASH] end pid={os.getpid()} {sample_id} {fname} {pointer}", file=sys.stderr)
        except Exception:
            pass
def enumerate_tasks(
    eis_results: Dict[str, Any],
    skip_existing: bool,
    cycle_filter: Optional[Set[int]],
    skip_by_pin_status: bool = False,  # default False; you can wire this to a CLI flag
) -> List[Tuple[str, str, str, Any, Dict[str, Any]]]:
    """
    Create tasks across both JSON shapes:
      - Case A: sample -> file -> cycle_data (list of cycles)
      - Case B: sample -> pin -> cycle_N (keyed cycles)
    Returns a list of tuples:
      ("list", sample_id, file_id, idx, cycle_obj) or
      ("key", sample_id, pin_id, cycle_key, cycle_obj)
    """
    tasks: List[Tuple[str, str, str, Any, Dict[str, Any]]] = []

    for sample_id, sample in (eis_results or {}).items():
        if not isinstance(sample, dict):
            continue

        # Iterate nested entries, which may be pins or files
        for id2, entry in sample.items():
            if not isinstance(entry, dict):
                continue

            # Optional pin-level skip, off by default
            if skip_by_pin_status:
                pin_fitting = str(entry.get("fitting", "")).lower()
                if skip_existing and pin_fitting == "ok":
                    continue

            # Case A: list style under 'cycle_data'
            if isinstance(entry.get("cycle_data"), list):
                for idx, c in enumerate(entry["cycle_data"]):
                    if not isinstance(c, dict):
                        continue
                    if skip_existing and str(c.get("fit_status", "")).lower() == "ok":
                        continue
                    cidx = c.get("cycle") or c.get("cycle_index", idx)
                    if cycle_filter is not None and int(cidx) not in cycle_filter:
                        continue
                    tasks.append(("list", sample_id, id2, idx, c))

            # Case B: keyed cycles like 'cycle_1'
            for key, c in entry.items():
                if not isinstance(c, dict):
                    continue
                if not str(key).lower().startswith("cycle_"):
                    continue
                if skip_existing and str(c.get("fit_status", "")).lower() == "ok":
                    continue
                cidx = c.get("cycle") or c.get("cycle_index") or _parse_cycle_index_from_key(key)
                if cycle_filter is not None and (cidx is None or int(cidx) not in cycle_filter):
                    continue
                tasks.append(("key", sample_id, id2, key, c))

    return tasks

def autoprocess_eis_json(
    json_path: str,
    out_path: Optional[str] = None,
    skip_existing: bool = True,
    checkpoint_interval: int = 50,
    cleanup_intermediate: Optional[str] = None,
    jobs: Optional[int] = None,
    show_progress: bool = True,
) -> None:
    """
    Autoprocess all cycles found in the input JSON, writing progress checkpoints and
    a final output JSON. Shows a tqdm progress bar unless disabled.

    Refactored to show a persistent dashboard of worker status lines, indicating
    which file and cycle each process is currently handling.
    """
    with open(json_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    eis_results = data.get("eis_results", {}) or {}

    # Build worklist of cycles
    cycle_filter = build_cycle_index_filter(os.getenv("EIS_AUTOPROC_CYCLE_INDEX"))
    # If you want a CLI flag, parse it and pass through instead of env
    tasks = enumerate_tasks(
        eis_results=eis_results,
        skip_existing=skip_existing,
        cycle_filter=cycle_filter,
        skip_by_pin_status=False,  # leave False to avoid skipping whole pins
    )
    
    # Optional visibility
    print(
        f"[INFO] samples={len(eis_results)}, cycles_to_process={len(tasks)}",
        file=sys.stderr,
    )
    
    if not tasks:
        print("[INFO] No cycles require processing, exiting.", file=sys.stderr)
        atomic_write_json(out_path or json_path, data, fallback_path=cleanup_intermediate)
        if cleanup_intermediate:
            delete_if_exists(cleanup_intermediate)
        if show_progress and pbar is not None:
            pbar.close()
        return

    # Resolve jobs
    if jobs is None:
        env_jobs = os.getenv("EIS_AUTOPROC_JOBS")
        jobs = int(env_jobs) if env_jobs else (os.cpu_count() or 1)
    jobs = max(1, int(jobs))

    # Create dashboard bars
    pbar: Optional[tqdm] = None
    worker_bars: List[tqdm] = []
    pid_to_slot: Dict[int, int] = {}
    free_slots: List[int] = []

    if show_progress:
        # Main progress bar, leave=True to keep the line on screen
        pbar = tqdm(
            total=len(tasks),
            desc="Fitting cycles",
            unit="cycle",
            position=0,
            leave=True,
            dynamic_ncols=True
        )
        # Fixed worker status lines, one per job, leave=True for persistent lines
        n_lines = max(1, jobs)
        for i in range(n_lines):
            wb = tqdm(
                total=1,
                position=i + 1,
                leave=True,
                bar_format='{desc}',
                dynamic_ncols=True
            )
            wb.set_description_str(f'Worker {i+1}: idle')
            worker_bars.append(wb)
        free_slots = list(range(n_lines))

    processed = 0
    failed = 0

    if jobs == 1:
    # Sequential
        for i, t in enumerate(tasks):
            kind, sample_id, fname, pointer, cycle_obj = t
    
            # Show start on the single worker line
            if show_progress and worker_bars:
                label = _format_task_label(kind, sample_id, fname, pointer)
                worker_bars[0].set_description_str(f'Worker 1: {label}')
                worker_bars[0].refresh()
    
            # Execute task
            kind, sample_id, fname, pointer, fit, err = _fit_task(t)
    
            # Merge results
            if fit:
                data["eis_results"][sample_id][fname][pointer].update(fit)
                processed += 1
            else:
                data["eis_results"][sample_id][fname][pointer]["fit_status"] = "error"
                data["eis_results"][sample_id][fname][pointer]["fit_error"] = err
                failed += 1
    
            if show_progress and pbar is not None:
                pbar.update(1)
                worker_bars[0].set_description_str('Worker 1: idle')
                worker_bars[0].refresh()
    
            # Checkpoint
            if processed > 0 and (processed % checkpoint_interval == 0 or i == len(tasks) - 1):
                atomic_write_json(out_path or json_path, data)
                if show_progress:
                    tqdm.write(f"[INFO] Checkpoint saved after {processed} processed cycles to {out_path or json_path}")
    
    else:
        # Parallel
        print(f"[INFO] Fitting {len(tasks)} cycles with {jobs} processes", file=sys.stderr)
        ctx = mp.get_context("spawn")
        chunksize = 1  # responsive
    
        manager = mp.Manager()
        status_q = ctx.SimpleQueue()
    
        # Status updater thread
        stop_event = threading.Event()
        updater = None
        if show_progress:
            updater = threading.Thread(
                target=_run_status_updater,
                args=(stop_event, status_q, worker_bars, pid_to_slot, free_slots),
                daemon=True,
            )
            updater.start()
    
        with ctx.Pool(
            processes=jobs,
            initializer=_init_worker,
            initargs=(status_q,),
            maxtasksperchild=50
        ) as pool:
            for i, res in enumerate(pool.imap_unordered(_fit_task, tasks, chunksize=chunksize)):
                kind, sample_id, fname, pointer, fit, err = res
    
                if fit:
                    data["eis_results"][sample_id][fname][pointer].update(fit)
                    processed += 1
                else:
                    data["eis_results"][sample_id][fname][pointer]["fit_status"] = "error"
                    data["eis_results"][sample_id][fname][pointer]["fit_error"] = err
                    failed += 1
    
                if show_progress and pbar is not None:
                    pbar.update(1)
    
                if processed > 0 and (processed % checkpoint_interval == 0 or i == len(tasks) - 1):
                    atomic_write_json(out_path or json_path, data)
                    if show_progress:
                        tqdm.write(f"[INFO] Checkpoint saved after {processed} processed cycles to {out_path or json_path}")
    
        # Stop updater thread
        if updater:
            stop_event.set()
            updater.join(timeout=1.0)

    checkpoint_target = cleanup_intermediate
    
    # Checkpoints
    atomic_write_json(out_path or json_path, data, fallback_path=checkpoint_target)
    
    # Final write
    ok = atomic_write_json(out_path or json_path, data, fallback_path=checkpoint_target)
    if ok and cleanup_intermediate:
        delete_if_exists(cleanup_intermediate)

    if show_progress:
        # Ensure status lines show idle at end and then close bars
        for i, wb in enumerate(worker_bars):
            wb.set_description_str(f'Worker {i+1}: idle')
            wb.refresh()
        if pbar is not None:
            pbar.close()
        for wb in worker_bars:
            wb.close()

    print(f"[INFO] cycles_total={len(tasks)}, processed={processed}, failed={failed}, out={out_path or json_path}")
    delete_if_exists(cleanup_intermediate)


def main():
    parser = argparse.ArgumentParser(
        description="Autoprocess EIS JSON, keyed cycles under pin entries, with progress bar and parallelization."
    )
    parser.add_argument("--json", required=True, help="Path to input JSON, updated in place by default")
    parser.add_argument("--out", help="Optional output JSON path, defaults to input")
    parser.add_argument("--no-skip", action="store_true", help="Recompute even if fit_status is ok")
    parser.add_argument("--cleanup", help="Optional path of intermediate JSON to delete after success")
    parser.add_argument("--checkpoint", type=int, default=20, help="Checkpoint interval in processed cycles")
    parser.add_argument("--jobs", type=int, help="Processes to use, default CPU count")
    parser.add_argument("--no-progress", action="store_true", help="Disable progress bar")
    args = parser.parse_args()

    mp.freeze_support()

    autoprocess_eis_json(
        json_path=args.json,
        out_path=args.out or args.json,
        skip_existing=not args.no_skip,
        checkpoint_interval=args.checkpoint,
        cleanup_intermediate=args.cleanup,
        jobs=args.jobs,
        show_progress=not args.no_progress,
    )


if __name__ == "__main__":
    main()
