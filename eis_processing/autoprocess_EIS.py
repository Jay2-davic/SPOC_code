# autoprocess_EIS.py

#!/usr/bin/env python3
import argparse
import json
import os
import sys
import multiprocessing as mp
import threading
import time
import shutil
import uuid
import queue
from typing import Any, Dict, List, Optional, Tuple

from tqdm.auto import tqdm

# Prefer local import first, with robust fallback
try:
    import autoprocess_functions as apf
    from autoprocess_functions import fit_cycle_from_cycle_obj
except ImportError:
    script_dir = os.path.abspath(os.path.dirname(__file__))
    sys.path.insert(0, script_dir)
    import autoprocess_functions as apf
    from autoprocess_functions import fit_cycle_from_cycle_obj

# Global status queue handle, wired in each worker by the pool initializer
STATUS_QUEUE = None

def atomic_write_json(
    path: str,
    obj: dict,
    retries: int = 8,
    backoff_sec: float = 0.25,
    fallback_path: Optional[str] = None,
) -> bool:
    """
    Try to atomically write JSON to 'path', retrying on Windows lock,
    and fall back to writing a checkpoint file if replace fails.
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
            time.sleep(backoff_sec * (attempt + 1))
        except Exception:
            time.sleep(backoff_sec * (attempt + 1))

    # Fallback: write to checkpoint file
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


def _init_worker(status_queue):
    global STATUS_QUEUE
    STATUS_QUEUE = status_queue

    # Ensure headless mode in workers to avoid GUI imports
    os.environ.setdefault("EIS_HEADLESS", "1")

    # Limit BLAS threads to avoid oversubscription stalls
    os.environ.setdefault("OPENBLAS_NUM_THREADS", "1")
    os.environ.setdefault("MKL_NUM_THREADS", "1")
    os.environ.setdefault("NUMEXPR_NUM_THREADS", "1")

    # Apply default guardrails in workers too
    os.environ.setdefault("EIS_ATTEMPTS", "120")
    os.environ.setdefault("EIS_ROUND_BUDGET_SEC", "120")
    os.environ.setdefault("EIS_MAX_POINTS", "600")

    # Install progress emitter in this worker
    def _emit(event: str, **kw):
        try:
            STATUS_QUEUE.put(("progress", os.getpid(), event, kw))
        except Exception:
            pass
    apf.PROGRESS_EMITTER = _emit


def _format_task_label(sample_id, pin_id, cycle_key):
    return f'{sample_id} | {pin_id} | {cycle_key}'


WORKER_TOTALS: Dict[int, int] = {}

def _drain_status_queue(status_q, worker_bars, pid_to_slot, free_slots):
    """
    Drain any pending worker status events and refresh the fixed lines.
    Handles both start/end and progress events.
    """
    try:
        while True:
            msg = status_q.get_nowait()
            if not msg:
                continue

            if msg[0] == "progress":
                _, pid, event, kw = msg
                if pid not in pid_to_slot:
                    pid_to_slot[pid] = free_slots.pop(0) if free_slots else 0
                slot = pid_to_slot[pid]
                bar = worker_bars[slot]

                if event == "prepare":
                    total = int(kw.get("total_attempts", 0)) or 1
                    WORKER_TOTALS[pid] = total
                    bar.reset(total=total)
                    bar.n = 0
                    # Preserve desc, append total
                    bar.set_description_str(f'{bar.desc} - {total} attempts')
                    bar.refresh()
                elif event == "attempt":
                    cur = int(kw.get("attempt", 0))
                    total = WORKER_TOTALS.get(pid, bar.total or 1)
                    cur = max(0, min(cur, total))
                    bar.n = cur
                    # Update desc with current attempt
                    prefix = bar.desc.split(" - ")[0]
                    bar.set_description_str(f'{prefix} - attempt {cur}/{total}')
                    bar.refresh()
                elif event == "done":
                    total = WORKER_TOTALS.get(pid, bar.total or bar.n)
                    bar.n = total
                    prefix = bar.desc.split(" - ")[0]
                    bar.set_description_str(f'{prefix} - done {total}/{total}')
                    bar.refresh()
                continue

            # Support older start/end messages if used
            if msg[0] in ("start", "end"):
                evt_type, pid, kind, sample_id, pin_id, cycle_key = msg
                if pid not in pid_to_slot:
                    pid_to_slot[pid] = free_slots.pop(0) if free_slots else 0
                slot = pid_to_slot[pid]
                if evt_type == "start":
                    label = _format_task_label(sample_id, pin_id, cycle_key)
                    worker_bars[slot].set_description_str(f'Worker {slot+1}: {label} - preparing')
                    worker_bars[slot].reset(total=1)
                    worker_bars[slot].n = 0
                    worker_bars[slot].refresh()
                else:
                    worker_bars[slot].set_description_str(f'Worker {slot+1}: idle')
                    worker_bars[slot].refresh()
                continue
    except queue.Empty:
        return


DEBUG_DASH = os.getenv("EIS_DASHBOARD_DEBUG", "0") == "1"

def _fit_task(task):
    """
    Worker task. Returns a tuple the parent can merge:
      ("key", sample_id, pin_id, cycle_key, fit_dict or None, error or None)
    """
    kind, sample_id, pin_id, cycle_key, cycle_obj = task

    # Optionally send start message, parent will prefer progress messages
    try:
        if STATUS_QUEUE is not None:
            STATUS_QUEUE.put(("start", os.getpid(), kind, sample_id, pin_id, cycle_key))
        if DEBUG_DASH:
            print(f"[DASH] start pid={os.getpid()} {sample_id} {pin_id} {cycle_key}", file=sys.stderr)
    except Exception:
        pass

    try:
        fit = fit_cycle_from_cycle_obj(cycle_obj)
        return kind, sample_id, pin_id, cycle_key, fit, None
    except Exception as exc:
        return kind, sample_id, pin_id, cycle_key, None, str(exc)
    finally:
        try:
            if STATUS_QUEUE is not None:
                STATUS_QUEUE.put(("end", os.getpid(), kind, sample_id, pin_id, cycle_key))
            if DEBUG_DASH:
                print(f"[DASH] end pid={os.getpid()} {sample_id} {pin_id} {cycle_key}", file=sys.stderr)
        except Exception:
            pass


def build_tasks(eis_results: Dict[str, Any], skip_existing: bool) -> List[Tuple[str, str, str, Any, Dict[str, Any]]]:
    """
    Build worklist of keyed cycles for schema:
      eis_results[sample_id][pin_id]["cycle_N"] = {...}
    """
    tasks_local: List[Tuple[str, str, str, Any, Dict[str, Any]]] = []
    for sample_id, sample in (eis_results or {}).items():
        if not isinstance(sample, dict):
            continue
        for pin_id, pin_entry in sample.items():
            if not isinstance(pin_entry, dict):
                continue
            # Optional pin-level filter can be added here, e.g. pin_entry.get("fitting") == "ok"
            for cycle_key, c in pin_entry.items():
                if not (isinstance(c, dict) and str(cycle_key).lower().startswith("cycle_")):
                    continue
                if skip_existing and str(c.get("fit_status", "")).lower() == "ok":
                    continue
                tasks_local.append(("key", sample_id, pin_id, cycle_key, c))
    return tasks_local


def task_id(t: Tuple[str, str, str, Any, Dict[str, Any]]) -> Tuple[str, str, str, str]:
    kind, sample_id, pin_id, cycle_key, _ = t
    return (kind, sample_id, pin_id, str(cycle_key))


def autoprocess_eis_json(
    json_path: str,
    out_path: Optional[str] = None,
    skip_existing: bool = True,
    checkpoint_interval: int = 50,
    cleanup_intermediate: Optional[str] = None,
    jobs: Optional[int] = None,
    show_progress: bool = True,
    retry_delay_sec: int = 200,
    max_retries: int = 3,
) -> None:
    """
    Multi-pass scheduler:
      - Build cycles
      - Process in passes
      - Defer failures to retry queue
      - Wait retry_delay_sec between passes
      - After max_retries, mark remaining as error retry_exceeded
    """
    with open(json_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    eis_results = data.get("eis_results", {}) or {}

    pending = build_tasks(eis_results, skip_existing=skip_existing)
    if not pending:
        print("[INFO] No cycles require processing, exiting.", file=sys.stderr)
        atomic_write_json(out_path or json_path, data, fallback_path=cleanup_intermediate)
        delete_if_exists(cleanup_intermediate)
        return

    retry_count: Dict[Tuple[str, str, str, str], int] = {}
    pass_num = 0

    while pending and pass_num < max_retries:
        pass_num += 1
        print(f"[INFO] Pass {pass_num}/{max_retries}, cycles={len(pending)}", file=sys.stderr)

        # Resolve jobs
        if jobs is None:
            env_jobs = os.getenv("EIS_AUTOPROC_JOBS")
            jobs_local = int(env_jobs) if env_jobs else (os.cpu_count() or 1)
        else:
            jobs_local = jobs
        jobs_local = max(1, int(jobs_local))
        
        try:
            # Sort by estimated cost: number of points in 'freq' or the first values list
            pending.sort(
                key=lambda t: len(
                    (
                        (t[4].get("data_array_values") or [[]])[0]
                        or (t[4].get("values") or [[]])[0]
                    )
                ),
                reverse=True,
            )
        except Exception:
            pass

        # Dashboard bars
        pbar: Optional[tqdm] = None
        worker_bars: List[tqdm] = []
        pid_to_slot: Dict[int, int] = {}
        free_slots: List[int] = []

        if show_progress:
            pbar = tqdm(
                total=len(pending),
                desc=f"Fitting cycles - pass {pass_num}",
                unit="cycle",
                position=0,
                leave=True,
                dynamic_ncols=True
            )
            for i in range(jobs_local):
                wb = tqdm(total=1, position=i + 1, leave=True, dynamic_ncols=True, bar_format='{desc}')
                wb.set_description_str(f'Worker {i+1}: idle')
                worker_bars.append(wb)
            free_slots = list(range(len(worker_bars)))

        processed = 0
        deferred = 0
        next_pending: List[Tuple[str, str, str, Any, Dict[str, Any]]] = []

        if jobs_local == 1:
            # Sequential
            for i, t in enumerate(pending):
                kind, sample_id, pin_id, cycle_key, cycle_obj = t
                if show_progress and worker_bars:
                    label = _format_task_label(sample_id, pin_id, cycle_key)
                    worker_bars[0].set_description_str(f'Worker 1: {label} - preparing')
                    worker_bars[0].reset(total=1)
                    worker_bars[0].n = 0
                    worker_bars[0].refresh()

                kind, sample_id, pin_id, cycle_key, fit, err = _fit_task(t)

                if fit:
                    data["eis_results"][sample_id][pin_id][cycle_key].update(fit)
                    processed += 1
                else:
                    # Defer problematic cycle
                    next_pending.append(t)
                    tk = task_id(t)
                    retry_count[tk] = retry_count.get(tk, 0) + 1
                    data["eis_results"][sample_id][pin_id][cycle_key]["fit_status"] = "error"
                    data["eis_results"][sample_id][pin_id][cycle_key]["fit_error"] = str(err)
                    deferred += 1

                if pbar is not None:
                    pbar.update(1)
                    worker_bars[0].set_description_str('Worker 1: idle')
                    worker_bars[0].refresh()

                if processed > 0 and (processed % checkpoint_interval == 0 or i == len(pending) - 1):
                    atomic_write_json(out_path or json_path, data, fallback_path=cleanup_intermediate)
                    if show_progress:
                        tqdm.write(f"[INFO] Checkpoint saved after {processed} cycles")

        else:
            # Parallel
            print(f"[INFO] Fitting {len(pending)} cycles with {jobs_local} processes", file=sys.stderr)
            ctx = mp.get_context("spawn")
            chunksize = 1

            status_q = ctx.Queue()
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
                processes=jobs_local,
                initializer=_init_worker,
                initargs=(status_q,),
                maxtasksperchild=50
            ) as pool:
                for i, res in enumerate(pool.imap_unordered(_fit_task, pending, chunksize=chunksize)):
                    kind, sample_id, pin_id, cycle_key, fit, err = res

                    if fit:
                        data["eis_results"][sample_id][pin_id][cycle_key].update(fit)
                        processed += 1
                    else:
                        t_defer = ("key", sample_id, pin_id, cycle_key, data["eis_results"][sample_id][pin_id][cycle_key])
                        next_pending.append(t_defer)
                        tk = task_id(t_defer)
                        retry_count[tk] = retry_count.get(tk, 0) + 1
                        data["eis_results"][sample_id][pin_id][cycle_key]["fit_status"] = "error"
                        data["eis_results"][sample_id][pin_id][cycle_key]["fit_error"] = str(err)
                        deferred += 1

                    if pbar is not None:
                        pbar.update(1)

                    if processed > 0 and (processed % checkpoint_interval == 0 or i == len(pending) - 1):
                        atomic_write_json(out_path or json_path, data, fallback_path=cleanup_intermediate)
                        if show_progress:
                            tqdm.write(f"[INFO] Checkpoint saved after {processed} cycles")

            if updater:
                stop_event.set()
                updater.join(timeout=1.0)

        # Final write per pass
        atomic_write_json(out_path or json_path, data, fallback_path=cleanup_intermediate)
        if show_progress:
            for i, wb in enumerate(worker_bars):
                wb.set_description_str(f'Worker {i+1}: idle')
                wb.refresh()
            if pbar is not None:
                pbar.close()
            for wb in worker_bars:
                wb.close()

        print(f"[INFO] pass={pass_num}, processed={processed}, deferred={deferred}, out={out_path or json_path}", file=sys.stderr)

        # If there are deferred cycles and we still have retries left, wait and retry
        if next_pending and pass_num < max_retries:
            print(f"[INFO] Waiting {retry_delay_sec} seconds before retrying {len(next_pending)} cycles", file=sys.stderr)
            time.sleep(retry_delay_sec)
            pending = next_pending
        else:
            pending = next_pending

    # After max_retries, mark remaining as final errors
    if pending:
        print(f"[WARN] {len(pending)} cycles still failing after {max_retries} passes, marking as error", file=sys.stderr)
        for t in pending:
            kind, sample_id, pin_id, cycle_key, cycle_obj = t
            cycle_obj["fit_status"] = "error"
            prev_err = str(cycle_obj.get("fit_error", ""))
            cycle_obj["fit_error"] = f"{prev_err}; retry_exceeded"
        atomic_write_json(out_path or json_path, data, fallback_path=cleanup_intermediate)

    delete_if_exists(cleanup_intermediate)
    print(f"[INFO] done, out={out_path or json_path}")


def _run_status_updater(stop_event, status_q, worker_bars, pid_to_slot, free_slots):
    """
    Continuously drain the status queue and refresh worker status lines.
    Runs until stop_event is set.
    """
    while not stop_event.is_set():
        _drain_status_queue(status_q, worker_bars, pid_to_slot, free_slots)
        time.sleep(0.1)  # 100 ms refresh cadence


def main():
    parser = argparse.ArgumentParser(
        description="Autoprocess EIS JSON, keyed cycles under pin entries, multi-pass with retries, and progress dashboard."
    )
    parser.add_argument("--json", required=True, help="Path to input JSON, updated in place by default")
    parser.add_argument("--out", help="Optional output JSON path, defaults to input")
    parser.add_argument("--no-skip", action="store_true", help="Recompute even if fit_status is ok")
    parser.add_argument("--cleanup", help="Optional checkpoint JSON path to use when target is locked")
    parser.add_argument("--checkpoint", type=int, default=20, help="Checkpoint interval in processed cycles")
    parser.add_argument("--jobs", type=int, help="Processes to use, default CPU count")
    parser.add_argument("--no-progress", action="store_true", help="Disable progress bar")
    parser.add_argument("--retry-delay", type=int, default=200, help="Seconds to wait between retry passes")
    parser.add_argument("--max-retries", type=int, default=3, help="Maximum retry passes for problematic cycles")
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
        retry_delay_sec=args.retry_delay,
        max_retries=args.max_retries,
    )


if __name__ == "__main__":
    main()
