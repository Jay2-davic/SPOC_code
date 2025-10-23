import copy
import numpy as np
import os
import pickle
import time
import pandas as pd

from joblib import Parallel, delayed

from builder import build_dataset, build_model, build_scaler, build_acquisition
from utils.config_util import save_configs
from utils.general_util import log
from pipeline_utils import save_csv  # reuse CSV save with logging

class Engine(object):

    def __init__(self, configs, save_dir, args_dict):
        self.save_dir = save_dir

        # Safely read overridable args. Ensure the keys exist in args_dict when calling Engine.
        acq = args_dict.get("acq", None)
        top_ratio_override = args_dict.get("top_ratio", None)
        self.seeds_jobs = int(args_dict.get("seeds_jobs", -1))  # NEW

        if acq:
            configs["train"]["acquisition"] = acq
        if top_ratio_override is not None:
            configs["train"]["top_ratio"] = top_ratio_override

        self.configs = configs
        save_configs(configs, os.path.join(save_dir, "configs.yaml"))

        np.random.seed(22)
        self.seeds = np.random.choice(
            range(10000), size=configs["train"]["n_seeds"], replace=False
        )
        self.dataset = build_dataset(configs["data"])
        self.scaler = build_scaler(configs["train"]["scaler"])

        self.n_initial = configs["train"]["n_initial"]
        self.acquisition = build_acquisition(configs["train"]["acquisition"])
        self.top_ratio = configs["train"]["top_ratio"]

    def run(self):
        start_time = time.time()

        x_all, y_all = self.dataset.get_data()
        top_indices = self.dataset.get_top_indices(ratio=self.top_ratio)

        outputs = Parallel(n_jobs=self.seeds_jobs)(
            delayed(self.run_once)(s, x_all, y_all, top_indices)
            for s in self.seeds
        )

        total_time = time.time() - start_time
        log.warn("Total elapsed time: {} sec".format(total_time))

        outputs = np.swapaxes(np.array(outputs), 0, 1)
        results = {
            "total_time": total_time,
            "exp_indices": outputs[0],
            "top_counts": ",".join(f"{float(v):.6f}" for v in outputs[1]),
        }

        # --- Unique pickle file naming ---
        yaml_path = self.configs.get("yaml_path", None)
        if yaml_path:
            yaml_base = os.path.splitext(os.path.basename(yaml_path))[0]
            pickle_name = f"results_{yaml_base}.pickle"
        else:
            pickle_name = "results.pickle"

        configs_path = os.path.join(self.save_dir, "configs.yaml")
        results_path = os.path.join(self.save_dir, pickle_name)
        with open(results_path, "wb") as f:
            pickle.dump(results, f)

        # --- Save CSV index row ---
        index_row = {
            "yaml_path": os.path.abspath(yaml_path) if yaml_path else "",
            "configs_path": os.path.abspath(configs_path),
            "results_path": os.path.abspath(results_path),
            "total_time_sec": total_time,
            "n_seeds": len(self.seeds),
            "n_initial": self.n_initial,
            "acquisition": self.configs["train"]["acquisition"],
            "top_ratio": self.top_ratio,
            "scaler": self.configs["train"]["scaler"],
            "model_name": self.configs["model"]["name"],
        }
        index_df = pd.DataFrame([index_row])
        print(index_df)
        per_run_index_csv = os.path.join(self.save_dir, "results_index.csv")
        save_csv(index_df, per_run_index_csv)

    def run_once(self, seed, x_all, y_all, top_indices):
        log.info("Trial #{}/{} (Seed={})".format(
            np.where(self.seeds == seed)[0][0] + 1, len(self.seeds), seed
        ))
        np.random.seed(seed)

        exp_all = list(np.arange(len(self.dataset)))
        # list of candidates we have already observed adding in the initial experiments
        exp_done = np.random.choice(exp_all, size=self.n_initial, replace=False).tolist()
        # pool of candidates to be examined
        exp_rest = list(set(exp_all) - set(exp_done))

        # list to store all observed good candidates' features and target
        x = [x_all[i] for i in exp_done]
        y = [y_all[i] for i in exp_done]
        # list of cumulative number of top candidates found at each learning cycle
        num_tops = [i in top_indices for i in exp_done]
        num_tops = np.cumsum(num_tops).tolist()
        # number of top candidates found so far
        num_top = num_tops[-1]

        model = build_model(
            copy.deepcopy(self.configs["model"]),
            input_dim=len(self.dataset.features)
        )

        for i in np.arange(len(exp_rest)):
            self.scaler.fit(x)
            x_train = self.scaler.transform(x)
            y_train = np.array(y)
            model.train(x_train, y_train)

            # by evaluating acquisition function values at candidates remaining in pool
            # we choose candidate with larger acquisition function value to be observed next
            x_test = self.scaler.transform(x_all[exp_rest])
            mean, std = model.get_mean_std(x_test)
            ac_values = self.acquisition(mean, std, y_best=np.min(y))
            next_exp = exp_rest[np.argmax(ac_values)]

            x.append(x_all[next_exp])
            y.append(y_all[next_exp])

            if next_exp in top_indices:
                num_top += 1

            num_tops.append(num_top)

            exp_rest.remove(next_exp)
            exp_done.append(next_exp)

        return exp_done, num_tops
