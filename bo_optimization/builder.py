from sklearn import preprocessing

from datasets import base as base_datasets
from models import base as base_models
from utils import eval_util


DATASETS = {
    "crossed_barrel": base_datasets.CrossedBarrel,
    "polymer_v4": base_datasets.PolymerV4,
}

SCALERS = {
    "standard": preprocessing.StandardScaler,
}

MODELS = {
    "random_forest": base_models.RandomForestRegressor,
    "gaussian_process": base_models.GaussianProcessRegressor
}

ACQUISITIONS = {
    "EI": eval_util.EI,
    "LCB": eval_util.LCB,
    "PI": eval_util.PI,
}


def build_dataset(data_config):
    data_name = data_config.pop("name")
    if data_name not in DATASETS:
        raise ValueError("Unknown dataset: {}".format(data_name))
    return DATASETS[data_name](**data_config)


def build_scaler(scaler_name):
    if scaler_name not in SCALERS:
        raise ValueError("Unknown scaler: {}".format(scaler_name))
    return SCALERS[scaler_name]()


def build_model(model_config, **kwargs):
    model_name = model_config["name"]
    if model_name not in MODELS:
        raise ValueError("Unknown model: {}".format(model_name))
    model_params = model_config["params"] if "params" in model_config else {}
    if model_name == "gaussian_process":
        model_params["input_dim"] = kwargs["input_dim"]
    return MODELS[model_name](**model_params)


def build_acquisition(acquisition_name):
    if acquisition_name not in ACQUISITIONS:
        raise ValueError("Unknown acquisition: {}".format(acquisition_name))
    return ACQUISITIONS[acquisition_name]
