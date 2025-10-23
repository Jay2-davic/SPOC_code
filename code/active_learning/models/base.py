import numpy as np
import sklearn.ensemble
import sklearn.gaussian_process
import sklearn.linear_model
import warnings

from sklearn.gaussian_process.kernels import Matern, RBF, RationalQuadratic, ExpSineSquared


class Regressor(object):

    def __init__(self, **params):
        self.params = params

    def set_params(self, **params):
        self.predictor.set_params(**params)

    def train(self, x_train, y_train):
        # self.set_params(**self.params)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            self.predictor.fit(x_train, y_train)

    def get_mean_std(self, x):
        pass


class RandomForestRegressor(Regressor):

    def __init__(self, **params):
        super().__init__(**params)
        self.predictor = sklearn.ensemble.RandomForestRegressor(**params)

    def get_mean_std(self, x):
        predictions = np.array([tree.predict(x) for tree in self.predictor.estimators_])
        mean = np.mean(predictions, axis=0)
        std = np.std(predictions, axis=0)
        return mean, std

class GaussianProcessRegressor(Regressor):

    kernel_infos = {
        "Matern": {
            "class": Matern,
            "defaults": {"length_scale": 1.0, "nu": 2.5}
        },
        "RBF": {
            "class": RBF,
            "defaults": {"length_scale": 1.0}
        },
        "RQ": {
            "class": RationalQuadratic,
            "defaults": {"length_scale": 1.0, "alpha": 1.0}
        },
        "ESS": {
            "class": ExpSineSquared,
            "defaults": {"length_scale": 1.0, "periodicity": 1.0}
        },
    }

    def __init__(self, **params):
        super().__init__(**params)
        self.optimize_params = params.pop("optimize", {})
        self.kernel_params = params.pop("kernel", {})
        kernel_name = self.kernel_params.pop("name")
        kernel_info = self.kernel_infos[kernel_name]
        kernel_params = {**kernel_info["defaults"], **self.kernel_params}

        self.kernel = kernel_info["class"](**kernel_params)
        self.predictor = sklearn.gaussian_process.GaussianProcessRegressor(
            kernel=self.kernel,
            **self.optimize_params
        )

    def get_mean_std(self, x):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            mean, std = self.predictor.predict(x, return_std=True)
        return mean, std

