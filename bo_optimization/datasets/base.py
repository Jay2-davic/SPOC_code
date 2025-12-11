import math
import os
import pandas as pd
import sys

from sklearn.preprocessing import OneHotEncoder
from utils.general_util import log


class Dataset(object):

    def __init__(self, file_name, target, data_dir="./datasets/files"):
        file_path = os.path.join(data_dir, file_name)
        self.df = pd.read_csv(file_path)
        self.target = target

    def __len__(self):
        return len(self.df)

    @property
    def features(self):
        if not hasattr(self, "_features"):
            self._features = list(self.df.columns[self.df.columns != self.target])
        return self._features

    def preprocess(self):
        log.infov("Preprocess dataset..")
        log.info(" - Shape (Before): {}".format(self.df.shape))
        self.df = self.encode_category(self.df)
        self.df = self.adjust_target(self.df)
        self.df = self.remove_redundancy(self.df)
        log.warn(" - Shape (After) : {}".format(self.df.shape))

    def encode_category(self, df):
        col_types = df.dtypes
        cat_cols = col_types[col_types == "object"].index.tolist()
        num_cols = col_types[col_types != "object"].index.tolist()
        log.info(" - Categorical ({}) / Numerical ({})".format(
            len(cat_cols), len(num_cols)
        ))

        if len(cat_cols) > 0:
            df_cat = df[cat_cols]
            encoder = OneHotEncoder(sparse_output=False, handle_unknown="ignore")
            encoded_data = encoder.fit_transform(df_cat)
            df_encoded = pd.DataFrame(encoded_data, columns=encoder.get_feature_names_out(cat_cols))
            df = df.drop(cat_cols, axis=1)
            df = pd.concat([df, df_encoded], axis=1)
        return df

    def adjust_target(self, df):
        # for P3HT/CNT, Crossed barrel, AutoAM, their original goals were to
        # maximize objective value. Here, we add negative sign to all of its
        # objective values because default BO in the framework below aims
        # for global minimization only P3HT/CNT, Crossed barrel, AutoAM need
        # this line; Perovskite and AgNP do not need this line.
        df[self.target] = -df[self.target].values
        return df

    def remove_redundancy(self, df):
        # For some datasets, each input feature x could have been evaluated
        # more than once. To perform pool-based active learning, we need to
        # group the data by unique input feature x value. For each unique x in
        # design space, we only keep the average of all evaluations there as
        # its objective value.
        df = df.groupby(self.features)[self.target].agg(lambda x: x.unique().mean())
        df = (df.to_frame()).reset_index()
        return df

    def get_top_indices(self, ratio=0.05):
        # number of top candidates
        n_top = int(math.ceil(len(self) * ratio))
        # top candidates and their indicies
        top_indices = self.df.sort_values(self.target).head(n_top).index.to_list()
        return top_indices

    def get_data(self):
        x = self.df[self.features].values
        y = self.df[self.target].values
        return x, y


class CrossedBarrel(Dataset):

    def __init__(self, data_name="CrossedBarrel", target="toughness"):
        self.data_name =  data_name
        file_name = data_name + "_dataset.csv"
        super().__init__(file_name, target)
        self.preprocess()


class PolymerV4(Dataset):

    def __init__(self, material=None, print_type=None, data_name="Polymer_v4",
                 target="ionic_conductivity_final"):
        self.data_name =  data_name
        self.material = material
        self.print_type = print_type
        file_name = data_name + "_dataset.csv"
        super().__init__(file_name, target)
        self.extract_specific_exps()
        if len(self) == 0:
            log.warn("DataFrame is empty. Terminating the job.")
            sys.exit(1)
        self.preprocess()

    def extract_specific_exps(self):
        """
        Extracts rows from the DataFrame for a specific material and print_type.

        Parameters:
        - material (str): The material to filter by (e.g., "LiTFSI" or "NaTFSI").
        - print_type (str): The print type to filter by (e.g., "PCB" or "coin").
        """
        # Validate input parameters
        if (self.material is not None) and (self.material not in self.df['material'].unique()):
            raise ValueError(f"Material '{self.material}' not found in the DataFrame.")
        if (self.print_type is not None) and (self.print_type not in self.df['print_type'].unique()):
            raise ValueError(f"Print type '{self.print_type}' not found in the DataFrame.")

        # Filter the DataFrame
        if self.material is not None:
            self.df = self.df[self.df['material'] == self.material].reset_index(drop=True)

        if self.print_type is not None:
            self.df = self.df[self.df['print_type'] == self.print_type].reset_index(drop=True)
