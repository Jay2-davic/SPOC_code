import shutil
import yaml


def load_configs(config_path):
    with open(config_path) as f:
        configs = yaml.load(f, Loader=yaml.FullLoader)
    return configs


def save_configs(configs, save_path):
    shutil.copyfile(configs, save_path)


def save_configs(configs, save_path):
    with open(save_path, 'w') as f:
        yaml.dump(configs, f, default_flow_style=False, sort_keys=False)

