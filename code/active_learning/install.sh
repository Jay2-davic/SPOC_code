#!/bin/bash

conda install pytorch==1.8.0 torchvision==0.9.0 -c pytorch
conda install -c conda-forge jupyterlab

pip install -r requirements.txt
