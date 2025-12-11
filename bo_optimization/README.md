SPOC Active Learning
============================

## Setup

#### Requirements
  * Python == 3.8
  * conda (recommended)

#### Install dependencies

The dependencies can be installed using the following command (conda required):
```bash
bash install.sh
```

Or please check `requirements.txt`.
```bash
pip install -r requirements.txt
```

## Data Files

Place the preprocessed CSV datasets into `./datasets/files`.
The following files are required for the current version:
- `PE_MLtraining_v2,27_LiTFSI_PCB.csv`
- `PE_MLtraining_v2,27_NaTFSI_PCB.csv`

## Run

You can run the experiments with the command below.
```bash
python run.py --config_path ./configs/polymer_rf.yaml --tag TEST
```

Sample configurations can be found in the <a href='./configs'>configs</a> directory.

**NOTE**: You do not need to specify the training directory.<br>
If run as above, a unique identifiable directory will be created in `/usr/workspace/$USER` with `$TAG`.


## Acknowledgment
Many of core functions are borrowed from [this repo](https://github.com/PV-Lab/Benchmarking).

## Citation
```BibTex
@article{schwartz2025spoc,
  title={{Studying-Polymers-On a-Chip (SPOC): High-Throughput Screening of Polymers for Battery Applications}},
  author={Schwartz, Johanna and
          Jimenez, Jayvic and
          Marufu, Michell and
          Silverman, Micah and
          Tzintzun, Santiago and
          Au, Brian and
          Cerda, Robert and
          Igtanloc, Chelsea and
          Rivadeneira Velasco, Katherine and
          Elshatoury, Maged and
          Lau, Chloe and
          Hu, Zeyuan and
          Ojal, Nishant and
          Wood, Marissa and
          Xiao, Yiran and
          Cho, Seongkoo and
          Gongora, Aldair and
          Noh, Junhyug and
          Massey, Travis and
          Marple, Maxwell},
  year={2025}
}
```
