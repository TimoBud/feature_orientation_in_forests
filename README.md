# SARF and CARF

This repository contains the R implementation of **Similarity-Aware Random Forests (SARF)** and **Cluster-Aware Random Forests (CARF)** together with the code used to reproduce the simulation studies and benchmark experiments presented in the accompanying paper.

## Repository structure

```
.
├── R/                         # Algorithm implementations
├── data/                      # Generated and benchmark data
├── figures/                   # Generated figures
├── results/                   # Intermediate and final results
└── scripts/
    ├── benchmark/             # Benchmark experiments
    ├── data_creation/         # Synthetic data generation
    ├── demonstration/         # Demonstration experiments
    └── runtime/               # Runtime experiments
```

### `R/`

Contains the implementations of

- Random Forest
- Extremely Randomized Trees
- Rotation Forest
- Random Rotation Forest
- SARF
- CARF

together with helper functions shared across algorithms.

---

## Workflow

The repository is organized into four largely independent workflows.

### 1. Data generation

Generate the synthetic datasets used in the demonstration experiments.

```
scripts/data_creation/

01_create_simple_gaussian_data.R
02_complex_data_generators.R
03_create_complex_data.R
```

The generated datasets are written to

```
data/demonstration/
```

---

### 2. Demonstration experiments

Run the experiments from the demonstration section.

```
scripts/demonstration/

01_run_on_simple_gaussians.R
02_run_on_complex_data.R
03_evaluation_complex_data.R
04_evaluation_simple_gaussians.R
```

Results are written to

```
results/demonstration/
```

---

### 3. Benchmark experiments

Run the OpenML benchmark study.

```
scripts/benchmark/

00_select_tasks.R
01_run_benchmark.R
02_create_result_df.R
03_benchmark_comparison.R
04_statistical_tests.R
05_create_meta_features.R
06_correlation_performance_meta_features.R
```

Results are stored in

```
results/benchmark/
```

---

### 4. Runtime experiments

Measure training and prediction times.

```
scripts/runtime/

01_run_complexity.R
02_run_time_visualization.R
```

Results are written to

```
results/runtime/
```

---

## Running the repository

The scripts are intended to be executed from the repository root.

Typical execution order:

```text
1. scripts/data_creation/*
2. scripts/demonstration/*
3. scripts/benchmark/*
4. scripts/runtime/*
```

The scripts use relative paths throughout and therefore do not require any manual path adjustments.

---

## Requirements

The code was developed in **R (>= 4.x)**.

Required packages include (among others)

- ranger
- mlr3
- OpenML
- tidyverse
- ggplot2
- kableExtra

Additional dependencies can be installed automatically using the package manager of your choice.

---

## Citation

If you use this software, please cite the accompanying publication.