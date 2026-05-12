# NHANES Cadmium and Blood Pressure Analysis

## Overview

This repository contains a reproducible R workflow for analyzing the association between urinary cadmium and blood pressure among U.S. adults using NHANES 2017–2018 data.

The project demonstrates applied skills in health data science, environmental epidemiology, survey-weighted regression, reproducible R programming, and manuscript-style table and figure generation.

## Research Question

Is urinary cadmium exposure associated with systolic and diastolic blood pressure among U.S. adults?

## Data Source

This project uses publicly available NHANES 2017–2018 data.

Raw NHANES data files are not included in this repository. Users should download the required `.XPT` files from the official NHANES website and place them in the `data_raw/` folder.

Required files:

- `DEMO_J.XPT`
- `BPX_J.XPT`
- `UM_J.XPT`
- `BMX_J.XPT`
- `SMQ_J.XPT`
- `COT_J.XPT`
- `ALB_CR_J.XPT`

## Methods

The analysis includes:

- importing NHANES demographic, examination, laboratory, smoking, urinary biomarker, and survey design data;
- merging datasets by participant identifier;
- restricting the analytic sample to adults aged 20 years or older;
- creating a complete-case analytic dataset;
- log2-transforming urinary cadmium;
- calculating mean systolic and diastolic blood pressure;
- defining the NHANES complex survey design using MEC weights, strata, and primary sampling units;
- fitting survey-weighted linear regression models;
- conducting sensitivity analyses;
- generating manuscript-style tables and figures.

## Models

Primary outcome:

- Systolic blood pressure

Primary exposure:

- Log2-transformed urinary cadmium

Sequential models:

- Model 1: unadjusted
- Model 2: adjusted for age, sex, and race/ethnicity
- Model 3: fully adjusted for age, sex, race/ethnicity, family income-to-poverty ratio, education, BMI, smoking status, log-transformed serum cotinine, and urinary creatinine

Sensitivity analyses:

- Diastolic blood pressure outcome
- Urinary cadmium quartiles
- Unweighted linear regression model

## Key Results

The final complete-case analytic sample included 1,361 adults.

In the fully adjusted survey-weighted model, urinary cadmium was not significantly associated with systolic blood pressure.

Sensitivity analyses using diastolic blood pressure, urinary cadmium quartiles, and an unweighted model showed broadly consistent null findings.

## Repository Structure

```text
.
├── README.md
├── scripts/
│   └── 01_run_full_analysis.R
├── docs/
│   ├── writing_sample.pdf
│   ├── variable_dictionary.md
│   └── methods_summary.md
├── outputs/
│   ├── tables/
│   └── figures/
├── data_raw/
│   └── README.md
└── data_clean/
    └── README.md
