# Data Documentation

## Overview

This study uses longitudinal data from the **Health and Retirement Study (HRS)** to examine the relationship between caring for aging parents and the risk of marital dissolution among adult caregivers.

Two primary data sources are used:

### 1. HRS Core Survey Data

The HRS Core Survey provides longitudinal information on respondents' demographic characteristics, family relationships, health, employment, and marital status across survey waves.

- **Source:** University of Michigan, Health and Retirement Study
- **Website:** https://hrsdata.isr.umich.edu/
- **Access:** Public-use data available to registered HRS users.

### 2. RAND HRS Longitudinal File

The RAND HRS Longitudinal File provides harmonized variables across survey waves, facilitating longitudinal analysis of respondents' demographic, health, economic, and family characteristics.

- **Source:** RAND Center for the Study of Aging
- **Website:** https://hrsdata.isr.umich.edu/data-products/rand
- **Access:** Available to registered HRS users, subject to applicable HRS conditions of use.

## Data Access Instructions

Researchers seeking to replicate this study must independently obtain the required datasets.

1. Register for an account through the [HRS Data Portal](https://hrsdata.isr.umich.edu/).
2. Download the HRS Core Survey Data used in this study.(Leave Behind core data from 2004 to 2020)
3. Download the corresponding RAND HRS Longitudinal File.(randhrs1992_2020v1 and randhrsfamr1992_2018v2) 
4. Review the accompanying codebooks and documentation.
5. Place the downloaded files in the appropriate local data directory.

## Data Organization

After downloading the required datasets, organize the files according to the structure expected by the replication scripts.


01_Data/
├── 01_raw/
├── 02_processed/
└── 03/final/

The exact file names and directory paths should be adjusted according to the data products and versions used in the study.

## Data Availability and Restrictions

The original HRS data files are not included in this repository.

Researchers must independently obtain the data from the official HRS Data Portal and comply with the applicable HRS Conditions of Use. Data files and individual-level datasets derived from HRS data should not be redistributed through this repository.

## Citation

Researchers using HRS data should follow the official HRS citation guidelines and cite the relevant data products and documentation used in their analyses.

For additional information, visit: https://hrsdata.isr.umich.edu/
