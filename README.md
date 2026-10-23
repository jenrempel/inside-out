**Overview:**

This repository contains the analytical code and the final analytical dataset associated with the manuscript: Rempel., J. L.,* Dobbin, K. B.,* Cunniff, A., & Hernandez, A. (2026). The water inside and out: Drinking water data, disparities, and regulatory distinctions among California’s carceral and non-carceral water systems. Environmental Justice. Advance online publication. https://doi.org/10.1177/19394071261482069. *Co-corresponding authors. Authors contributed equally, listed reverse alphabetically.


**Title of dataset:**

The Water Inside and Out


**Authors:**

Jenny Rempel, ORCID: 0000-0003-3916-2405, UC Berkeley, jrempel@berkeley.edu, co-corresponding author

Kristin Dobbin, ORCID: 0000-0001-8499-6850, UC Berkeley, kbdobbin@berkeley.edu, co-corresponding author

Abby Cunniff, ORCID: 0009-0006-8161-5112, UC Santa Barbara, aecunniff@ucsb.edu 

Ariana Hernandez, UCLA, ahern98@g.ucla.edu


**Project summary:**

This repository includes scripts and an analytical dataset describing California water systems categorized as carceral water systems, carceral-serving water systems, or other water systems. The data includes 4,250 California public water systems, including community water systems and non-transient non-community water systems. 

 
**File Directory Map:**

Scripts are listed in the numerical order in which they should be run to reproduce the analysis. Folders are designated for raw data, intermediate data, processed data, and outputs. Intermediate data files created by the authors are included in the intermediate data folder. The final analytical dataset is included in the outputs folder ("outputs/final_analytical_dataset.csv"). The data dictionary is included as "data_dictionary.csv". To reproduce the analysis, (1) clone the repository, (2) download the required raw data and place it in the designated raw_data/ folder or appropriate sub-folders, (3) set up the software environment, and (4) run the scripts in the numerical order in which they are listed.

```text
├── README.md
├── data_dictionary.csv                              # Data dictionary
├── renv.lock
├── renv/  
├── raw_data/                                        # Designated folder for input raw data files from publicly available sources
│   ├── SDWA_latest_downloads/                       # Designated folder for publicly available USEPA ECHO SDWA raw data files
│   ├── ca_boundaries_072325/                        # Designated folder for publicly available dCA Drinking Water System Area Boundaries (CA State Geoportal)
│   ├── ca_institutional_type/                       # Designated folder for publicly available 2023 California Community Water System Institutional Type Update
│   ├── caltrans/                                    # Designated folder for publicly available Adjusted Urban Areas file from CalTrans
│   ├── ddw_acrs/                                    # Designated folder for publicly available Annual Compliance Report datasets from the CA State Water Resources Control Board
│   └── i03_Hydrologic_Regions/                      # Designated folder for publicly available hydrologic region data from the CA Department of Water Resources
├── intermediate_data/                               # Designated folder for intermediate data files (e.g. those created by the authors)
│   ├── updated_HIFLD_list_with_closures.xlsx        # List of nine carceral facilities that the authors identified as having recently closed (see supplementary material table S1 for full list of closed facilities identified by the authors)
│   ├── closest_pws_w_name_assigned_final.xlsx       # List of facilities assigned to water systems based on manual review of publicly available online information (e.g. CCRs) for facilities that did not intersect with any drinking water system boundaries
│   └── multimatch_manual_check_assigned_final.xlsx  # List of facilities assigned to water systems based on manual review of publicly available online information (e.g. CCRs) for facilities that intersected with multiple drinking water system boundaries
├── processed_data/                                  # Designated folder for processed data
├── 01_ca_create_typology.Rmd                        # Script to create water system typology
├── 02_swrcb_acr_outcomes.Rmd                        # Script to clean Annual Compliance Report (ACR) data and join it to key covariates from the US EPA's SDWA SDWIS dataset
├── 03_download_ccrs.Rmd                             # Script to download Consumer Confidence Reports (CCRs)
├── 04_further_data_compilation.Rmd                  # Script to compile data used in the paper analyses
├── 05_main_analysis.Rmd                             # Script to perform primary data analyses
├── 06_population_analysis.Rmd                       # Script to perform additional population analyses
├── 07_rurality_analysis.r                           # Script to perform additional rurality analyses
└── outputs/                                         # Designated folder for generated outputs (e.g. tables, figure)
    └──  final_analytical_dataset.csv                 # Final analytical dataset used in the paper
                                             
```

**Dataset Overview:**
The final analytical dataset for The Water Inside and Out is included as "outputs/final_analytical_dataset.csv". It contains 19 variables (columns) and 4,250 rows (not including column headers). Missing data codes are as listed in the data dictionary. The variable list and definitions are in the included data dictionary ("data_dictionary.csv"). 


**Data availability:**

Input datasets are indicated at the top of each script.


**Requirements:**

This analysis was conducted using R version 4.5.1 (and reproduced in R version 4.6.0). Required packages are loaded at the top of each script. Key packages include: tidyverse, janitor, sf, lubridate, mapview, readxl, pdftools, xlsx, stringr, forcats, gtsummary, kableExtra, ggmap, sp, tigris, jgsbook, ggpubrlib, and nngeo. Software package versioning is noted in the renv files.


**Funding acknowledgment and disclaimer:**

Research reported in this repository was supported by the: Berkeley Fellowship for Graduate Study; the National Science Foundation Graduate Research Fellowship Program under Grant Nos. DGE 1752814 and 2146752; the National Science Foundation Social Behavioral and Economic Sciences (SBE) Postdoctoral Fellowship Program grant number 2104829; and the National Institute of Environmental Health Sciences (NIEHS) Superfund Research Program (P42ES004705). 
 
Any opinions, findings, and conclusions or recommendations expressed in this material are solely those of the authors and do not necessarily reflect the views of the National Science Foundation or other funders. 
 
The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.


**License:**

Code and scripts are licensed under the MIT License: https://opensource.org/license/mit. This final analytical dataset is made available under the Creative Commons Attribution-NonCommercial 4.0 International (CC BY 4.0) License: https://creativecommons.org/licenses/by-nc/4.0/ 
