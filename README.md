# malaria-Rt

This repository contains the code and data used to produce the results presented in the paper **"Estimating the effective reproduction number of Plasmodium vivax malaria transmission in South Korea"**.

## Data Availability Notice

The original analysis in the paper was conducted using malaria symptom onset data from South Korea, which is confidential and cannot be publicly shared. Instead, this repository provides publicly available malaria confirmed case data from the Korea Disease Control and Prevention Agency (KDCA). While there are differences between symptom onset dates and confirmation dates, the overall results and conclusions remain consistent with those reported in the paper.

## Data Files

- `input/malaria_kdca.csv`: Regional/weekly malaria confirmed case time series (Source: Korea Disease Control and Prevention Agency)
- `input/Gyeonggi_mos_on_off.csv`: Daily time series of malaria vector mosquito presence (on/off) in Gyeonggi region  
- `input/Incheon_mos_on_off.csv`: Daily time series of malaria vector mosquito presence (on/off) in Incheon region
- `input/Gyeonngi_temp.csv`: Daily temperature time series for Gyeonggi region
- `input/Incheon_temp.csv`: Daily temperature time series for Incheon region

## Code Structure

Execute the scripts in the following order:

### Step 1: Serial Interval Sampling
- `SI_backward_Gyeonggi_code.r`: Serial interval sampling for Gyeonggi region (backward)
- `SI_forward_Gyeonggi_code.r`: Serial interval sampling for Gyeonggi region (forward)  
- `SI_backward_Incheon_code.r`: Serial interval sampling for Incheon region (backward)
- `SI_forward_Incheon_code.r`: Serial interval sampling for Incheon region (forward)

### Step 2: Serial Interval Distribution
- `w_dist_backward_Gyeonggi_code.r`: Serial interval distribution for Gyeonggi region (backward)
- `w_dist_forward_Gyeonggi_code.r`: Serial interval distribution for Gyeonggi region (forward)
- `w_dist_backward_Incheon_code.r`: Serial interval distribution for Incheon region (backward)  
- `w_dist_forward_Incheon_code.r`: Serial interval distribution for Incheon region (forward)

### Step 3: IIP Ratio Computation
- `iip_ratio_backward_Gyeonggi_code.r`: Compute short IIP ratio for Gyeonggi region
- `iip_ratio_backward_Incheon_code.r`: Compute short IIP ratio for Incheon region

### Step 4: Reproduction Number Estimation
- `ci_r_case_code_julia.ipynb`: Compute the case reproduction number
- `r_inst_code.r`: Compute the instantaneous reproduction number

### Step 5: Visualization
- `Fig_*.r`, `Table_*.r`: Code for generating figures and tables

## System Requirements

**Storage Warning**: The analysis involves random sampling of over 5 million serial intervals for each region/week, resulting in approximately **13GB** of computational results that will be stored locally.
