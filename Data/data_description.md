# Data Description
The corresponding issue is https://github.com/advayvyas/ForecastEvaluation/issues/10. This markdown file is meant to have short descriptions of each dataset and a missing values analysis for merged/joined datasets. 

_If investigating the datasets, the prediction values are either "state_value" (state_raw) or "value" (raw_data) with the truth values always being "oracle_value" with a corresponding "output_type" (quantile) and "output_type_id" (quantile level)._

<img width="660" height="410" alt="image" src="https://github.com/user-attachments/assets/935c7de6-f045-435b-972f-6f59d430cb2e" />

Initial submission status figure from https://github.com/advayvyas/ForecastEvaluation/issues/10#issuecomment-4709391465 by Dr. Kim.

# Datasets
## locations.csv
A list of locations and states along with other geographical information.

## lookup.csv
A lookup table that matches locations to states (with the assigned state being "state" if the location is a state already).

## oracle-output.csv
The truth data downloaded from the FluSight Hub.

## predictions.csv
The prediction data acquired from the FluSight hub.

## raw_data.csv
The joined prediction/truth data.

**NA values: 48852 of 756036 (~6.46%) missing due to projected data into the future (when processed) where truth values had yet not been saved. The date range of missing values was specifically 5/30/26 - 6/13/26, spread out amongst models and locations decently uniformly.**

## state_raw.csv
The joined prediction/truth data with state predictions and local truth. New York values are removed from this dataset, per project assumptions/directions.
         
**NA values: Missing prediction data from NAU-INFLAenza model on the date 12/20/25 for the locations of south-carolina and north-carolina. Additionally, there is missing predictions from the models UT-GBQR, epiENGAGE-GBQR, epiENGAGE-baseline, epiENGAGE-ensemble_mean, and epiENGAGE-lop_norm on the date 11/22/25 for location north-carolina. NA investigation in issue https://github.com/advayvyas/ForecastEvaluation/issues/3**.

## standard-scoring
### standard_coverage.csv
The same as raw_data but with a coverage binary value attached.
### standard_coverage_summary.csv
A summary of the previous file, by horizon and model.
### standard_wis.csv
The same as raw_data but with a WIS value attached.
### standard_wis_summary.csv
A summary of the previous file, by horizon and model.

## state-scoring
### state_coverage.csv
The same as state_raw but with a coverage binary value attached.
### state_coverage_summary.csv
A summary of the previous file, by horizon and model.
### state_wis.csv
The same as state_raw but with a WIS value attached.
### state_wis_summary.csv
A summary of the previous file, by horizon and model.

