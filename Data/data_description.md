# Data Description
Development of https://github.com/advayvyas/ForecastEvaluation/issues/10, will have short description of each dataset and then missing values analysis for merged/joined datasets.

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

## state_raw.csv
The joined prediction/truth data with state predictions and local truth.

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

