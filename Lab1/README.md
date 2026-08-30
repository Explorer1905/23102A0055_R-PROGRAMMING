# Air Quality Data Cleaning — R Practical

Cleans the Beijing Multi-Site Air Quality Dataset (Aotizhongxin station) using R.

## What it does
- Imports and inspects the dataset with error handling (tryCatch)
- Differentiates NA, NULL, and NaN
- Summarizes missing values per variable
- Imputes numeric variables using median (loop-based, reusable code)
- Imputes categorical variable (wind direction) using mode
- Validates a derived pollution_ratio variable for NA/NaN/Inf
- Compares missing values before vs after cleaning
- Visualizes missingness with a bar chart
- Exports the cleaned dataset as CSV

## Files
- `air_quality_cleaning.R` — main script
- `PRSA_Data_Aotizhongxin_20130301-20170228.csv` — raw dataset
- `cleaned_air_quality_data.csv` — cleaned output
