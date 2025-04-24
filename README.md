# Medication_errors
The Medication Error Dashboard is a user-friendly R Shiny application designed to improve patient safety and clinical quality by enabling real-time data entry and analysis of medication errors. This tool seamlessly integrates error tracking, visualization, and performance benchmarking using Six Sigma methodology.

# Features
Real-time Data Entry of medication errors via an interactive interface

Classification of errors (A to I)

Calculation of Key Metrics:

Number of Errors

Number of Opportunities (per unique UHID and drug)

Error Rate per Million

Six Sigma Level

Side-by-Side Analysis:

Per patient

Per drug

Across error categories

Visual Analytics:

Bar plots of error categories

Six Sigma score 

# Outputs
Error Rate Table: Per UHID, Drug, Error Type

Dashboard Metrics:

Total Errors

Total Opportunities

Error per Opportunity

Error per Million Opportunities

Approximate Six Sigma

Dynamic Visualizations:

Line charts and bar plots

Comparison by department, time period, or category

# Sample Outputs
![image](https://github.com/user-attachments/assets/1d2b5898-a6f1-4a7b-8205-3241ff35c2fc)

# Technologies Used
R / Shiny for app interface and logic

Tidyverse for data wrangling

ggplot2 for interactive graphs

Google Sheets / CSV for real-time data sync
