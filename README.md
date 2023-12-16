Team members: Rachel Sheraden (working alone)
Project type: Analyze an existing data set
Goal: to generate accurate and informative dose-response curves and baseline-adjusted plots from lab data collected in the Greninger Retrovirology lab at Harborview NJB. These plots will be used in a formal talk for the Laboratory Medicine Research Conference in January 2024.
Details: 
    My Master's thesis for Laboratory Medicine is on a novel assay for the detection of HIV-preventative PrEP drug levels in patient blood. The assay generates a fluorescent signal inverse to the amount of drug (high drug=low signal, low drug=high signal). By adding multiple known concentrations of the drug into baseline blood samples, we can generate a standard curve of drug concentration x fluorescent signal that mimics a dose-response curve. The data is currently recorded in a pre-formated Excel template that compiles into a single column-wise spreadsheet from which I can make my plots. 

Packages used: dplyr, ggplot2, tidyr
Data cleaning/wrangling: arrange raw data in clear way R likes, remove any missing data/NAs, clean cloumn titles, etc.
Data analysis: calculate the max and min effective drug concentrations, the IC50 (50% inbihition contration), the linearity, and the limit of detection (LOD) 
Plot(s): 
    1. Continuous dose-response curve from known concentrations
    2. Logarithmic dose-response curve
    3. 4-point logistical regression curve 
    4. Baseline-adjusted logarithmic dose-response
These plots/iterations of these plots will be used for my upcoming research update for Laboratory Medicine Research Conference (LAB M502) at the end of January. 