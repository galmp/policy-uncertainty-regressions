# Policy Uncertainty and Industry Returns  
### Empirical Asset Pricing Project (R)

This project investigates whether policy uncertainty indices contain explanatory power over industry-level stock returns using standard time-series regressions from empirical finance.

It is designed both as an academic-style empirical study and as a portfolio project demonstrating applied quantitative finance and econometrics skills.

---

## Project Objective

The purpose of this project is to examine whether policy-related uncertainty measures affect expected stock returns across industries.

Specifically, I test whether:
- Climate Policy Uncertainty (CPU)
- Uncertainty due to Climate Transition (UCT)

have statistically significant explanatory power for the excess returns of the Fama-French 10 industry portfolios.

---

## Data Sources

All datasets are publicly available and commonly used in academic research:

- Fama-French 10 Industry Portfolios (monthly returns)
- Fama-French 3 Factors (Market, SMB, HML)
- Climate Policy Uncertainty (CPU) index
- Uncertainty due to Climate Transition (UCT) index

The raw datasets are stored in the `data/` folder.

---

## Methodology

For each industry portfolio, I estimate time-series regressions of the form:

### Baseline specification (no controls)

Excess Return = Alpha + Beta × Uncertainty + Error

### Extended specification (with controls)

Excess Return = Alpha + Beta × Uncertainty  
+ Market Factor + SMB + HML + Error

Where:
- Excess Return refers to industry portfolio returns minus the risk-free rate  
- Uncertainty is either CPU or UCT  
- Market, SMB, HML are the Fama-French risk factors  

This approach follows standard empirical asset pricing practice.

---
## Results & Interpretation (What to Look For)

The main quantity of interest is **β (beta)** on the uncertainty variable (CPU or UCT).  
It captures how next-month industry excess returns co-move with uncertainty.

### How to read the tables
Each table reports results for each industry under two specifications:

- **No controls**: Excess Return ~ Uncertainty  
- **FF3 controls**: Excess Return ~ Uncertainty + MKT + SMB + HML  

For each industry and model you will see:
- **Beta (β)**: economic direction and magnitude
- **p-value**: statistical significance
- **Significance stars** (if included in your export):  
  `*** p < 0.01`, `** p < 0.05`, `* p < 0.10`

### Interpretation guide
- **β > 0**: higher uncertainty is associated with higher next-month excess returns  
  (consistent with a “risk premium” interpretation, but not causal evidence)
- **β < 0**: higher uncertainty is associated with lower next-month excess returns  
  (consistent with reduced risk appetite / risk-off pricing)
- If results remain significant **after FF3 controls**, the uncertainty measure provides
  additional explanatory power beyond standard risk factors.
---
## Outputs

Running the script produces the following files in the `results/` folder:

- `CPU_table.csv` — formatted regression table (CPU)
- `UCT_table.csv` — formatted regression table (UCT)
- `results.csv` — raw coefficients (baseline model)
- `results_control.csv` — raw coefficients (FF3 controls)

These tables are directly usable in academic reports, papers, or further analysis.

## Limitations

This project is intentionally focused on transparency and replicability rather than causal identification.  
Some limitations include:

- No causal identification strategy (e.g., IV, local projections, natural experiments)  
- Results are sample-period dependent  
- No multiple-testing correction across industries  

These choices are deliberate, as the goal is to demonstrate clean empirical implementation rather than causal claims.

## How to Run the Project

1. Open RStudio  
2. Set the working directory to the project root  
3. Run the script:

```r
source("scripts/Data Analysis.R")


