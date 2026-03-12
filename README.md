# paper-moe-analysis

# paper-moe-analysis

Statistical analysis of modulus of elasticity (MOE) measurements in managed and unmanaged *Pinus nigra* stands using destructive and non-destructive methods.

## Repository structure

- `data/raw/data.csv` — canonical dataset used in the analysis
- `scripts/MOE_analysis.R` — main R analysis script
- `outputs/tables/` — exported summary tables and model results
- `outputs/figures/` — exported plots
- `report/` — notes and manuscript-related files

## Reproducibility

The analysis can be reproduced by running:

```r
source("scripts/MOE_analysis.R")
