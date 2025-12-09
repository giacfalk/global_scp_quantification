# Global Systemic Cooling Poverty Quantification

This repository contains the full R workflow used to construct the **Systemic Cooling Poverty Index (SCPI)** and all intermediate indicators for the paper:

**Falchetta, G., Mazzone, A., Bhasin, S., Davide, M., Bezerra, P., Fabbri, K., Bertarelli, G., Pistorio, A., Dal Barco, I., De Cian, E. (2025). “Systemic Cooling Poverty across World Countries.”**  
Manuscript under review at *Nature Sustainability*.  

The code operationalises the multidimensional **Systemic Cooling Poverty (SCP)** framework, generating:
- 17 binary deprivation indicators  
- five aggregated deprivation dimensions  
- a composite **SCPI** at household, regional, and national levels  
- descriptive statistics, maps, tables  
- robustness and uncertainty analyses

---

## Overview

Systemic Cooling Poverty (SCP) describes conditions where people cannot remain thermally safe due to intersecting deficits in:
1. **Climate exposure**
2. **Infrastructure & assets**
3. **Social & thermal inequalities**
4. **Health**
5. **Education & working standards**

Indicator definitions and dimension rules follow the criteria described in the manuscript and its Supplementary Information.

The workflow covers 28 countries (≈1.15M households), integrating DHS surveys, ERA5-Land humid-heat metrics, LCZ maps, urban form, water and sanitation, healthcare accessibility, energy reliability, asset ownership, and national cooling policies.

---

## Repository Structure

### Main driver
- **`SOURCER.R`** — master file to operate the full workflow

### Data processing
- `process_dhs_raw.R` — cleans DHS microdata and harmonises variables.  
- `process_data_combine_dimensions.R` — merges DHS data with climate and geospatial layers.  
- `recover_missing_objects_rst.R` — rebuilds spatial objects if missing.

### Indicator construction
- `define_binary_indicators.R` — main indicator definitions.  
- `define_binary_indicators_sensitivity.R` — alternative definitions.

### Dimension aggregation & SCPI
- `aggregate_indicators.R`  
- `calculate_index.R` — household + national SCPI.  
- `calculate_index_regions.R` — subnational SCPI.  
- `define_dimensions_sensitivity.R`

### Descriptive statistics & inequality
- `deprivation_stats.R`  
- `gini_within_subnat.R`

### Visualisation & tables
- `make_descriptive_plots.R`  
- `make_descriptive_plots_regions.R`  
- `make_radar_plots.R`  
- `biv_descriptive_map.R`  
- `plot_scales.R`  
- `summary_tables.R`  
- `table_indicators.R`

### Sensitivity/uncertainty scripts
- `weighting_sensitivity_regions.R` — Dirichlet-weight Monte Carlo.  
- `sens_analysis_figures_tables.R` — full robustness suite.

---

## Data Requirements

Input datasets are retrivable from Zenodo at XXX. Sources (detailed in the manuscript) include:  
- DHS Household / Women / Men Recode datasets  
- ERA5-Land (1980–2020) wet-bulb temperature  
- Local Climate Zones  
- Google Dynamic World V1  
- Electricity prices and outages  
- Healthcare accessibility  
- National cooling policy inventory

Place inputs under:

```
data_and_sources_dimensions/
```

---

## Installation

Requires **R ≥ 4.0** and package dependencies contained in the script files.

---

## Quick Start

```bash
git clone https://github.com/giacfalk/global_scp_quantification.git
cd global_scp_quantification
```

1. Download all required datasets into `data_and_sources_dimensions/`.  
2. Open `SOURCER.R` and set the correct `stub` path.  
3. Run:

```r
source("SOURCER.R")
```

This builds indicators, dimensions, SCPI, figures, tables, and sensitivity analyses.

---

## Outputs

Running the workflow generates:
- Household, regional, and national SCPI datasets  
- Indicator and dimension statistics  
- Maps, radar plots, bivariate maps  
- Summary tables  
- Sensitivity and uncertainty outputs

---

## License

**CC BY-NC-SA 4.0**

---

## Citation

Falchetta et al. (2025). *Systemic Cooling Poverty across World Countries.*  
Under review at *Nature Sustainability*.

---

## Contact

**Giacomo Falchetta**  
`giacomo.falchetta@cmcc.it`
