# Global Systemic Cooling Poverty Quantification

[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.17878396-blue)](https://doi.org/10.5281/zenodo.17878396)
[![License: CC BY-NC-SA 4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/)
[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0-blue)](https://www.r-project.org/)

This repository contains the complete replication code for constructing the **Systemic Cooling Poverty Index (SCPI)** as presented in:

> **Falchetta, G., Mazzone, A., Bhasin, S., Davide, M., Bezerra, P., Fabbri, K., Bertarelli, G., Pistorio, A., Dal Barco, I., De Cian, E. (2025).** "Systemic Cooling Poverty across World Countries." *Manuscript under review at Nature Sustainability.*

## 🌡️ Overview

**Systemic Cooling Poverty (SCP)** describes the condition where people cannot maintain thermal safety due to intersecting deprivations across multiple dimensions. Unlike traditional energy poverty metrics that focus solely on access or affordability, SCP captures the systemic nature of cooling vulnerability by integrating:

1. **Climate Exposure** – humid-heat stress and extreme temperature events
2. **Infrastructure & Assets** – housing quality, cooling equipment, electricity access
3. **Social & Thermal Inequalities** – urban heat islands, water/sanitation, building materials
4. **Health** – healthcare accessibility, heat-vulnerable populations
5. **Education & Working Standards** – educational attainment, employment conditions

The SCPI is constructed from **17 binary deprivation indicators** aggregated into **5 dimensions**, covering **28 countries** and approximately **1.15 million households** from DHS surveys integrated with geospatial climate and infrastructure data.

## 📁 Repository Structure

```
global_scp_quantification/
│
├── SOURCER.R                              # Master orchestration script
│
├── Data Processing Scripts
│   ├── process_dhs_raw.R                  # Clean and harmonize DHS microdata
│   ├── process_data_combine_dimensions.R  # Merge DHS with climate/geospatial data
│   └── recover_missing_objects_rst.R      # Rebuild spatial objects if needed
│
├── Indicator Construction
│   ├── define_binary_indicators.r         # Main 17 binary indicators
│   ├── define_binary_indicators_sensitivity.r  # Alternative definitions
│   └── check_dhs_processed.R              # Data validation checks
│
├── Index Calculation
│   ├── aggregate_indicators.R             # Aggregate indicators into dimensions
│   ├── calculate_index.R                  # Compute household & national SCPI
│   ├── calculate_index_regions.R          # Compute subnational SCPI
│   └── define_dimensions_sensitivity.r    # Alternative dimension specifications
│
├── Analysis & Statistics
│   ├── deprivation_stats.R                # Descriptive statistics
│   └── gini_within_subnat.R               # Inequality measures
│
├── Visualization
│   ├── make_descriptive_plots.R           # National-level plots
│   ├── make_descriptive_plots_regions.R   # Subnational-level plots
│   ├── make_radar_plots.R                 # Dimension radar charts
│   ├── biv_descriptive_map.R              # Bivariate choropleth maps
│   └── plot_scales.R                      # Custom plotting scales
│
├── Tables & Reports
│   ├── summary_tables.R                   # Summary statistics tables
│   └── table_indicators.R                 # Indicator definition tables
│
└── Sensitivity Analysis
    ├── weighting_sensitivity_regions.R    # Dirichlet-weight Monte Carlo
    └── sens_analysis_figures_tables.R     # Full robustness analysis suite
```

## 🔧 Installation

### Prerequisites

- **R** ≥ 4.0
- **RStudio** (recommended)
- **8+ GB RAM** recommended

### Clone Repository

```bash
git clone https://github.com/giacfalk/global_scp_quantification.git
cd global_scp_quantification
```

## 📊 Data Requirements

### Input Data Sources

All input datasets must be downloaded from Zenodo: [![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.17878396-blue)](https://doi.org/10.5281/zenodo.17878396)

The workflow integrates multiple data sources:

| Data Type | Source | Description |
|-----------|--------|-------------|
| Household surveys | DHS Program | Household/Women/Men Recode datasets for 28 countries |
| Climate data | ERA5-Land | Wet-bulb temperature (1980-2020) |
| Urban morphology | LCZ/WUDAPT | Local Climate Zones classification |
| Land cover | Google Dynamic World | V1 land cover time series |
| Infrastructure | Various | Electricity prices, outages, water/sanitation |
| Healthcare | Weiss et al. | Travel time to healthcare facilities |
| Policy | UNEP | National cooling policy inventory |

### Data Organization

Place all downloaded data in the following path:

```
./data_and_sources_dimensions/

```

## 🚀 Usage

### Quick Start

1. **Set up data directory**: Place all input data in `data_and_sources_dimensions/`

2. **Configure paths**: Open `SOURCER.R` and update the `stub` variable to your project root:

```r
# In SOURCER.R, line ~5:
stub <- "/path/to/your/global_scp_quantification/"
```

3. **Run the complete workflow**:

```r
source("SOURCER.R")
```

This will execute the entire pipeline sequentially.

### Modular Execution

You can also run individual components:

```r
# Process raw DHS data only
source("process_dhs_raw.R")

# Calculate indicators only
source("define_binary_indicators.r")

# Generate visualizations only
source("make_descriptive_plots.R")
```

## 🔄 Workflow Description

### Phase 1: Data Preparation

1. **`process_dhs_raw.R`**: Cleans and harmonizes DHS microdata across countries, standardizing variable names and coding schemes

2. **`process_data_combine_dimensions.R`**: Spatially joins household locations with:
   - ERA5-Land climate data (humid-heat metrics)
   - LCZ classifications (urban morphology)
   - Dynamic World land cover
   - Healthcare accessibility layers
   - Infrastructure data

### Phase 2: Indicator Construction

3. **`define_binary_indicators.r`**: Creates 17 binary deprivation indicators:
   - Climate: Days above dangerous wet-bulb temperature, heat wave exposure
   - Infrastructure: Lack of electricity, poor housing materials, no cooling devices
   - Social: Urban heat island effect, inadequate water/sanitation
   - Health: Limited healthcare access, presence of vulnerable individuals
   - Education: Low educational attainment, informal employment

### Phase 3: Aggregation

4. **`aggregate_indicators.R`**: Combines binary indicators into 5 dimensions using weighted averaging

5. **`calculate_index.R`**: Computes household-level SCPI using Alkire-Foster methodology

6. **`calculate_index_regions.R`**: Aggregates to subnational and national levels

### Phase 4: Analysis & Visualization

7. **`deprivation_stats.R`**: Calculates descriptive statistics and prevalence rates

8. **`make_descriptive_plots.R`** & **`make_radar_plots.R`**: Generate publication figures

9. **`summary_tables.R`**: Creates formatted tables for manuscript

### Phase 5: Robustness Checks

10. **`weighting_sensitivity_regions.R`**: Monte Carlo simulation with Dirichlet-distributed weights

11. **`sens_analysis_figures_tables.R`**: Comprehensive sensitivity analysis including:
    - Alternative indicator thresholds
    - Different dimension weightings
    - Varying aggregation methods

```

### Key Outputs

- **SCPI datasets**: Household, regional, and national-level indices with all dimension scores
- **Maps**: Choropleth and bivariate maps showing spatial distribution of SCP
- **Figures**: Radar plots, histograms, scatter plots for manuscript
- **Tables**: Formatted summary statistics and indicator definitions
- **Sensitivity results**: Uncertainty bounds and robustness metrics

## 📖 Citation

If you use this code or data, please cite:

```bibtex
@article{falchetta2025systemic,
  title={Systemic Cooling Poverty across World Countries},
  author={Falchetta, Giacomo and Mazzone, Antonella and Bhasin, Shikha and 
          Davide, Marinella and Bezerra, Paula and Fabbri, Kristian and 
          Bertarelli, Gaia and Pistorio, Anna and Dal Barco, Ilaria and 
          De Cian, Enrica},
  journal={Nature Sustainability},
  year={2025},
  note={Manuscript under review}
}
```

## 📄 License

This repository is licensed under **CC BY-NC-SA 4.0** (Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International).

You are free to:
- **Share**: Copy and redistribute the material
- **Adapt**: Remix, transform, and build upon the material

Under the following terms:
- **Attribution**: Give appropriate credit
- **NonCommercial**: Not for commercial purposes
- **ShareAlike**: Distribute derivatives under same license

See [LICENSE](https://creativecommons.org/licenses/by-nc-sa/4.0/) for details.

## 👥 Contributors

**Lead Authors**:
- Giacomo Falchetta (CMCC Foundation) - giacomo.falchetta@cmcc.it
- Antonella Mazzone
- Shikha Bhasin
- Marinella Davide
- Paula Bezerra
- Kristian Fabbri
- Gaia Bertarelli
- Anna Pistorio
- Ilaria Dal Barco
- Enrica De Cian

## 📧 Contact

For questions, issues, or collaboration inquiries:

- **GitHub Issues**: [Report a bug or request a feature](https://github.com/giacfalk/global_scp_quantification/issues)
- **Email**: giacomo.falchetta@cmcc.it
