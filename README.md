**Systemic Cooling Poverty**

Replication code for the paper “Systemic Cooling Poverty across World Countries” by Falchetta, Giacomo, Antonella Mazzone, Shikha Bhasin, Marinella Davide, Paula Bezerra, Kristian Fabbri, Gaia Bertarelli, Anna Pistorio, Ilaria Dal Barco, and Enrica De Cian. 2025. Manuscript under review at Nature Sustainability.

## Overview

This repository provides a workflow (in R) to quantify and analyze “Systemic Cooling Poverty” at a global (or regional) scale.  
It includes scripts for data processing, index calculation, aggregation, sensitivity analyses, plotting descriptive statistics and maps, and summarizing results.

## Structure

- `SOURCER.R` — the master script calling the different modules of the workflow of the analysis
  - `process_dhs_raw.R` — processing raw data (e.g. DHS surveys / input data).  
  - `recover_missing_objects_rst.R` — handle missing raster or spatial objects.  
  - `define_binary_indicators.r` — define binary indicators relevant for cooling poverty.  
  - `define_binary_indicators_sensitivity.r` — sensitivity-analysis variants of binary indicators definitions.  
  - `define_dimensions_sensitivity.r` — define dimensions of the index under different sensitivity scenarios.  
  - `calculate_index.R` — compute the main index of cooling poverty.  
  - `calculate_index_regions.R` — compute index at regional level.  
  - `aggregate_indicators.R` — aggregate indicators as part of index creation.  
  - `deprivation_stats.R` — generate statistics on deprivation / poverty.  
  - `gini_within_subnat.R` — compute inequality (e.g. Gini) within subnational units.  
  - `make_descriptive_plots.R` & `make_descriptive_plots_regions.R` — produce descriptive visualizations (global / regional).  
  - `make_radar_plots.R` — create radar-style plots summarizing multiple dimensions.  
  - `plot_scales.R` — plot how indicators / index varies across scales or thresholds.  
  - `biv_descriptive_map.R` — generate bivariate maps (e.g. overlaying two variables).  
  - `summary_tables.R` & `table_indicators.R` — build summary tables of results / indicators.  
  - `weighting_sensitivity_regions.R` — perform sensitivity analysis on weighting schemes at regional level.  
  - `sens_analysis_figures_tables.R` — compile results and figures/tables for sensitivity analysis.

## Usage

1. Retrieve raw input data from Zenodo  
2. Run `process_dhs_raw.R` (or other data-processing scripts) to clean/format the data.  
3. Use `define_binary_indicators.r` (or sensitivity version) to define the relevant deprivation / cooling-poverty indicators.  
4. Execute `calculate_index.R` to compute the cooling-poverty index (and optionally `calculate_index_regions.R` for regional summaries).  
5. Use `aggregate_indicators.R` and `deprivation_stats.R` / `gini_within_subnat.R` to compute aggregated statistics and inequality metrics.  
6. Generate plots and tables using the various `make_…` and `plot_…` scripts — descriptive maps, radar plots, summary tables, sensitivity-analysis outputs, etc.  
7. If needed, run `weighting_sensitivity_regions.R` / `sens_analysis_figures_tables.R` to explore how different weighting schemes / parameter choices affect results.

## (Optional) Sensitivity analysis

The repository supports sensitivity analysis regarding:

- Definition of binary indicators (`define_binary_indicators_sensitivity.r`).  
- Choice of dimensions and weighting scheme (`define_dimensions_sensitivity.r`, `weighting_sensitivity_regions.R`).  
- Alternative aggregation or scaling procedures.

This helps to assess robustness of the cooling-poverty index to methodological choices.

## Output / Results

Expected outputs include:

- A global and/or regional index of systemic cooling poverty.  
- Summary statistics (e.g. deprivation share, inequality metrics such as Gini).  
- Descriptive and comparative plots (maps, radar plots, charts) illustrating spatial patterns and dimension breakdowns.  
- Tables summarizing indicators, index values, sensitivity analyses.

## Requirements

- R (≥ 4.0)  
- R packages for data manipulation, spatial data and plotting (e.g. `sf`, `raster`/`terra`, `ggplot2`, `dplyr`, etc.)  

## License

CC Attribution-NonCommercial-ShareAlike 4.0 International

## Contact / Author

If you have questions or want to collaborate, feel free to reach out: giacomo.falchetta@cmcc.it 

