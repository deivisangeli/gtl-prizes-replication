# Replication Package: "The Missing Nobels"

Replication materials for:

> Agarwal, R., Angeli, D., & Gaule, P. "The Missing Nobels."

## Quick Start

```bash
# 1. Install R (>= 4.3) and ensure Rscript is on your PATH

# 2. Install renv (if not already installed)
Rscript -e "install.packages('renv')"

# 3. Restore package library
Rscript -e "renv::restore()"

# 4. Generate all tables and figures
Rscript run_all.R

# 5. Verify outputs
Rscript tests/test_outputs.R
```

## Repository Structure

```
├── README.md                 This file
├── run_all.R                 Master script — runs all code/*.R in order
├── _helpers.R                Shared configuration (paths, colors, plotting functions)
├── renv.lock                 Locked R package versions
├── data/                     Input datasets (see Data Provenance below)
├── code/                     Analysis scripts (numbered)
│   ├── 01_ranking_tables.R        → Tables 1, S1, S3
│   ├── 02_correlation_table.R     → Table S2
│   ├── 03_scatterplots.R          → Figures 1-3, S1
│   ├── 04_money_weights.R         → Figure 6, S2
│   ├── 05_prizes_per_doctor.R     → Table 2
│   ├── 06_robustness.R            → Table S4
│   ├── 07_density_phd.R           → Figure 5
│   ├── 08_density_vs_academics.R  → Figure 4
│   ├── 09_density_tiers.R         → Figures S3-S4
│   └── 10_density_funding.R       → Figure S5
├── output/
│   ├── tables/               Generated .tex table files
│   └── figures/              Generated .pdf and .png figure files
├── tests/
│   └── test_outputs.R        Verification script
└── expected/                 Reference copies of key outputs
```

## Data Provenance

| File | Description | Source |
|------|-------------|--------|
| `cleanPrizeList.xlsx` | Final cleaned list of 99 most prestigious prizes with all indicators | Authors' compilation from multiple sources |
| `cleanEC.xlsx` | Final cleaned list of 68 early-career prizes | Authors' compilation |
| `mainPrizeList_pre-imputation.xlsx` | Prize list before OLS imputation of missing ratings | Authors' compilation |
| `nsf2023.xlsx` | US doctoral degrees by field (2023) | NSF Survey of Earned Doctorates |
| `2022budgetByField.xlsx` | US federal R&D budget by field (2022) | NSF NCSES |
| `all_winners_with_plotFinestField.csv` | Individual prize winners 2015-2024 with field assignments | Authors' collection from prize websites + OpenAlex API |
| `subfield_to_finest_group.csv` | Mapping from OpenAlex subfields to field groups | Authors |
| `nsf_field_to_finest_group.csv` | Mapping from NSF doctorate fields to field groups | Authors |
| `vs_academics_by_finest_group.csv` | Count of senior research academics by field | OpenAlex API |
| `authors_by_finest_group.csv` | Count of active researchers by field | OpenAlex API |

## Software Requirements

- R >= 4.3
- All R packages are managed via `renv` (see `renv.lock`)
- Key packages: tidyverse, readxl, stargazer, ggrepel, LaplacesDemon, estimatr, Hmisc, xtable

## Notes

- The robustness exercise (script 06) uses `set.seed(42)` for reproducibility. Results will match exactly across runs.
- All outputs use relative paths. No external dependencies (Dropbox, Overleaf) are required.
- Scripts are designed to be run sequentially via `run_all.R`, but each can also be run independently from the repo root.

## License

Data and code are provided for replication purposes. Please cite the paper if you use these materials.
