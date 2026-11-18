# Lobomycosis systematic review — data and reproducible figures

[![Zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24255907.svg)](https://doi.org/10.5281/zenodo.24255907)

This repository provides the data, analysis scripts, figures and audit files supporting a systematic review of human lobomycosis. Its purpose is to enable independent inspection, reproduction and reuse of the published-evidence synthesis. The dataset comprises **98 publications** (1962–2026); 94 publications report a combined **1,463 cases**, and 71 publications provide **256 extracted individual records**. Reported case counts may overlap across publications and must not be interpreted as unique patients.

The main materials are in **English**. An optional Spanish-language reproduction package is available in [`Español_Spanish/`](Espa%C3%B1ol_Spanish/LEEME.md).

## Contents

| Location | Contents |
| --- | --- |
| `data/raw/Lobomycosis_database_EN.xlsx` | English dataset: publication-level (98) and individual-level (256) sheets |
| `scripts/` | Seven English R scripts for Figures 2–5 and S1–S3 |
| `data/` | English CSV summaries, single-sheet Excel audits and PRISMA screening counts |
| `figures/Figures English/` | Eight manuscript figures, including the editable Figure 1 PRISMA PowerPoint |
| `Español_Spanish/` | Spanish dataset, seven R scripts, plots, CSV summaries and audits, plus `LEEME.md` |
| `docs/` | Analysis manifest, provisional data dictionary and repository audit |

Figure 1 (PRISMA) was made in PowerPoint and has no R script. Figures 2–5 and S1–S3 each have an English script and a Spanish counterpart. **All seven R-generated figures, including S1–S3, are complete in English and Spanish**, with corresponding PNG, TIFF, CSV summaries and Excel audits. The two languages use separate data workbooks.

## Reproduce a figure in RStudio

1. Install **R**, **RStudio**, **Arial** and the R packages used by the chosen script (`readxl`, `openxlsx`, `ragg`, `systemfonts`; some analyses also require `ggplot2`, `dplyr`, `tidyr`, `sf`, `ggalluvial`, `cowplot`, `scales`, `ggrepel`, `patchwork` and `rnaturalearthdata`).
2. Open an English `.R` file from `scripts/` in RStudio and execute the entire file with **Source**.
3. When prompted by `file.choose()`, select `data/raw/Lobomycosis_database_EN.xlsx`.
4. Inspect the **four outputs**, saved beside the script: `*.png`, `*.tiff`, `*_summary.csv` and a **single-sheet** `*_audit.xlsx`.

The input workbook is never modified. Existing outputs are preserved by adding a suffix such as `_run01`. The files already supplied in `figures/` and `data/` are reviewed copies; re-running a script does **not** automatically copy new outputs into those folders. The supported workflow is interactive RStudio; unattended command-line execution has not been verified.

| Figure | English script | Primary analysis |
| --- | --- | --- |
| 2 | `fig02_geo_en.R` | Reported cases and publications by country |
| 3 | `fig03_geo_alluvial_en.R` | Individual geography and exposure–age–clinical alluvial |
| 4 | `fig04_clinical_en.R` | Anatomical sites and lesion morphology |
| 5 | `fig05_treatment_en.R` | Treatment response, recurrence and follow-up |
| S1 | `figs01_duration_en.R` | Exact disease duration and lower-bound observations |
| S2 | `figs02_diagnosis_en.R` | Publication trends and reported diagnostic methods |
| S3 | `figs03_evidence_en.R` | Availability of information across 14 variables |

## Analytical notes

- Publication counts, reported case totals and individually extracted records have different denominators.
- **ND = not documented** (Spanish: **no documentado**): information absent or insufficiently described in the source publication. ND is not zero, a negative result or absence of clinical response.
- A record with multiple anatomical sites or morphological categories can contribute to more than one category, but only once within each category.
- Values recorded as `>` or `≥` are lower bounds and are excluded from medians and interquartile ranges for exact durations.
- Regional boundaries in Figure 3 are schematic where no verified boundary file is provided. The maps are not incidence or prevalence estimates.
- Treatment/outcome comparisons are descriptive, not causal estimates of treatment effectiveness.

## Completed analyses, validation and release status

The archived v1.0.0 release contains all 14 R scripts (seven in each language), 14 PNG/TIFF figure pairs and 14 Excel audit workbooks. **Figures S1, S2 and S3 are complete in both languages.** Across the stored audits, **292 manuscript controls are marked successful**, with no failed controls detected. This is a check of saved files and data; **the authors have successfully run the scripts in RStudio; independent execution in a clean R environment has not yet been documented**. See `docs/repository_audit_2026-11-15.md`.

The stored figure files are the research outputs for this release and are retained alongside their scripts, summaries and audits to support reproducibility.

The coauthors have agreed to open publication of this work. The repository is publicly archived in Zenodo under DOI [10.5281/zenodo.24255907](https://doi.org/10.5281/zenodo.24255907). Respect privacy and third-party redistribution rights when reusing the materials.

**Manuscript tables:** Tables 1 and 2 and Supplementary Tables S1–S3 are maintained separately as Word files for the manuscript and are not included in the archived v1.0.0 analysis package.

**Versioning:** Documentation updates on GitHub's `main` branch do not change the immutable Zenodo v1.0.0 archive. A later GitHub release would be needed for a newly archived snapshot.

## Licenses and citation

- **R scripts:** [MIT License](LICENSE), allowing reuse, modification and redistribution.
- **Original research data, figures and documentation:** [CC BY 4.0](LICENSE-DATA.md), allowing reuse and adaptation with attribution. Third-party publications and externally sourced assets are not relicensed here.
- **Cite the archived v1.0.0 dataset and scripts:** [10.5281/zenodo.24255906](https://doi.org/10.5281/zenodo.24255906) (version-specific DOI, recommended for manuscript reproducibility).
- **All versions of the repository:** [10.5281/zenodo.24255907](https://doi.org/10.5281/zenodo.24255907) (concept DOI, also used in the badge above).
- **Citation metadata:** see [CITATION.cff](CITATION.cff). Cite the originating publications when reusing extracted data.

