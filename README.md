Overview

This repository contains the complete dataset, analytical outputs, and supplementary documentation associated with a study examining how coastal forests on Long Island, NY are structurally and compositionally reorganizing along the salt marsh–forest ecotone.
The research integrates tree-level measurements, community composition, density metrics, basal area, and mixed-effects modeling to test whether empirical forest structure aligns with predictions of the ecological ratchet model of coastal forest retreat.
Together, these files provide a reproducible, transparent account of the study’s methodology—from raw field sheets to final statistical analyses.

Datasets include:

Plot-level descriptors and GPS metadata
Tree-level DBH measurements (n = 331)
Community composition matrices
Density per acre and per hectare
Seven site-specific raw sheets for full traceability
Supporting R code, diagnostics, and visualization outputs
This repository mirrors the dataset described in the associated manuscript and Data Primer.

Repository Structure
root/
│ README.md
│
├── data_raw/
│   ├── Capstone-Table.xlsx
│   ├── CenterPoints.csv
│   ├── Density-Composition.csv
│   ├── DBH.csv
│   ├── [7 site-level raw field sheets]
│   └── Equation_sheet.csv
│
├── data_processed/
│   ├── Species matrices
│   ├── Derived density tables
│   ├── Model-ready DBH datasets
│   └── Cleaned composition and structural datasets
│
├── analysis/
│   ├── DBH Analysis.html
│   ├── Basal Area.html
│   ├── Community Composition Analysis.html
│   ├── Density Analysis.pdf
│   └── representative-plot.pdf
│
└── manuscript/
    ├── Structural and Compositional Divergence (DOCX)
    ├── Supplement (DOCX)
    └── Data Overview / Primer (DOCX)

📊 Dataset Descriptions
CenterPoints.csv

Plot-level metadata, including GPS coordinates (Garmin GPSMAP 67i, ±1.8–3 m), elevation (NAVD88), and field notes.
These values anchor each plot spatially and define plot classification (Edge, Transition, Interior), which is central to the mixed-effects framework.

Density-Composition.csv

A processed, plot-level community dataset summarizing:

Total stems per plot
Density per acre
Species counts and abundances
Plot type and site metadata
Used for Bray–Curtis dissimilarity matrices, NMDS ordination, PERMANOVA, and dispersion tests (PERMDISP).
DBH.csv
Individual-tree DBH records (n = 331), including:
Tree ID
Scientific and common names
DBH (cm) at 1.37 m
Plot type (Edge, Transition, Interior)
Location (site)

Used for:

Log-transformed DBH mixed-effects models
Structural comparisons across the ecotone
Per-plot basal area calculations
Site-Level Raw Sheets

Seven CSV files preserving the original field data.
These contain unaltered DBH readings, species identities, and plot assignments for all trees measured at:

Wertheim
Seatuck
Pine Neck / Quogue
Ludlow Creek
Gardiner County Park
Haven Point
Shinnecock Reservation
These are essential for transparency, traceability, and reproducing the study from first principles.

Equation_sheet.csv

Contains the original Excel formulas used during early data exploration, including:
Plot area conversions (1/20 acre → per acre → per hectare)
Density calculations
Relative abundance derivations
Preliminary DBH transformations
These formulas are replicated within the R scripts.

Analysis Outputs

This folder includes HTML and PDF outputs generated using R.
These files correspond to the analyses described in the manuscript and supplement:

DBH Analysis.html – Diagnostics and mixed models

Basal Area.html – Calculations and summaries

Community Composition Analysis.html – PERMANOVA, NMDS, PERMDISP
Density Analysis.pdf – Density modeling and residual structure
representative-plot.pdf – Empirical vs. conceptual ratchet model visualization
The representative figure displays differences in structural size, composition, and density across the gradient.

- Field Methods Summary

Field data were collected across seven coastal forest sites on Long Island, NY.
At each site, three circular plots (radius ≈ 8 m; area = 1/20 acre) were established along a transect perpendicular to the marsh:

Edge Plot: At the marsh boundary
Transition Plot: ~17.5 m inland
Interior Plot: ~35 m inland

Within each plot:

All trees and saplings ≥2.54 cm DBH were measured at 1.37 m height
Seedlings (<1.37 m) were excluded due to dense understory (Smilax spp.) and survey constraints
GPS coordinates were collected with a Garmin GPSMAP 67i
This sampling design follows the methodological details described in the manuscript and Data Primer.

- Analytical Workflow

All analyses were conducted in R 4.5.0, using packages detailed in the Supplement (MASS, tidyverse, vegan, lmerTest, patchwork, gridExtra, etc.).

1. Community Composition
   
Bray–Curtis dissimilarity
NMDS ordination
PERMANOVA with blocked permutations (Location as a strata variable)
Multivariate dispersion testing (PERMDISP)
PERMDISP indicated no significant difference in dispersion among plot types, validating subsequent PERMANOVA interpretations.

2. Tree Size (DBH) Analysis

A log-transformed mixed-effects model was used:
log(DBH) ~ Plot + (1 | Location)

Residual diagnostics justified the log transformation.
Results show significantly smaller DBH in edge plots compared to transition and interior zones.

3. Density Analysis

Density per acre was modeled as:
log(Density) ~ Plot + (1 | Location)

No significant differences were detected across plot types, indicating that density remains relatively stable even as forest structure and composition reorganize.

4. Basal Area & Structural Integration

Basal area calculations and structural comparisons appear in:
Basal Area.html
representative-plot.pdf
The representative plot visualizes empirical patterns alongside predictions from a conceptual ratchet model, highlighting divergence in:
Structural size
Density
Composition

- Example R Usage
Load data
db <- read_csv("data_raw/DBH.csv")
den <- read_csv("data_raw/Density-Composition.csv")

Run PERMANOVA
library(vegan)
comm <- den %>% select(-Location, -Plot)
adonis2(vegdist(comm, "bray") ~ den$Plot, strata = den$Location)

Fit DBH LMM
library(lmerTest)
db$logDBH <- log(db$DBH)
lmer(logDBH ~ Plot + (1|Location), data = db)

- Citation

Anderson, N.C. (Year).
Structural and Compositional Divergence Across the Salt Marsh–Forest Ecotone: An Assessment of Coastal Forest Retreat Models on Long Island, NY.
(Full citation updated upon publication.)

- Licensing & Use

Raw field data are included to ensure reproducibility and scientific transparency.

Redistribution or reuse beyond academic purposes should be discussed with the author.

Processed datasets and scripts may be used with appropriate citation.

- Contact

Nicholas (Cole) Anderson
Email: Nicholas.s.anderson@stonybrook.edu
Affiliation: Stony Brook University
