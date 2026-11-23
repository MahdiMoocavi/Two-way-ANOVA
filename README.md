# Factorial Analysis of Living Conditions and PHQ-9 Scores

An R analysis examining the association between living conditions, area of residence, and PHQ-9 depression scores. The script combines descriptive analysis, assumption checks, factorial and follow-up ANOVA models, post-hoc comparisons, and non-parametric alternatives.

## Research question

How do living conditions and residential area relate to PHQ-9 scores, and is there evidence of an interaction between the two factors?

## Analysis workflow

1. Import the source spreadsheet and recode living conditions and residential area as categorical factors.
2. Inspect descriptive statistics, including group sizes, means, standard deviations, skewness, and kurtosis.
3. Visualize PHQ-9 scores across groups.
4. Examine distributional assumptions, variance homogeneity, and potential outliers.
5. Fit a two-way ANOVA with living condition, area of residence, and their interaction.
6. Estimate effect sizes and conduct Tukey post-hoc comparisons.
7. Run a one-way follow-up analysis and non-parametric alternatives (Kruskal–Wallis and Dunn tests).

## Input data

The script opens a file-selection dialog and expects an Excel workbook containing at least these columns:

- `PHQ9`: PHQ-9 total score
- `LivingWith`: living-condition grouping variable
- `Area`: residential-area grouping variable

The original dataset is not included in this repository.

## Requirements

R and the following packages:

```r
install.packages(c(
  "tidyverse", "readxl", "rstatix", "ggpubr", "car",
  "purrr", "MASS", "multcomp", "outliers", "lsr",
  "moments", "officer"
))
```

## Run

Open `factorial_anova_phq9.R` in RStudio or another R environment, install the dependencies, and execute the script. Select the input workbook when prompted.

## Outputs

The script displays descriptive and inferential results and creates two image files, `image1.png` and `image2.png`, for selected group visualizations. Additional plots are displayed in the active graphics device.

## Reproducibility note

The analysis is tailored to the original variable names and dataset structure. The outlier-filtering section should be reviewed and validated before its filtered data are used for interpretation. Raw data are intentionally not included.
