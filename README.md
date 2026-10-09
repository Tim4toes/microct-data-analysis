# Whole Bone Analysis Pipeline

This repository contains an automated R pipeline for processing, standardising, visualising, and statistically analysing microCT whole bone morphometry datasets.

## Prerequisites
Ensure you have R installed along with the following packages: `ggplot2`, `svglite`, `openxlsx`, `nlme`, `emmeans`, `splines`, and `patchwork`. 
* Setup your directory with a raw data folder (e.g., `2d results`) containing your initial microCT `.csv` outputs.

## Workflow Instructions

### Step 1: Clean and Prepare Raw Data
**Run Script:** `1 data file prep for whole bone analysis.R`
*   **What it does:** Prepares raw 2D microCT output data for whole bone analysis. It cleans the raw CSV files, removes extraneous headers and blank lines, standardises the bone parameter columns (e.g., T.Ar, Tb.Th), calculates the relative bone length percentage, and exports the cleaned files to a designated folder.
*   **Action:** Update the `input_dir` and `output_dir` paths, and define your `treatment_groups` vector to match your file naming conventions. Run the script to generate standardised individual files in the output directory.
Update the bone parameter columns (e.g., T.Ar, Tb.Th) to include only your required parameters.

### Step 2: Consolidate and Standardise
**Run Script:** `2 whole bone analysis percentage script.R`
*   **What it does:** Consolidates the cleaned 2D bone data files into a single master dataset. It uses linear interpolation to standardise the bone measurements into exactly 101 slices (0% to 100% of bone length) for every sample, and exports the combined data as `wholeboneanalysis.csv`.
*   **Action:** Set your `main_dir` to point to the parent folder containing your cleaned data. Ensure `treatment_groups` matches the groups defined in Step 1. Run the script to generate the combined `wholeboneanalysis.csv` file in your main directory.

### Step 3: Visualise and Analyse
**Run Script:** `3 bone line graph and heatmaps_LMM.R`
*   **What it does:** Visualises bone parameters and performs statistical analysis. It generates whole-bone line graphs with SEM ribbons, runs Linear Mixed Models (LMM) to calculate global and slice-by-slice p-values for group comparisons, exports the statistics to Excel, and produces significance heatmaps.
*   **Action:** Update the `main_dir`, `treatment_groups`, and assign hex codes in `group_colors`. Run the script. The script will automatically create `line graphs` and `heatmaps` subdirectories, populate them with `.tif` and `.svg` files, and export `PValues_LMM.xlsx`.

### Step 4: Calculate Relative Changes
**Run Script:** `4 percent and fold changes.R`
*   **What it does:** Calculates relative differences between treatment groups. It generates pairwise fold-change and percentage-change CSV files containing dynamic Excel formulas for slice-by-slice baseline comparisons, as well as consolidated numerical datasets optimized for graphing.
*   **Action:** Define your `main_dir` and explicitly set the `baseline_group_selection` (e.g., `"group3"`) for the graphing dataset. Run the script. It will generate a `change` folder containing `fold change` and `percent change` subdirectories. 

### Step 5.1: Visualise Percentage Change
**Run Script:** `5.1 percentage change graphs.R`
*   **What it does:** Generates line graphs of percentage change data. It plots selected treatment groups against a defined baseline, featuring a dashed horizontal line at 0% to represent the baseline group within a unified legend.
*   **Action:** Update the `main_dir`, configure your `baseline_to_plot`, select the `groups_to_plot` to include on the graph, and verify your `group_colors`. Run the script to generate the graphs in a newly created `percentage change graphs` folder.

### Step 5.2: Visualise Fold Change
**Run Script:** `5.2 fold change graphs.R`
*   **What it does:** Generates line graphs of fold change data. It plots selected treatment groups against a defined baseline, featuring a dashed horizontal line at 1.0 (ratio of 1:1) to represent the baseline group within a unified legend.
*   **Action:** Update the `main_dir`, configure your `baseline_to_plot`, select the `groups_to_plot` to include on the graph, and verify your `group_colors`. Run the script to generate the graphs in a newly created `fold change graphs` folder.

---

## Understanding the Dynamic Excel Formulas

The CSV files generated in Step 4 for pairwise comparisons (e.g., `wholeboneanalysis_group1_pct_Change_vs_group3.csv`) utilize dynamic Excel formulas to preserve live relationships with your raw data. Make sure `wholeboneanalysis.csv` is open when opening the respective comparison CSV files.

NOTE: The formulas execute a slice-by-slice calculation representing the standard percentage change equation:
`=(Target_Value - Baseline_Mean) / Baseline_Mean * 100`

### Formula Breakdown Example (T.Ar, Column G)
`=('[wholeboneanalysis.csv]wholeboneanalysis'!G2 - AVERAGEIFS('[wholeboneanalysis.csv]wholeboneanalysis'!G:G, '[wholeboneanalysis.csv]wholeboneanalysis'!$A:$A, "group3", '[wholeboneanalysis.csv]wholeboneanalysis'!$F:$F, $F2)) / AVERAGEIFS(...) * 100`

1.  **Extracting the Target Value:** `'[wholeboneanalysis.csv]wholeboneanalysis'!G2` pulls the absolute morphometric measurement (e.g., T.Ar) for a single replicate at a specific spatial sequence.
2.  **Calculating the Slice-Specific Baseline Mean:** `AVERAGEIFS(...)` calculates the mean of the baseline group, restricted by two critical conditions:
    *   **Group Check (`$A:$A, "group3"`):** It only averages rows matching the specific baseline variable (e.g., "group3").
    *   **Spatial Sequence Check (`$F:$F, $F2`):** It only averages rows where the `Seq` (Column F) exactly matches the sequence of the sample currently being evaluated. 
3.  **Spatial Integrity:** This dual-condition calculation ensures that spatial mapping remains intact. A morphometric trait measured at Seq 45 (midshaft) is mathematically compared only to the baseline's average specifically at Seq 45, preserving the interpolation boundaries established in Step 2.

