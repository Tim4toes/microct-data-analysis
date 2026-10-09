# =============================================================================
# SCRIPT 5.2: fold change line graphs.R
# DESCRIPTION: This script generates line graphs of fold change data. 
# It plots selected treatment groups against a defined baseline, featuring a 
# dashed horizontal line at 1 (ratio of 1:1) to represent the baseline group. 
# Users can easily configure which groups to include or exclude.
# =============================================================================

#########################################################
### A) Loading required libraries
#########################################################
library(ggplot2)
if (!require("svglite")) install.packages("svglite")
library(svglite)

#########################################################
### B) User Configuration
#########################################################

## 1. Define your MAIN directory path 
main_dir <- "E:/test/"
manual_title <- NULL 

## 2. Define the baseline group reference used for the calculations
## This is the group representing the 1.0 flat line on the graph
baseline_to_plot <- "group3"

## 3. Define the treatment groups you want to display on the graph
## To add or remove groups, simply modify this vector. 
## Example 1: groups_to_plot <- c("group1", "group2")
## Example 2 (single group): groups_to_plot <- c("group1")
groups_to_plot <- c("group1", "group2")

## 4. Define colors for each plotted group
## Ensure every group listed in groups_to_plot has an assigned color
group_colors <- c(
  "group1" = "#0269bf", 
  "group2" = "#ff1616",
  "group3" = "#35B779FF",
  "group4" = "#8c564b", # Placeholder for future groups
  "group5" = "#e377c2"  # Placeholder for future groups
)

#########################################################
### C) File Setup and Data Loading
#########################################################
cat("\n=====================================================================\n")
cat("STARTING FOLD CHANGE GRAPH GENERATION\n")
cat("=====================================================================\n")

## Define input file and output directory
input_file <- file.path(main_dir, "change", "fold change", "graphing_ready_fold_change.csv")
plot_dir   <- file.path(main_dir, "fold change graphs")

## Check if data exists
if (!file.exists(input_file)) {
  stop("Error: 'graphing_ready_fold_change.csv' not found. Please ensure Script 4 ran successfully.")
}

## Create output folder
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## Load data (check.names = FALSE prevents R from altering headers)
df_fold <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

## Filter dataset based on user preferences
df_filtered <- df_fold[df_fold$Baseline_Group == baseline_to_plot & df_fold$Variable %in% groups_to_plot, ]

if(nrow(df_filtered) == 0) {
  stop("No data matches the selected baseline and groups. Please check your spelling in the User Configuration section.")
}

## Coerce Variable to factor with exact levels for ordering
df_filtered$Variable <- factor(df_filtered$Variable, levels = groups_to_plot)

#########################################################
### D) Helper Functions & Labels
#########################################################
Traits <- c("T.Ar", "B.Ar", "B.ArT.Ar", "T.Pm", "B.Pm", "B.PmB.Ar", "Po.total", "MMI.polar", "Tb.Th")

y_labels <- c(
  "T.Ar" = "Fold change in T.Ar",
  "B.Ar" = "Fold change in B.Ar",
  "B.ArT.Ar" = "Fold change in B.Ar/T.Ar",
  "T.Pm" = "Fold change in T.Pm",
  "B.Pm" = "Fold change in B.Pm", 
  "B.PmB.Ar" = "Fold change in B.Pm/B.Ar", 
  "Po.total" = "Fold change in Po.total", 
  "MMI.polar" = "Fold change in MMI.polar",
  "Tb.Th" = "Fold change in Tb.Th"
)

## Safe metric calculator bypassing formula character limits
calculate_metrics_safe <- function(data, trait_col) {
  # Calculate Means
  agg_mean <- aggregate(data[[trait_col]], by = list(Variable = data$Variable, Seq = data$Seq), FUN = mean, na.rm = TRUE)
  names(agg_mean)[3] <- "mean"
  
  # Calculate Standard Error of the Mean (SEM)
  agg_sem <- aggregate(data[[trait_col]], by = list(Variable = data$Variable, Seq = data$Seq), FUN = function(x) {
    if (sum(!is.na(x)) <= 1) return(NA_real_)
    sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  })
  names(agg_sem)[3] <- "sem"
  
  # Merge and compute ribbon boundaries
  metric_df <- merge(agg_mean, agg_sem, by = c("Variable", "Seq"))
  metric_df$low <- metric_df$mean - metric_df$sem
  metric_df$high <- metric_df$mean + metric_df$sem
  
  return(metric_df)
}

#########################################################
### E) Plotting Loop
#########################################################
for(trait in Traits) {
  trait_col <- paste0(trait, "_FoldChange")
  
  if (!(trait_col %in% names(df_filtered))) {
    cat("  Warning: Column", trait_col, "not found in data. Skipping...\n")
    next
  }
  
  metric_df <- calculate_metrics_safe(df_filtered, trait_col)
  metric_df <- metric_df[complete.cases(metric_df), ]
  
  if (nrow(metric_df) == 0) {
    cat("  Warning: No complete data for", trait_col, ". Skipping...\n")
    next
  }
  
  # Calculate dynamic limits
  y_min <- min(metric_df$low, na.rm = TRUE)
  y_max <- max(metric_df$high, na.rm = TRUE)
  
  # Ensure the 1.0 baseline is always visible on the graph
  y_min <- min(y_min, 1)
  y_max <- max(y_max, 1)
  
  y_padding <- (y_max - y_min) * 0.05
  if (y_padding == 0) y_padding <- 0.5 # Minimum padding if everything is exactly 1.0
  
  y_limits <- c(y_min - y_padding, y_max + y_padding)
  y_breaks <- pretty(y_limits, n = 8)
  
  line_plot <- ggplot(metric_df, aes(x = Seq, y = mean, color = Variable, fill = Variable)) +
    # Add explicit baseline reference line at y = 1 for Fold Change
    geom_hline(aes(yintercept = 1, linetype = "baseline"), color = "black", linewidth = 0.8) +
    
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.4, colour = NA) +
    geom_line(linewidth = 1.2) +
    
    # name = NULL removes the legend headings, stacking the items nicely
    scale_color_manual(name = NULL, values = group_colors) +
    scale_fill_manual(name = NULL, values = group_colors) +
    scale_linetype_manual(name = NULL, 
                          values = c("baseline" = "dashed"), 
                          labels = paste0(baseline_to_plot, " (baseline)")) +
    
    labs(title = manual_title, x = "% of analysed parietal bone region", y = y_labels[[trait]]) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, breaks = y_breaks, expand = c(0, 0)) +
    theme_classic() +
    theme(
      aspect.ratio = 0.6,
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 15)),
      axis.text = element_text(size = 16, colour = "black"),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(margin = margin(r = 20)),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(15, 15, 15, 25),
      
      # These properties explicitly pull the separate legend boxes together
      legend.spacing = unit(0, "cm"), 
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )
  
  # Save plots
  ggsave(plot = line_plot, filename = file.path(plot_dir, paste0(trait, "_fold_change.tif")), width = 9, height = 5.5, dpi = 300, device = "tiff")
  ggsave(plot = line_plot, filename = file.path(plot_dir, paste0(trait, "_fold_change.svg")), width = 9, height = 5.5, device = svglite)
  
  cat("  Saved fold change line plot for trait:", trait, "\n")
}

cat("\nFINISHED FOLD CHANGE GRAPHS SUCCESSFULLY\n")