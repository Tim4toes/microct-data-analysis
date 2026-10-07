## =============================================================================
## SCRIPT 3: 3 Bone line graph and heatmaps_LMM.R
## DESCRIPTION: This script visualizes bone parameters and performs statistical analysis. 
## It generates whole-bone line graphs with SEM ribbons, runs Linear Mixed Models (LMM) 
## to calculate global and slice-by-slice p-values for group comparisons, exports the 
## statistics to Excel, and produces significance heatmaps.
## =============================================================================

#########################################################
### A) Loading required libraries
#########################################################
# Unused libraries (lattice, latticeExtra, phia, gridExtra, devEMF, car, Cairo, 
# gplots, RColorBrewer, magick, grid) have been removed for optimization.
library(ggplot2)
if (!require("svglite")) install.packages("svglite")
library(svglite)
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)
library(nlme)       
library(emmeans)    
library(splines)    
library(patchwork)

#########################################################
### B) Define Global Settings, Groups & Colors
#########################################################

## 1. Define your MAIN directory path 
main_dir <- "E:/test/"
manual_title <- NULL 

## 2. Define your treatment groups in the exact order you want them plotted
treatment_groups <- c("group1", "group2", "group3")

## 3. Define colors for each group using a named vector
group_colors <- c(
  "group1" = "#0269bf", 
  "group2" = "#ff1616", 
  "group3" = "#35B779FF"
)

## 4. AUTOMATED Pairwise Comparison Generation
## This automatically creates all possible pair combinations (e.g., 6 pairs for 4 groups)
comp_matrix <- combn(treatment_groups, 2)
comparisons <- list()
comparison_labels <- c()

for (i in 1:ncol(comp_matrix)) {
  comp_name <- paste(comp_matrix[1, i], "vs", comp_matrix[2, i], sep = "_")
  comparisons[[comp_name]] <- c(comp_matrix[1, i], comp_matrix[2, i])
  comparison_labels <- c(comparison_labels, paste(comp_matrix[1, i], "vs", comp_matrix[2, i]))
}

# Define Heatmap Colors
myCol2 <- c("#FDE725FF", "#35B779FF", "#31688eff", "#440154FF")
myBreaks2 <- c(0, 0.001, 0.01, 0.05, 1)

# Function to calculate metrics (Optimized with merge to guarantee row alignment)
calculate_metrics <- function(data, trait) {
  form <- as.formula(paste(trait, "~ Variable + Seq"))
  Avg <- aggregate(form, FUN = mean, na.action = na.pass, data = data)
  SEM <- aggregate(form, FUN = function(x) {
    if (sum(!is.na(x)) <= 1) return(NA_real_)
    sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  }, na.action = na.pass, data = data)
  
  metric_df <- merge(Avg, SEM, by = c("Variable", "Seq"), suffixes = c(".mean", ".sem"))
  metric_df$low <- metric_df[[paste0(trait, ".mean")]] - metric_df[[paste0(trait, ".sem")]]
  metric_df$high <- metric_df[[paste0(trait, ".mean")]] + metric_df[[paste0(trait, ".sem")]]
  names(metric_df)[3] <- "mean"
  
  return(metric_df[, c("Variable", "Seq", "mean", "low", "high")])
}

# Define traits to plot
Traits <- c("T.Ar", "B.Ar", "B.ArT.Ar", "T.Pm", "B.Pm", "B.PmB.Ar", "Po.total", "MMI.polar", "Tb.Th")

# Define y-axis labels for each trait
y_labels <- c(
  "T.Ar" = "T.Ar (mm²)",
  "B.Ar" = "B.Ar (mm²)",
  "B.ArT.Ar" = "B.Ar/T.Ar (%)",
  "T.Pm" = "T.Pm (mm)",
  "B.Pm" = "B.Pm (mm)", 
  "B.PmB.Ar" = "B.Pm/B.Ar (1/mm)", 
  "Po.total" = "Po.total (%)", 
  "MMI.polar" = "MMI.polar (mm⁴)",
  "Tb.Th" = "Tb.Th (mm)"
)

# Define comparison pairs for LMM Post-Hoc Tests
comparisons <- list(
  "group1_vs_group2" = c("group1", "group2"),
  "group1_vs_group3" = c("group1", "group3"),
  "group2_vs_group3" = c("group2", "group3")
)

#########################################################
### C) File Setup and Data Loading
#########################################################

cat("\n=====================================================================\n")
cat("STARTING GRAPH GENERATION & ANALYSIS\n")
cat("=====================================================================\n")

## Map paths to the main directory
data_file <- file.path(main_dir, "wholeboneanalysis.csv")
plot_dir  <- file.path(main_dir, "line graphs")
heat_dir  <- file.path(main_dir, "heatmaps")

## Safety Check: Verify the wholeboneanalysis.csv data exists
if (!file.exists(data_file)) {
  stop("Error: 'wholeboneanalysis.csv' not found in the main directory. Please check the path.")
}

## Create output folders if they don't exist
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(heat_dir)) dir.create(heat_dir, recursive = TRUE)

## Read in final combined data
final <- read.csv(data_file, h=TRUE, sep=",")

# Coerce Variable to factor and ensure proper leveling for plotting order
final$Label <- as.factor(final$Label)
final$Variable <- factor(final$Variable, levels = treatment_groups)

#########################################################
### D) Plotting individual parameters
#########################################################

for(trait in Traits) {
  metric_df <- calculate_metrics(final, trait)
  metric_df[,3:5] <- lapply(metric_df[,3:5], as.numeric)
  metric_df <- metric_df[complete.cases(metric_df), ]
  
  if (nrow(metric_df) == 0) next
  
  y_min <- min(metric_df$low, na.rm = TRUE)
  y_max <- max(metric_df$high, na.rm = TRUE)
  y_min <- max(0, y_min)  
  
  y_padding <- (y_max - y_min) * 0.05
  if (y_padding == 0) y_padding <- 1
  y_limits <- c(y_min - y_padding, y_max + y_padding)
  y_breaks <- pretty(y_limits, n = 8)
  y_limits[1] <- max(0, y_limits[1])
  
  line_plot <- ggplot(metric_df, aes(x = Seq, y = mean, color = Variable, fill = Variable)) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.4, colour = NA) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = group_colors) +
    scale_fill_manual(values = group_colors) +
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
      plot.margin = margin(15, 15, 15, 25)
    )
  
  ggsave(plot = line_plot, filename = file.path(plot_dir, paste0(trait, ".tif")), width = 9, height = 5.5, dpi = 300, device = "tiff")
  ggsave(plot = line_plot, filename = file.path(plot_dir, paste0(trait, ".svg")), width = 9, height = 5.5, device = svglite)
  
  cat("  Saved line plot for trait:", trait, "\n")
}

#########################################################
### E) LMM Analysis & Pairwise Comparisons
#########################################################
lmm_p_all <- list()
lmm_p_global <- list()

seq_grid <- sort(unique(final$Seq))

for(trait in Traits) {
  cat("  Running LMM model for trait:", trait, "\n")
  lmm_p_all[[trait]] <- list()
  
  dat <- final[!is.na(final[[trait]]) & !is.na(final$Variable) & !is.na(final$Seq) & !is.na(final$Label), c("Seq", "Variable", "Label", trait), drop = FALSE]
  names(dat)[4] <- "Y"
  
  m_full <- try(lme(fixed = Y ~ Variable * ns(Seq, df = 4), random = ~1 | Label, data = dat, method = "REML"), silent = TRUE)
  m_red <- try(lme(fixed = Y ~ ns(Seq, df = 4), random = ~1 | Label, data = dat, method = "REML"), silent = TRUE)
  
  for(comp_name in names(comparisons)) {
    lmm_p_all[[trait]][[comp_name]] <- data.frame(Seq = seq_grid, p = NA)
  }
  
  if(!inherits(m_full, "try-error") && !inherits(m_red, "try-error")) {
    lr <- anova(update(m_red, method = "ML"), update(m_full, method = "ML"))
    pval_col <- grep("p[\\.-]?value", colnames(lr), ignore.case = TRUE)
    lmm_p_global[[trait]] <- if (length(pval_col) > 0) as.numeric(lr[[pval_col[1]]][2]) else NA_real_
    
    ######################################################################################
    # MULTIPLE TESTING CORRECTION TOGGLE
    # Uncomment exactly ONE of the 'pw' Options below:
    ######################################################################################
    
    emm <- try(emmeans(m_full, ~ Variable | Seq, at = list(Seq = seq_grid)), silent = TRUE)
    
    if (!inherits(emm, "try-error")) {
      
      # OPTION 1: No Correction
      # pw <- try(as.data.frame(contrast(emm, method = "pairwise", adjust = "none")), silent = TRUE)
      
      # OPTION 2: Tukey (Best for Slice-by-Slice)
      pw <- try(as.data.frame(contrast(emm, method = "pairwise", adjust = "tukey")), silent = TRUE)
      
      # OPTION 3: Global FDR (Best for Whole-Bone)
      # pw <- try(as.data.frame(contrast(emm, method = "pairwise", adjust = "none")), silent = TRUE)
      # if (!inherits(pw, "try-error")) {
      #   pw_pval_col <- grep("p[\\.-]?value", colnames(pw), ignore.case = TRUE)
      #   if (length(pw_pval_col) > 0) pw[[pw_pval_col[1]]] <- p.adjust(pw[[pw_pval_col[1]]], method = "fdr")
      # }
      
      ######################################################################################
      
      # Execute Vectorized Data Extraction
      if (!inherits(pw, "try-error")) {
        pw_pval_col <- grep("p[\\.-]?value", colnames(pw), ignore.case = TRUE)
        
        if (length(pw_pval_col) > 0) {
          for (comp_name in names(comparisons)) {
            comp_groups <- comparisons[[comp_name]]
            label1 <- paste(comp_groups[1], "-", comp_groups[2])
            label2 <- paste(comp_groups[2], "-", comp_groups[1])
            
            # Subset the specific comparison from the emmeans output
            sub_pw <- pw[trimws(as.character(pw$contrast)) %in% c(label1, label2), ]
            
            # Merge by Seq to ensure correct alignment before assigning p-values
            merged_pw <- merge(data.frame(Seq = seq_grid), 
                               sub_pw[, c("Seq", colnames(pw)[pw_pval_col[1]])], 
                               by = "Seq", all.x = TRUE)
            
            lmm_p_all[[trait]][[comp_name]]$p <- as.numeric(merged_pw[[2]])
          }
        }
      }
    }
  } else {
    lmm_p_global[[trait]] <- NA_real_
  }
}

#########################################################
### F) Export to Excel
#########################################################
wb <- createWorkbook()
for(trait in Traits) {
  for(comp_name in names(comparisons)) {
    sheet_name <- paste(trait, comp_name, sep = "_")
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, x = lmm_p_all[[trait]][[comp_name]])
  }
}
excel_output_path <- file.path(main_dir, "PValues_LMM.xlsx")
saveWorkbook(wb, excel_output_path, overwrite = TRUE)
cat("  Statistics saved to PValues_LMM.xlsx\n")

#########################################################
### G) Generate Separate Main and Posthoc Heatmaps
#########################################################
create_comparison_heatmaps <- function(trait) {
  min_seq <- min(seq_grid)
  max_seq <- max(seq_grid)
  
  # 1. MAIN HEATMAP 
  global_p <- lmm_p_global[[trait]]
  if (length(global_p) == 0 || is.na(global_p)) global_p <- NA
  
  main_data <- data.frame(
    Seq = seq_grid,
    p_binned = cut(rep(global_p, length(seq_grid)), breaks = myBreaks2, labels = c("p ≤ 0.001", "0.001 < p ≤ 0.01", "0.01 < p ≤ 0.05", "p > 0.05"), include.lowest = TRUE)
  )
  
  p_main <- ggplot(main_data, aes(x = Seq, y = 1, fill = p_binned)) +
    geom_tile(height = 1, width = 1) +
    scale_fill_manual(values = myCol2, drop = FALSE) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    annotate("text", x = max(seq_grid) + 1.5, y = 1, label = "Global Effect", hjust = 0, vjust = 0.5, size = 3.5, fontface = "bold") +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7), 
      plot.margin = margin(5, 100, 10, 0), 
      legend.position = "none",
      plot.background = element_rect(fill = "white", colour = NA) 
    ) +
    coord_cartesian(clip = "off", xlim = c(min_seq - 0.5, max_seq + 0.5), expand = FALSE)
  
  main_file <- file.path(heat_dir, paste0(trait, "_LMM_pvalueheatmap_main.tif"))
  ggsave(main_file, p_main, width = 9, height = 1.2, dpi = 300, device = "tiff")
  
  # 2. POSTHOC HEATMAPS 
  comp_names <- names(comparisons)
  heat_plots <- list()
  
  for(i in seq_along(comp_names)) {
    comp_name <- comp_names[i]
    
    valid_data <- data.frame(
      Seq = lmm_p_all[[trait]][[comp_name]]$Seq,
      p_value = as.numeric(as.character(lmm_p_all[[trait]][[comp_name]]$p))
    )
    valid_data <- valid_data[!is.na(valid_data$p_value), ]
    
    heat_data <- data.frame(
      Seq = valid_data$Seq,
      p_binned = cut(valid_data$p_value, breaks = myBreaks2, labels = c("p ≤ 0.001", "0.001 < p ≤ 0.01", "0.01 < p ≤ 0.05", "p > 0.05"), include.lowest = TRUE)
    )
    
    heat_plots[[i]] <- ggplot(heat_data, aes(x = Seq, y = 1, fill = p_binned)) +
      geom_tile(height = 1, width = 1) +
      scale_fill_manual(values = myCol2, drop = FALSE) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      annotate("text", x = max(valid_data$Seq) + 1.5, y = 1, label = comparison_labels[i], hjust = 0, vjust = 0.5, size = 3.5) +
      theme_void() +
      theme(
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7), 
        plot.margin = margin(0, 100, 0, 0), 
        legend.position = "none"
      ) +
      coord_cartesian(clip = "off", xlim = c(min_seq - 0.5, max_seq + 0.5), expand = FALSE)
  }
  
  p_posthoc <- wrap_plots(heat_plots, ncol = 1)
  posthoc_file <- file.path(heat_dir, paste0(trait, "_LMM_pvalueheatmap_posthoc.tif"))
  ggsave(posthoc_file, p_posthoc, width = 9, height = 2.5, dpi = 300, device = "tiff")
}

lapply(Traits, create_comparison_heatmaps)
cat("\nFINISHED GRAPHS, LMM T-TEST EXPORT, & HEATMAPS SUCCESSFULLY\n")