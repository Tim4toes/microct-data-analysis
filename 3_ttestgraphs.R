# This script use the wholeboneanalysis.csv file generated from 2_wholeboneanalysis.R to generate line graphs
# for each trait and perform statistical analysis (T-test) between control and treated groups. 
# The results are saved as high-resolution TIFF and SVG files, and the statistical results are exported to 
# a .csv file for later generation of significance heatmaps.

## =============================================================================
## OPTIMIZED BONE LINE GRAPH PLOTS & STATS (CONTROL VS TREATED)
## =============================================================================

## 1. Load Required Libraries
library(ggplot2)
library(openxlsx)
library(svglite)

## 2. Define Main Directory and Set Working Directory
main_dir <- "C:/Users/tim4t/OneDrive - Royal Veterinary College/Clofazimine_project/Parietal bone"
setwd(main_dir)

## 3. Read the Data
# Assumes wholeboneanalysis.csv is in the main directory based on previous script
final <- read.csv("wholeboneanalysis.csv", h=T, sep=",")

# Ensure proper leveling of the Variable factor
final$Variable <- factor(final$Variable, levels = c("control", "treated"))

## 4. Define Global Settings
col1 <- "#0269bf" # Control (Blue)
col2 <- "#ff1616" # Treated (Red)

# Define Traits matching the output of your previous script
Traits <- c("T.Ar", "B.Ar", "B.ArT.Ar", "T.Pm", "B.Pm", "B.PmB.Ar", "Porosity", "Tb.Th")

# Define axis labels for mapping
y_labels <- c(
  "T.Ar" = "T.Ar (mm²)",
  "B.Ar" = "B.Ar (mm²)",
  "B.ArT.Ar" = "B.Ar/T.Ar",
  "T.Pm" = "T.Pm (mm)",
  "B.Pm" = "B.Pm (mm)",
  "B.PmB.Ar" = "B.Pm/B.Ar (mm⁻¹)",
  "Porosity" = "Porosity (%)",
  "Tb.Th" = "Bone thickness (mm)"
)

# Create Output Directory for Graphs
plot_dir <- file.path(main_dir, "Line graphs")
if (!dir.exists(plot_dir)) dir.create(plot_dir)

# Function to calculate mean, and low/high for SEM
calculate_metrics <- function(data, trait) {
  Avg <- aggregate(as.formula(paste(trait, "~ Variable + Seq")), FUN = mean, na.action = na.pass, data = data)
  SEM <- aggregate(as.formula(paste(trait, "~ Variable + Seq")), 
                   FUN = function(x) sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x))), 
                   na.action = na.pass, data = data)
  metric_df <- cbind(Avg, (Avg[,3] - SEM[,3]), (Avg[,3] + SEM[,3]))
  names(metric_df) <- c("Variable", "Seq", "mean", "low", "high")
  return(metric_df)
}

## =============================================================================
## A) Generate Line Graphs with Confidence Ribbons
## =============================================================================
cat("\nGenerating Graphs...\n")

for(trait in Traits) {
  metric_df <- calculate_metrics(final, trait)
  metric_df <- metric_df[complete.cases(metric_df), ]
  
  y_min <- max(0, min(metric_df$low, na.rm = TRUE))
  y_max <- max(metric_df$high, na.rm = TRUE)
  y_padding <- (y_max - y_min) * 0.05
  y_limits <- c(max(0, y_min - y_padding), y_max + y_padding)
  
  line_plot <- ggplot(metric_df, aes(x = Seq, y = mean, color = Variable, fill = Variable)) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.4, colour = NA) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("control" = col1, "treated" = col2)) +
    scale_fill_manual(values = c("control" = col1, "treated" = col2)) +
    labs(x = "% cranial bone region", y = y_labels[[trait]]) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    theme_classic() +
    theme(
      aspect.ratio = 0.6,
      axis.text = element_text(size = 16, colour = "black"),
      axis.title = element_text(size = 20),
      axis.title.y = element_text(margin = margin(r = 20)),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(15, 15, 15, 25)
    )
  
  # Save High-Res TIFF and SVG
  ggsave(plot = line_plot, filename = file.path(plot_dir, paste0(trait, ".tif")), width = 9, height = 5.5, dpi = 300, device = "tiff")
  ggsave(plot = line_plot, filename = file.path(plot_dir, paste0(trait, ".svg")), width = 9, height = 5.5, device = svglite)
  
  cat("  -> Saved", trait, "\n")
}

## =============================================================================
## B) Statistical Analysis (Raw T-Test Only)
## =============================================================================
cat("\nRunning Statistical Analysis...\n")

TTest_results <- list()
seq_range <- 0:100 # Sequence set exactly from 0 to 100

for (trait in Traits) {
  # Create a dataframe to store Seq and p-value
  trait_stats <- data.frame(Seq = seq_range, `p-value` = NA, check.names = FALSE)
  
  for(i in seq_along(seq_range)) { 
    current_seq <- seq_range[i]
    subset_data <- final[final$Seq == current_seq, ]
    
    # Run the standard T-test assuming independence
    if(nrow(subset_data) >= 2 && length(unique(subset_data$Variable)) == 2) {
      test_result <- try(t.test(as.formula(paste(trait, "~ Variable")), data = subset_data), silent = TRUE)
      if(!inherits(test_result, "try-error")) {
        trait_stats$`p-value`[i] <- test_result$p.value
      }
    }
  }
  
  TTest_results[[trait]] <- trait_stats
}

## 3. Export to Excel
wb <- createWorkbook()
for (trait in Traits) {
  addWorksheet(wb, sheetName = trait)
  writeData(wb, sheet = trait, x = TTest_results[[trait]])
}

excel_output_path <- file.path(main_dir, "TTest.xlsx")
saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("=====================================================================\n")
cat("FINISHED. Graphs saved in 'Line graphs' folder. Stats saved to:", excel_output_path, "\n")