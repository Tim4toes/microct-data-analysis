# =============================================================================
# SCRIPT 4: 4 percent and fold changes.R
# DESCRIPTION: This script calculates relative differences between treatment groups. 
# It generates pairwise fold-change and percentage-change CSV files containing dynamic 
# Excel formulas for slice-by-slice baseline comparisons, as well as consolidated numerical 
# datasets optimized for graphing.
# =============================================================================

# =========================================================
# SECTION 1: User Configuration & File Paths
# =========================================================
main_dir <- "E:/test/"
input_file <- file.path(main_dir, "wholeboneanalysis.csv")

# Define the exact name of the group you want to use as the baseline 
# for the graphing-ready datasets (e.g., "group3", "control", "vehicle")
baseline_group_selection <- "group3"


# =========================================================
# SECTION 2: Initialization, Copying & Data Loading
# =========================================================
# Define output directories
fold_dir <- file.path(main_dir, "change/fold change")
pct_dir <- file.path(main_dir, "change/percent change")

# Create output directories inside the main directory
dir.create(fold_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pct_dir, recursive = TRUE, showWarnings = FALSE)

# Force copy the raw data file into both output folders for Excel formula mapping
file.copy(from = input_file, to = file.path(fold_dir, "wholeboneanalysis.csv"), overwrite = TRUE)
file.copy(from = input_file, to = file.path(pct_dir, "wholeboneanalysis.csv"), overwrite = TRUE)
cat("Successfully copied wholeboneanalysis.csv to local change folders.\n")

# Read the raw data
df <- read.csv(input_file, stringsAsFactors = FALSE)


# =========================================================
# SECTION 3: Dynamic Mapping & Baseline Validation
# =========================================================
# Find all parameter columns (everything after 'Seq')
seq_idx <- which(names(df) == "Seq")
if (length(seq_idx) == 0) stop("Column 'Seq' not found. Please verify the CSV format.")
calc_cols <- names(df)[(seq_idx + 1):ncol(df)]

# Create an Excel column letter lookup table (A, B ... AA, AB)
get_excel_column <- function(n) {
  if (n <= 26) return(LETTERS[n])
  paste0(LETTERS[(n - 1) %/% 26], LETTERS[(n - 1) %% 26 + 1])
}
col_letters <- sapply(1:ncol(df), get_excel_column)
names(col_letters) <- names(df)

var_letter <- col_letters[["Variable"]]
seq_letter <- col_letters[["Seq"]]

# Track original row indices for precise Excel references
df$orig_row <- 1:nrow(df) + 1

# Identify all unique groups dynamically
groups <- unique(df$Variable)

# Validate that the selected baseline group exists in the dataset
if (!(baseline_group_selection %in% groups)) {
  stop(paste("\nERROR: The baseline group '", baseline_group_selection, 
             "' was not found in your data.\nAvailable groups are:", 
             paste(groups, collapse=", ")))
}

baseline_group <- baseline_group_selection
cat("Using manually selected baseline group:", baseline_group, "\n")

# Calculate numerical group means to build the graphing-ready datasets
mean_data <- aggregate(df[calc_cols], 
                       by = list(Variable = df$Variable, Seq = df$Seq), 
                       FUN = mean, na.rm = TRUE)

graphing_fold_list <- list()
graphing_pct_list <- list()
list_idx <- 1


# =========================================================
# SECTION 4: Generate Pairwise Comparisons & Formulas
# =========================================================
for (target_group in groups) {
  for (base_group in groups) {
    if (target_group == base_group) next
    
    # --- A. INDIVIDUAL EXCEL FORMULA FILES ---
    target_df <- df[df$Variable == target_group, ]
    if (nrow(target_df) == 0) next
    
    df_fold <- target_df
    df_pct <- target_df
    
    df_fold$Variable <- paste0(target_group, "_FoldChange_vs_", base_group)
    df_pct$Variable <- paste0(target_group, "_pct_Change_vs_", base_group)
    
    res_rows <- 1:nrow(target_df) + 1
    
    for (col_name in calc_cols) {
      letter <- col_letters[[col_name]]
      
      # Dynamically construct external Excel formula links
      val_ref <- sprintf("'[wholeboneanalysis.csv]wholeboneanalysis'!%s%d", letter, target_df$orig_row)
      avg_ref <- sprintf("AVERAGEIFS('[wholeboneanalysis.csv]wholeboneanalysis'!%s:%s, '[wholeboneanalysis.csv]wholeboneanalysis'!$%s:$%s, \"%s\", '[wholeboneanalysis.csv]wholeboneanalysis'!$%s:$%s, $%s%d)", 
                         letter, letter, var_letter, var_letter, base_group, seq_letter, seq_letter, seq_letter, res_rows)
      
      df_fold[[col_name]] <- sprintf("=%s/%s", val_ref, avg_ref)
      df_pct[[col_name]] <- sprintf("=(%s-%s)/%s*100", val_ref, avg_ref, avg_ref)
    }
    
    # Drop original row tracking column before writing
    df_fold$orig_row <- NULL
    df_pct$orig_row <- NULL
    
    fold_filename <- file.path(fold_dir, paste0("wholeboneanalysis_", target_group, "_FoldChange_vs_", base_group, ".csv"))
    pct_filename <- file.path(pct_dir, paste0("wholeboneanalysis_", target_group, "_pct_Change_vs_", base_group, ".csv"))
    
    # FIX APPLIED HERE: quote = TRUE ensures commas in formulas don't split the CSV columns
    write.csv(df_fold, fold_filename, row.names = FALSE, na = "NA", quote = TRUE)
    write.csv(df_pct, pct_filename, row.names = FALSE, na = "NA", quote = TRUE)
    
    # --- B. GRAPHING READY DATASETS (Only calculate against configured Baseline) ---
    if (base_group == baseline_group) {
      b_means <- mean_data[mean_data$Variable == base_group, ]
      merged <- merge(target_df, b_means, by = "Seq", suffixes = c("", "_base"))
      merged <- merged[order(merged$orig_row), ]
      
      # Retain identifying columns and append a baseline tracker
      fold_row <- merged[, c("Variable", "Treatment", "Label", "Seq1", "Seq2", "Seq")]
      fold_row$Baseline_Group <- base_group
      pct_row <- fold_row
      
      for (col_name in calc_cols) {
        base_col <- paste0(col_name, "_base")
        fold_row[[paste0(col_name, "_FoldChange")]] <- merged[[col_name]] / merged[[base_col]]
        pct_row[[paste0(col_name, "_%Change")]] <- (merged[[col_name]] - merged[[base_col]]) / merged[[base_col]] * 100
      }
      
      graphing_fold_list[[list_idx]] <- fold_row
      graphing_pct_list[[list_idx]] <- pct_row
      list_idx <- list_idx + 1
    }
  }
}

# =========================================================
# SECTION 5: Export Combined Graphing Datasets
# =========================================================
final_graphing_fold <- do.call(rbind, graphing_fold_list)
final_graphing_pct <- do.call(rbind, graphing_pct_list)

write.csv(final_graphing_fold, file.path(fold_dir, "graphing_ready_fold_change.csv"), row.names = FALSE, na = "NA")
write.csv(final_graphing_pct, file.path(pct_dir, "graphing_ready_percent_change.csv"), row.names = FALSE, na = "NA")

cat("Success: Folders created and files generated in:", main_dir, "\n")