# This script adds missing columns to the cleaned data from the datafileprep.R script. 
# Data from cleaned data is converted to 0-100% sequence # and interpolated to ensure 
# all samples have the same number of rows (101).

## =============================================================================
## OPTIMIZED AUTOMATED SCRIPT FOR SINGLE WHOLE BONE ANALYSIS FILE
## =============================================================================

## 1. Define directories
main_dir <- "C:/Users/tim4t/OneDrive - Royal Veterinary College/Clofazimine_project/Parietal bone"
input_dir <- file.path(main_dir, "Cleaned 2D data")
setwd(input_dir)

## 2. Define output file directly in the main directory
output_file <- file.path(main_dir, "wholeboneanalysis.csv")

## Remove existing wholeboneanalysis.csv if it exists
if (file.exists(output_file)) {
  file.remove(output_file)
  cat("Removed existing wholeboneanalysis.csv file from main directory\n")
}

## 3. Get list of all CSV files and order them: Control first, Treated second
all_csv_files <- list.files(pattern = "\\.csv$")
csv_files <- c(
  grep("control", all_csv_files, ignore.case = TRUE, value = TRUE),
  grep("treated", all_csv_files, ignore.case = TRUE, value = TRUE)
)

## Remove duplicates in case a filename accidentally contains both
csv_files <- unique(csv_files)

if (length(csv_files) == 0) {
  stop("No matching CSV files found for control or treated groups.")
}

cols_to_interp <- c("T.Ar", "B.Ar", "B.ArT.Ar", "T.Pm", "B.Pm", "B.PmB.Ar", "Porosity", "Tb.Th")

first_file <- TRUE
file_counter <- 1

cat("\n=====================================================================\n")
cat("STARTING BATCH PROCESSING FOR ALL FILES\n")
cat("=====================================================================\n")

## Loop through each CSV file sequentially 
for (file in csv_files) {
  sample_name <- tools::file_path_sans_ext(file)
  data1 <- read.csv(file, header = TRUE)
  
  ## Identify percent column
  percent_col <- if("X..of.length" %in% names(data1)) "X..of.length" else "percent_of_length"
  
  ## Determine Variable and Treatment cleanly
  is_control <- grepl("control", sample_name, ignore.case = TRUE)
  Variable <- rep(ifelse(is_control, "control", "treated"), 101)
  Treatment <- rep(ifelse(is_control, "1", "2"), 101)
  Label <- rep(as.character(file_counter), 101)
  
  ## ---------------------------------------------------------
  ## Vectorized Interpolation for the 0 to 100 sequence
  ## ---------------------------------------------------------
  x <- data1[[percent_col]]
  xout <- 0:100 
  
  # Locate boundaries. all.inside=TRUE safely handles the 0 and 100 limits.
  idx <- findInterval(xout, x, all.inside = TRUE)
  lowerX <- x[idx]
  upperX <- x[idx + 1]
  
  # Replicate your script's original NA placement for bounds 0 and 100
  Seq1 <- lowerX
  Seq2 <- upperX
  Seq1[c(1, 101)] <- NA
  Seq2[c(1, 101)] <- NA
  
  # Calculate fractional distance 'k' for the whole array
  k <- (xout - lowerX) / (upperX - lowerX)
  
  # Interpolate all parameters simultaneously 
  temp <- as.data.frame(lapply(cols_to_interp, function(col) {
    y <- data1[[col]]
    y[idx] + (y[idx + 1] - y[idx]) * k
  }))
  names(temp) <- cols_to_interp
  
  ## Build final matrix natively
  final <- cbind(Variable, Treatment, Label, Seq1, Seq2, Seq = xout, temp)
  
  ## Write to wholeboneanalysis.csv dynamically, appending after the first file
  write.table(final, file = output_file, 
              row.names = FALSE, 
              append = !first_file, 
              sep = ",", 
              col.names = first_file)
  
  if (first_file) first_file <- FALSE
  file_counter <- file_counter + 1
  
  cat("  Processed file:", file, "- Label:", Label[1], "- Variable:", Variable[1], "\n")
}

cat("=====================================================================\n")
cat("FINISHED PROCESSING. Output saved to:", output_file, "\n")
cat("=====================================================================\n")