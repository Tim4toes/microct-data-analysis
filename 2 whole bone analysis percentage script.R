## =============================================================================
## SCRIPT 2: 2 whole bone analysis percentage script.R
## DESCRIPTION: This script consolidates the cleaned 2D bone data files into a single 
## master dataset. It uses linear interpolation to standardize the bone measurements into 
## exactly 101 slices (0% to 100% of bone length) for every sample, and exports the 
## combined data as 'wholeboneanalysis.csv'.
## =============================================================================

# =========================================================
# SECTION 1: Define Directories and File Paths
# =========================================================
## Define your MAIN directory path
main_dir <- "E:/test"
input_dir <- file.path(main_dir, "cleaned 2d data")
output_file <- file.path(main_dir, "wholeboneanalysis.csv")

## Remove existing wholeboneanalysis.csv if it exists to start fresh
if (file.exists(output_file)) {
  file.remove(output_file)
  cat("Removed existing wholeboneanalysis.csv file from:", main_dir, "\n")
}

# =========================================================
# SECTION 2: Define Experimental Groups
# =========================================================
## Define the list of treatment groups you want to process sequentially.
## To add more groups or change names (e.g. "Control", "Treated"), simply update this vector.
treatment_groups <- c("group1", "group2", "group3")

## Initialize a flag for header writing and a global counter for unique animal IDs
first_file <- TRUE
file_counter <- 1 

# =========================================================
# SECTION 3: Main Processing Loop
# =========================================================
for (group in treatment_groups) {
  
  cat("\n=====================================================================\n")
  cat("STARTING PROCESSING FOR TREATMENT GROUP:", group, "\n")
  cat("=====================================================================\n")
  
  ## Get list of all CSV files in the input directory and filter by group prefix
  all_csv_files <- list.files(path = input_dir, pattern = "\\.csv$")
  csv_files <- grep(paste0("^", group), all_csv_files, value = TRUE)
  
  if (length(csv_files) == 0) {
    cat("No matching CSV files found in 'Cleaned 2D data' for group:", group, "- skipping.\n")
    next
  }
  
  for (file in csv_files) {
    ## Read the individual CSV
    full_file_path <- file.path(input_dir, file)
    data1 <- read.csv(full_file_path, header = TRUE)
    
    ## Check and standardize the percentage column name
    percent_col <- if("X..of.length" %in% colnames(data1)) {
      "X..of.length"
    } else if("percent_of_length" %in% colnames(data1)) {
      "percent_of_length"
    } else {
      stop(paste("Percentage column not found in file:", file))
    }
    
    # =========================================================
    # SECTION 4: Vectorized Metadata Assignment
    # =========================================================
    ## Automatically assign metadata based on the current active group name
    Label <- rep(as.character(file_counter), 101)
    Variable <- rep(group, 101)
    
    ## Automatically strip the word "group" to define the Treatment number. 
    ## If using custom names like "LowDose", Treatment will simply mirror Variable.
    treatment_val <- gsub("group", "", group, ignore.case = TRUE)
    Treatment <- rep(treatment_val, 101)
    
    # =========================================================
    # SECTION 5: Pre-allocate Data Arrays
    # =========================================================
    ## Initialize sequences
    Seq <- seq(from = 0, to = 100, by = 1)
    Seq1 <- rep(NA, 101)
    Seq2 <- rep(NA, 101)
    
    ## Pre-allocate numeric vectors of length 99 for high-speed memory assignment
    T.Ar <- numeric(99)
    B.Ar <- numeric(99)
    B.ArT.Ar <- numeric(99)
    T.Pm <- numeric(99)
    B.Pm <- numeric(99)
    B.PmB.Ar <- numeric(99)
    Po.total <- numeric(99)
    MMI.polar <- numeric(99)
    Tb.Th <- numeric(99)
    
    # =========================================================
    # SECTION 6: Linear Interpolation Loop (1% to 99%)
    # =========================================================
    for (i in 1:99) {
      ## Identify bounding rows using fast index lookup instead of subsetting
      lowerid <- max(which(data1[[percent_col]] <= i))
      lowerX <- data1[[percent_col]][lowerid]
      upperid <- lowerid + 1
      upperX <- data1[[percent_col]][upperid]
      
      Seq1[i + 1] <- lowerX
      Seq2[i + 1] <- upperX
      
      ## Calculate interpolation coefficient safely to prevent division-by-zero
      k <- if (upperX == lowerX) 0 else (1 / (upperX - lowerX)) * (i - lowerX)
      
      ## Interpolate trait values
      T.Ar[i] <- data1$T.Ar[lowerid] + (data1$T.Ar[upperid] - data1$T.Ar[lowerid]) * k
      B.Ar[i] <- data1$B.Ar[lowerid] + (data1$B.Ar[upperid] - data1$B.Ar[lowerid]) * k
      B.ArT.Ar[i] <- data1$B.ArT.Ar[lowerid] + (data1$B.ArT.Ar[upperid] - data1$B.ArT.Ar[lowerid]) * k
      T.Pm[i] <- data1$T.Pm[lowerid] + (data1$T.Pm[upperid] - data1$T.Pm[lowerid]) * k
      B.Pm[i] <- data1$B.Pm[lowerid] + (data1$B.Pm[upperid] - data1$B.Pm[lowerid]) * k
      B.PmB.Ar[i] <- data1$B.PmB.Ar[lowerid] + (data1$B.PmB.Ar[upperid] - data1$B.PmB.Ar[lowerid]) * k
      Po.total[i] <- data1$Po.total[lowerid] + (data1$Po.total[upperid] - data1$Po.total[lowerid]) * k
      MMI.polar[i] <- data1$MMI.polar[lowerid] + (data1$MMI.polar[upperid] - data1$MMI.polar[lowerid]) * k
      Tb.Th[i] <- data1$Tb.Th[lowerid] + (data1$Tb.Th[upperid] - data1$Tb.Th[lowerid]) * k
    }
    
    # =========================================================
    # SECTION 7: Combine and Export Data
    # =========================================================
    ## Bind the 1-99% sequence data
    temp <- cbind(T.Ar, B.Ar, B.ArT.Ar, T.Pm, B.Pm, B.PmB.Ar, Po.total, MMI.polar, Tb.Th)
    trait_cols <- c("T.Ar", "B.Ar", "B.ArT.Ar", "T.Pm", "B.Pm", "B.PmB.Ar", "Po.total", "MMI.polar", "Tb.Th")
    
    ## Extract the absolute 0% and 100% slices to cap the dataset
    seq0 <- subset(data1, data1[[percent_col]] == 0)[, trait_cols]
    seq100 <- subset(data1, data1[[percent_col]] == 100)[, trait_cols]
    
    ## Construct the final standardized 101-row data frame
    temp <- rbind(seq0, temp, seq100)
    final <- cbind(Variable, Treatment, Label, Seq1, Seq2, Seq, temp)
    
    ## Write to file (appends subsequent files automatically)
    write.table(final, file = output_file, row.names = FALSE, append = !first_file, sep = ",", col.names = first_file)
    
    if (first_file) first_file <- FALSE
    file_counter <- file_counter + 1
    
    cat("  Processed file:", file, "- Label:", Label[1], "- Variable:", Variable[1], "- Treatment:", Treatment[1], "\n")
  }
  cat("FINISHED PROCESSING FOR GROUP:", group, "\n")
}

cat("\n=====================================================================\n")
cat("ALL GROUPS PROCESSED SUCCESSFULLY. DATA COMBINED INTO:\n", output_file, "\n")
cat("=====================================================================\n")