# ------------------------
# Load required libraries
# ------------------------
library(readr)
library(dplyr)
library(openxlsx)
library(tidyr)

# ------------------------
# Define directories
# ------------------------
input_dir <- "C:/Users/tim4t/OneDrive - Royal Veterinary College/Clofazimine_project/Parietal bone/2D results"
output_dir <- "C:/Users/tim4t/OneDrive - Royal Veterinary College/Clofazimine_project/Parietal bone/Cleaned 2D data for graphs and heatmaps"

# Create the output directory if it doesn't exist.
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get a list of all CSV files in the folder.
csv_files <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize counters for each type
control_count <- 0
treated_count <- 0

# Define the custom text_to_columns function
text_to_columns <- function(text, delimiter = ",", header = TRUE) {
  # Split each line on the delimiter
  split_list <- strsplit(text, delimiter, fixed = TRUE)
  # Determine the maximum number of columns among rows
  max_cols <- max(sapply(split_list, length))
  # Pad rows with fewer fields so that all rows have the same length
  split_list <- lapply(split_list, function(x) {
    length(x) <- max_cols
    x
  })
  # Convert the list into a data frame
  df <- as.data.frame(do.call(rbind, split_list), stringsAsFactors = FALSE)
  # If header is TRUE, use the first row as header names and drop it from the data.
  if (header) {
    names(df) <- df[1, ]
    df <- df[-1, , drop = FALSE]
  }
  return(df)
}

# Loop through each CSV file
for (input_file in csv_files) {
  
  # Get the base name of the file
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  # Determine the output file name based on patterns in the input file name
  if (grepl("control|control", base_name, ignore.case = TRUE)) {
    control_count <- control_count + 1
    clean_name <- paste0("control", control_count, "_clean.csv")
  } else if (grepl("treated|treated", base_name, ignore.case = TRUE)) {
    treated_count <- treated_count + 1
    clean_name <- paste0("treated", treated_count, "_clean.csv")
  } else {
    clean_name <- paste0(base_name, "_clean.csv")
  }
  
  clean_file <- file.path(output_dir, clean_name)
  
  # ------------------------
  # PART 1: Read & Clean the Input File
  # ------------------------
  lines <- readLines(input_file, encoding = "UTF-8")
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "")
  
  # Search for the header line using key tokens.
  pattern <- "Pos\\.Z.*Obj\\.N.*T\\.Ar.*B\\.Ar"
  header_index <- grep(pattern, lines, ignore.case = TRUE, useBytes = TRUE)
  if (length(header_index) == 0) {
    cat("Header line not found in file:", input_file, "\n")
    next  # skip this file if header is not found
  }
  header_index <- header_index[1]
  
  # Keep lines starting from the header 
  remaining_lines <- lines[header_index:length(lines)]
  
  # Remove the line immediately after the header (if it exists)
  if (length(remaining_lines) >= 2) {
    remaining_lines <- remaining_lines[-2]
  }
  
  # Remove all lines starting from (and including) the first blank line.
  blank_index <- which(trimws(remaining_lines) == "")[1]
  if (!is.na(blank_index)) {
    remaining_lines <- remaining_lines[1:(blank_index - 1)]
  }
  
  # ------------------------
  # PART 2: Split the Comma-Delimited Text into Columns
  # ------------------------
  df <- text_to_columns(remaining_lines, delimiter = ",", header = TRUE)
  
  # ------------------------
  # PART 3: Subset the Data Frame and Rename Columns
  # ------------------------
  # Keep only columns 1 and 2, and any columns with specified headers
  cols_to_keep <- (seq_along(df) %in% c(1, 2)) | (names(df) %in% c("T.Ar", "B.Ar", "B.Ar/T.Ar", "T.Pm", "B.Pm", "B.Pm/B.Ar", "Po(tot)","Tb.Th(pl)"))
  df <- df[, cols_to_keep, drop = FALSE]
  
  # Rename specific columns
  colnames(df) <- gsub("B.Ar/T.Ar", "B.ArT.Ar", colnames(df))
  colnames(df) <- gsub("B.Pm/B.Ar", "B.PmB.Ar", colnames(df))
  colnames(df) <- gsub("Po(tot)", "Porosity", colnames(df))
  colnames(df) <- gsub("Tb.Th\\(pl\\)", "Tb.Th", colnames(df))

  # ------------------------
  # PART 4: Insert New Columns
  # ------------------------
  # (a) Insert a new first column with row numbers.
  row_numbers <- seq_len(nrow(df))
  df <- cbind(row_numbers, df)
  
  # (b) Insert 2 new columns between the column with header "Pos.Z" and the column with header "T.Ar"
  #     Since we inserted row_numbers, "Pos.Z" should be in column 2.
  pos_index <- which(names(df) == "Pos.Z")
  tar_index <- which(names(df) == "T.Ar")
  if (length(pos_index) == 0 || length(tar_index) == 0) {
    cat("Required columns ('Pos.Z' and 'T.Ar') not found in file:", input_file, "\n")
    next  # Skip file if headings are missing
  }
  
  # New column "Slice": duplicate the row_numbers (the new first column)
  new_slice <- df[, 1]
  # New column "percent_of_length": initialize with NA (will be updated next)
  new_percent <- rep(NA, nrow(df))
  
  # Split the data frame into two parts:
  df_left <- df[, 1:pos_index, drop = FALSE]
  df_right <- df[, (pos_index + 1):ncol(df), drop = FALSE]
  
  # Combine: left part, then new columns, then right part.
  df <- cbind(df_left,
              Slice = new_slice,
              percent_of_length = new_percent,
              df_right)
  
  # ------------------------
  # PART 5: Update Headers and Calculate percent_of_length
  # ------------------------
  # Override the first two column headers with blank strings.
  colnames(df)[1:2] <- c("", "")
  
  # Compute percent_of_length:
  num_data <- nrow(df)
  df$percent_of_length <- ((1:num_data - 1) / (num_data - 1)) * 100
  
  # ------------------------
  # PART 6: Write the Final Data Frame to a CSV File
  # ------------------------
  write.csv(df, file = clean_file, row.names = FALSE)
  
  cat("Processed file:", input_file, "\nOutput written to:", clean_file, "\n\n")
}