# ------------------------
# Define directories
# ------------------------
input_dir <- "C:/Users/tim4t/OneDrive - Royal Veterinary College/Clofazimine_project/Parietal bone/2D results"
output_dir <- "C:/Users/tim4t/OneDrive - Royal Veterinary College/Clofazimine_project/Parietal bone/Cleaned 2D data for graphs and heatmaps"

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get a list of all CSV files in the folder
csv_files <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize counters for each type
control_count <- 0
treated_count <- 0

# Loop through each CSV file
for (input_file in csv_files) {
  
  # Get the base name of the file
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  # Determine the output file name
  if (grepl("control", base_name, ignore.case = TRUE)) {
    control_count <- control_count + 1
    clean_name <- paste0("control", control_count, "_clean.csv")
  } else if (grepl("treated", base_name, ignore.case = TRUE)) {
    treated_count <- treated_count + 1
    clean_name <- paste0("treated", treated_count, "_clean.csv")
  } else {
    clean_name <- paste0(base_name, "_clean.csv")
  }
  
  clean_file <- file.path(output_dir, clean_name)
  
  # ------------------------
  # PART 1: Read & Clean the Input File
  # ------------------------
  # warn = FALSE prevents warnings on incomplete final lines
  lines <- readLines(input_file, encoding = "UTF-8", warn = FALSE)
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "")
  
  # Search for the header line using key tokens
  header_index <- grep("Pos\\.Z.*Obj\\.N.*T\\.Ar.*B\\.Ar", lines, ignore.case = TRUE, useBytes = TRUE)[1]
  
  if (is.na(header_index)) {
    cat("Header line not found in file:", input_file, "\n")
    next  
  }
  
  # Keep lines starting from the header 
  remaining_lines <- lines[header_index:length(lines)]
  
  # Remove the line immediately after the header (if it exists)
  if (length(remaining_lines) >= 2) {
    remaining_lines <- remaining_lines[-2]
  }
  
  # Remove all lines starting from (and including) the first blank line
  blank_index <- which(trimws(remaining_lines) == "")[1]
  if (!is.na(blank_index)) {
    remaining_lines <- remaining_lines[1:(blank_index - 1)]
  }
  
  # ------------------------
  # PART 2: Optimized Data Frame Conversion
  # ------------------------
  # Using read.csv with fill=TRUE natively handles uneven columns exactly like text_to_columns did.
  df <- read.csv(text = remaining_lines, header = TRUE, sep = ",", 
                 stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
  
  # ------------------------
  # PART 3: Subset the Data Frame and Rename Columns
  # ------------------------
  cols_to_keep <- (seq_along(df) %in% c(1, 2)) | (names(df) %in% c("T.Ar", "B.Ar", "B.Ar/T.Ar", "T.Pm", "B.Pm", "B.Pm/B.Ar", "Po(tot)", "Tb.Th(pl)"))
  df <- df[, cols_to_keep, drop = FALSE]
  
  # Rename specific columns using base R
  names(df) <- gsub("B\\.Ar/T\\.Ar", "B.ArT.Ar", names(df))
  names(df) <- gsub("B\\.Pm/B\\.Ar", "B.PmB.Ar", names(df))
  names(df) <- gsub("Po\\(tot\\)", "Porosity", names(df))
  names(df) <- gsub("Tb\\.Th\\(pl\\)", "Tb.Th", names(df))

  # ------------------------
  # PART 4 & 5: Insert Columns & Calculate Percentage
  # ------------------------
  num_data <- nrow(df)
  row_numbers <- 1:num_data
  percent_of_length <- ((row_numbers - 1) / (num_data - 1)) * 100
  
  # Bind row numbers first to match original logic and index shifting
  df <- cbind(row_numbers, df)
  
  pos_index <- which(names(df) == "Pos.Z")[1]
  if (is.na(pos_index)) {
    cat("Required column 'Pos.Z' not found in file:", input_file, "\n")
    next 
  }
  
  # Rebuild dataframe with new columns inserted natively
  df <- cbind(
    df[, 1:pos_index, drop = FALSE],
    Slice = row_numbers,
    percent_of_length = percent_of_length,
    df[, (pos_index + 1):ncol(df), drop = FALSE]
  )
  
  # Override the first two column headers with blank strings
  colnames(df)[1:2] <- c("", "")
  
  # ------------------------
  # PART 6: Write the Final Data Frame
  # ------------------------
  write.csv(df, file = clean_file, row.names = FALSE)
  
  cat("Processed file:", input_file, "\nOutput written to:", clean_file, "\n\n")
}