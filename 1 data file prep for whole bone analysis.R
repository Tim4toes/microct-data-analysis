# =========================================================
# SECTION 1: Define Directories and Groups
# =========================================================
input_dir <- "E:/test/2d results"
output_dir <- "E:/test/cleaned 2d data"

# Define the exact names of your treatment groups
# To change experiments or add groups, ONLY update this vector.
treatment_groups <- c("group1", "group2", "group3")

# If you ever have overlapping names (like group1 and group10, or "Dose" and "LowDose"), simply put the longer, more specific name first in your list:
# treatment_groups <- c("group10", "group1", ...) or c("LowDose", "Dose").

# Create the output directory if it doesn't exist.
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =========================================================
# SECTION 2: Automated Group Tracking
# =========================================================
# Create a dynamic list to track file counts for each group
group_counters <- setNames(as.list(rep(0, length(treatment_groups))), treatment_groups)

# Get a list of all CSV files in the folder.
csv_files <- list.files(path = input_dir, pattern = "\\.csv$", full.names = TRUE)

# =========================================================
# SECTION 3: Main Processing Loop
# =========================================================
for (input_file in csv_files) {
  
  # Get the base name of the file
  base_name <- tools::file_path_sans_ext(basename(input_file))
  clean_name <- paste0(base_name, "_clean.csv") # Default fallback name
  
  # Check the filename against the defined treatment groups
  for (group in treatment_groups) {
    if (grepl(group, base_name, ignore.case = TRUE)) {
      # Increment the specific counter for this group
      group_counters[[group]] <- group_counters[[group]] + 1
      # Format: "groupName_1_clean.csv"
      clean_name <- paste0(group, "_", group_counters[[group]], "_clean.csv")
      break # Stop checking other groups once a match is found
    }
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
  # PART 2: Read Text into Data Frame
  # ------------------------
  # Replaces the custom text_to_columns function with native base R read.csv
  # fill = TRUE automatically pads uneven rows with NAs
  df <- read.csv(text = remaining_lines, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, check.names = FALSE)
  
  # ------------------------
  # PART 3: Subset the Data Frame and Rename Columns
  # ------------------------
  # Keep only columns 1 and 2, and any columns with specified headers
  cols_to_keep <- (seq_along(df) %in% c(1, 2)) | (names(df) %in% c("T.Ar", "B.Ar", "B.Ar/T.Ar", "T.Pm", "B.Pm", "B.Pm/B.Ar", "Po(tot)", "MMI(polar)", "Tb.Th(pl)"))
  df <- df[, cols_to_keep, drop = FALSE]
  
  # Rename specific columns using a named vector for efficiency
  replacements <- c("B.Ar/T.Ar" = "B.ArT.Ar", 
                    "B.Pm/B.Ar" = "B.PmB.Ar", 
                    "Po(tot)" = "Po.total", 
                    "MMI(polar)" = "MMI.polar", 
                    "Tb.Th(pl)" = "Tb.Th")
  
  for (old_name in names(replacements)) {
    names(df)[names(df) == old_name] <- replacements[old_name]
  }
  
  # ------------------------
  # PART 4: Insert New Columns
  # ------------------------
  # Create a row numbers column and slice column
  row_numbers <- 1:nrow(df)
  
  # Identify positions to split the dataframe
  pos_index <- which(names(df) == "Pos.Z")
  
  if (length(pos_index) == 0 || !("T.Ar" %in% names(df))) {
    cat("Required columns ('Pos.Z' and 'T.Ar') not found in file:", input_file, "\n")
    next  # Skip file if headings are missing
  }
  
  # Calculate percent_of_length with protection against zero-division (1-row datasets)
  num_data <- nrow(df)
  new_percent <- if (num_data > 1) {
    ((1:num_data - 1) / (num_data - 1)) * 100
  } else {
    0
  }
  
  # Reconstruct the dataframe with new columns inserted
  df <- cbind(row_numbers = row_numbers,
              df[, 1:pos_index, drop = FALSE],
              Slice = row_numbers,
              percent_of_length = new_percent,
              df[, (pos_index + 1):ncol(df), drop = FALSE])
  
  # ------------------------
  # PART 5: Update Headers
  # ------------------------
  # Override the first two column headers with blank strings.
  colnames(df)[1:2] <- c("", "")
  
  # ------------------------
  # PART 6: Write the Final Data Frame to a CSV File
  # ------------------------
  write.csv(df, file = clean_file, row.names = FALSE)
  
  cat("Processed file:", input_file, "\nOutput written to:", clean_file, "\n\n")
}