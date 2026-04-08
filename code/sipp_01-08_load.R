library(tidyverse)
library(readr)


# Quick check for regex
read_lines("data/p01puw1.sas", n_max = 20)

# Function to parse SAS input statements and extract column specifications
parse_sas_input <- function(sas_file) {
  
  # Read the SAS input statement file
  sas_lines <- read_lines(sas_file)
  
  # Extract variable definitions between INPUT and ;
  input_start <- which(str_detect(sas_lines, "(?i)^\\s*INPUT"))
  input_end <- which(str_detect(sas_lines, ";"))[which(str_detect(sas_lines, ";")) > input_start][1]
  
  if (length(input_start) == 0 || length(input_end) == 0) {
    stop("Could not find INPUT statement in SAS file")
  }
  
  input_lines <- sas_lines[(input_start + 1):(input_end - 1)]
  
  # Parse each line to extract: variable name, start position, end position
  # Pattern: variable_name (optional $) start-end
  var_pattern <- "^\\s*([A-Z0-9_]+)\\s*(\\$)?\\s+(\\d+)\\s*-\\s*(\\d+)"
  
  col_info <- input_lines %>%
    str_subset(var_pattern) %>%
    str_match(var_pattern) %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names(c("full_match", "var_name", "is_char", "start", "end")) %>%
    mutate(
      start = as.integer(start),
      end = as.integer(end),
      is_char = !is.na(is_char)
    ) %>%
    select(var_name, start, end, is_char)
  
  return(col_info)
}

# Function to read fixed-width file using parsed SAS specifications
read_sipp_fwf <- function(dat_file, sas_file) {
  
  cat("  Parsing SAS file:", basename(sas_file), "\n")
  
  # Parse SAS input statement
  col_info <- parse_sas_input(sas_file)
  
  # Create column specification
  col_spec <- fwf_positions(
    start = col_info$start,
    end = col_info$end,
    col_names = col_info$var_name
  )
  
  # Determine column types (character vs numeric based on $ in SAS)
  col_types <- col_info$var_name %>%
    set_names(col_info$var_name) %>%
    map_chr(~if(col_info$is_char[col_info$var_name == .x]) "c" else "d") %>%
    paste0(collapse = "")
  
  cat("  Reading data file:", basename(dat_file), "\n")
  
  # Read the fixed-width file
  data <- read_fwf(
    dat_file,
    col_positions = col_spec,
    col_types = col_types,
    trim_ws = TRUE,
    na = c("", " ", "NA", ".")
  )
  
  return(data)
}

# Function to load complete SIPP wave (core + topical + weights)
load_sipp_wave <- function(panel, wave) {
  
  # All files are in the data directory
  data_dir <- "data"
  
  # File paths based on actual filenames
  if (panel == "2001") {
    core_dat <- file.path(data_dir, "l01puw7.dat")
    core_sas <- file.path(data_dir, "p01puw1.sas")  # Core SAS uses wave 1 format
    
    topical_dat <- file.path(data_dir, "p01putm7.dat")
    topical_sas <- file.path(data_dir, "p01putm7.sas")
    
    weight_dat <- file.path(data_dir, "rw01w7.dat")
    weight_sas <- file.path(data_dir, "rw01wx.sas")
    
  } else if (panel == "2004") {
    core_dat <- file.path(data_dir, "l04puw7.dat")
    core_sas <- file.path(data_dir, "l04puw1.sas")  # Core SAS uses wave 1 format
    
    topical_dat <- file.path(data_dir, "p04putm7.dat")
    topical_sas <- file.path(data_dir, "p04putm7.sas")
    
    weight_dat <- file.path(data_dir, "rw04w7.dat")
    weight_sas <- file.path(data_dir, "rw04wx.sas")
    
  } else if (panel == "2008" && wave == "03") {
    core_dat <- file.path(data_dir, "l08puw3.dat")
    core_sas <- file.path(data_dir, "l08puw1.sas")  # Core SAS uses wave 1 format
    
    topical_dat <- file.path(data_dir, "p08putm3.dat")
    topical_sas <- file.path(data_dir, "p08putm3.sas")
    
    weight_dat <- file.path(data_dir, "rw08w3.dat")
    weight_sas <- file.path(data_dir, "rw08wx.sas")
    
  } else if (panel == "2008" && wave == "11") {
    core_dat <- file.path(data_dir, "l08puw11.dat")
    core_sas <- file.path(data_dir, "l08puw1.sas")  # Core SAS uses wave 1 format
    
    topical_dat <- file.path(data_dir, "p08putm11.dat")
    topical_sas <- file.path(data_dir, "p08putm11.sas")
    
    weight_dat <- file.path(data_dir, "rw08w11.dat")
    weight_sas <- file.path(data_dir, "rw08wx.sas")
  }
  
  # Read each file using SAS specifications
  cat("\n=== Loading", panel, "Wave", wave, "===\n")
  
  cat("\nCore file:\n")
  core <- read_sipp_fwf(core_dat, core_sas)
  cat("  Loaded:", nrow(core), "rows,", ncol(core), "columns\n")
  
  # Standardize ID variable types to character
  core <- core %>%
    mutate(
      SSUID = as.character(SSUID),
      EPPPNUM = as.character(EPPPNUM),
      SPANEL = as.character(SPANEL),
      SWAVE = as.character(SWAVE)
    )
  
  # Filter core to reference month 4 (last month of wave)
  # This converts from person-month to person-record format
  core <- core %>%
    filter(SREFMON == 4)
  cat("  After filtering to SREFMON == 4:", nrow(core), "rows\n")
  
  cat("\nTopical Module file:\n")
  topical <- read_sipp_fwf(topical_dat, topical_sas)
  cat("  Loaded:", nrow(topical), "rows,", ncol(topical), "columns\n")
  
  # Standardize ID variable types to character
  topical <- topical %>%
    mutate(
      SSUID = as.character(SSUID),
      EPPPNUM = as.character(EPPPNUM),
      SPANEL = as.character(SPANEL),
      SWAVE = as.character(SWAVE)
    )
  
  cat("\nWeight file:\n")
  weights <- read_sipp_fwf(weight_dat, weight_sas)
  cat("  Loaded:", nrow(weights), "rows,", ncol(weights), "columns\n")
  
  # Standardize ID variable types to character
  weights <- weights %>%
    mutate(
      SSUID = as.character(SSUID),
      EPPPNUM = as.character(EPPPNUM),
      SPANEL = as.character(SPANEL),
      SWAVE = as.character(SWAVE)
    )
  
  # Filter weights to reference month 4 to match core
  weights <- weights %>%
    filter(SREFMON == 4)
  cat("  After filtering to SREFMON == 4:", nrow(weights), "rows\n")
  
  # Merge by person identifier
  # Common ID variables across all files: SSUID, EPPPNUM, SPANEL, SWAVE
  # Core and Topical also have: EENTAID, SHHADID
  id_vars_core_topical <- c("SSUID", "EENTAID", "EPPPNUM", "SPANEL", "SWAVE")
  id_vars_weight <- c("SSUID", "EPPPNUM", "SPANEL", "SWAVE")
  
  cat("\nMerging files...\n")
  sipp_merged <- core %>%
    left_join(topical, by = id_vars_core_topical, suffix = c("", "_TM")) %>%
    left_join(weights, by = id_vars_weight, suffix = c("", "_WT"))
  
  cat("  Final dataset:", nrow(sipp_merged), "rows,", ncol(sipp_merged), "columns\n")
  
  return(sipp_merged)
}

# Load all four waves
sipp_2001_w7 <- load_sipp_wave("2001", "07")
sipp_2004_w7 <- load_sipp_wave("2004", "07")
sipp_2008_w3 <- load_sipp_wave("2008", "03")
sipp_2008_w11 <- load_sipp_wave("2008", "11")

# Quick check
cat("\n=== Summary ===\n")
cat("2001 W7:", nrow(sipp_2001_w7), "observations\n")
cat("2004 W7:", nrow(sipp_2004_w7), "observations\n")
cat("2008 W3:", nrow(sipp_2008_w3), "observations\n")
cat("2008 W11:", nrow(sipp_2008_w11), "observations\n")
