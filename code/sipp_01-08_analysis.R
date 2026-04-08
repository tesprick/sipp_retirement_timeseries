library(tidyverse)

# Function to create pension access and participation variables for historical SIPP
process_sipp_wave <- function(sipp_data) {
  
  sipp_data %>%
    # Filter to employed population
    filter(RMESR %in% 1:5) %>%
    
    mutate(
      # Employment type (employee vs self-employed)
      employment_type = case_when(
        RMNJBBS == 2 ~ "self_employed",              # Business owner (main earnings)
        RMNJBBS == 1 & EBUSCNTR >= 1 ~ "self_employed",  # Has job but also owns business
        RMNJBBS == 1 & ECLWRK1 %in% 1:5 ~ "employee",    # Job, employee
        RMNJBBS == 1 & ECLWRK1 == 6 ~ "family_worker",   # Family worker
        EBUSCNTR >= 1 & EBUSCNTR <= 25 ~ "self_employed", # Fallback to EBUSCNTR
        EBUSCNTR == 0 ~ "contingent",
        EBUSCNTR == -1 & ECLWRK1 %in% 1:5 ~ "employee",
        EBUSCNTR == -1 & ECLWRK1 == 6 ~ "family_worker",
        TRUE ~ NA_character_
      ),
      
      # Full-time vs part-time
      FULL_PART_TIME = case_when(
        RMHRSWK == 1 ~ "full time",
        RMHRSWK %in% 2:6 ~ "part time",
        TRUE ~ NA_character_
      )
    ) %>%
    
    filter(
      employment_type == "employee",
      ECLWRK1 %in% 1:2,
      TAGE >= 21 & TAGE <= 65,
      FULL_PART_TIME == "full time",
      TPTOTINC > 0
    ) %>%
    
    mutate(
      # === ACCESS VARIABLE ===
      ANY_RETIREMENT_ACCESS = case_when(
        EINCPENS == 1 ~ "access",
        
        EINCPENS == 2 & (ENOINA04 == 1 | ENOINA06 == 1 | ENOINA07 == 1 | 
                           ENOINA08 == 1 | ENOINA10 == 1 | ENOINA11 == 1 | 
                           ENOINA12 == 1 | ENOINA13 == 1) ~ "access",
        
        EINCPENS == 2 & (ENOINA02 == 1 | ENOINA03 == 1 | ENOINA05 == 1 | 
                           ENOINA09 == 1) ~ "eventual access",
        
        EINCPENS == 2 & (ENOINA01 == 1 | ENOINA14 == 1) ~ "no access",
        
        E3PARTIC == 1 ~ "access",
        
        E3PARTIC == 2 & (ENOINB04 == 1 | ENOINB06 == 1 | ENOINB07 == 1 | 
                           ENOINB08 == 1 | ENOINB10 == 1 | ENOINB11 == 1 | 
                           ENOINB12 == 1 | ENOINB13 == 1) ~ "access",
        
        E3PARTIC == 2 & (ENOINB02 == 1 | ENOINB03 == 1 | ENOINB05 == 1 | 
                           ENOINB09 == 1) ~ "eventual access",
        
        E3PARTIC == 2 & (ENOINB01 == 1 | ENOINB14 == 1) ~ "no access",
        
        EPENSNYN == 2 & E3TAXDEF == 2 ~ "no access",
        
        TRUE ~ NA_character_
      ),
      
      # === PARTICIPATION VARIABLES ===
      participate = case_when(
        EINCPENS == 1 | E3PARTIC == 1 ~ "Yes",
        TRUE ~ "No"
      ),
      
      participate_active = case_when(
        E1PENCTR == 1 | E2PENCTR == 1 ~ "Yes",
        TRUE ~ "No"
      )
    )
}

# Apply to all waves
sipp_2001_w7_proc <- process_sipp_wave(sipp_2001_w7)
sipp_2004_w7_proc <- process_sipp_wave(sipp_2004_w7)
sipp_2008_w3_proc <- process_sipp_wave(sipp_2008_w3)
sipp_2008_w11_proc <- process_sipp_wave(sipp_2008_w11)

# Function to summarize a wave
summarize_wave <- function(sipp_data, year, wave) {
  cat("\n=== ", year, " Wave ", wave, " ===\n", sep = "")
  cat("N =", nrow(sipp_data), "\n\n")
  
  cat("Access:\n")
  access_table <- table(sipp_data$ANY_RETIREMENT_ACCESS, useNA = "ifany")
  print(access_table)
  cat("Percentages:\n")
  print(round(prop.table(access_table) * 100, 1))
  
  cat("\nParticipation (loose):\n")
  part_table <- table(sipp_data$participate, useNA = "ifany")
  print(part_table)
  cat("Percentages:\n")
  print(round(prop.table(part_table) * 100, 1))
  
  cat("\nParticipation (active):\n")
  active_table <- table(sipp_data$participate_active, useNA = "ifany")
  print(active_table)
  cat("Percentages:\n")
  print(round(prop.table(active_table) * 100, 1))
}

# Summarize all waves
summarize_wave(sipp_2001_w7_proc, 2001, 7)
summarize_wave(sipp_2004_w7_proc, 2004, 7)
summarize_wave(sipp_2008_w3_proc, 2008, 3)
summarize_wave(sipp_2008_w11_proc, 2008, 11)

# Function to calculate weighted shares
calculate_weighted_shares <- function(sipp_data) {
  
  # Get calendar year from RHCALYR
  year <- sipp_data$RHCALYR[1]
  
  sipp_data %>%
    summarise(
      year = year,
      n = n(),
      
      # Access shares (weighted)
      access_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS == "access"], na.rm = TRUE) / 
        sum(WPFINWGT, na.rm = TRUE) * 100,
      
      access_or_eventual_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS %in% c("access", "eventual access")], na.rm = TRUE) / 
        sum(WPFINWGT, na.rm = TRUE) * 100,
      
      no_access_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS == "no access"], na.rm = TRUE) / 
        sum(WPFINWGT, na.rm = TRUE) * 100,
      
      # Participation shares (weighted)
      participate_loose_share = sum(WPFINWGT[participate == "Yes"], na.rm = TRUE) / 
        sum(WPFINWGT, na.rm = TRUE) * 100,
      
      participate_active_share = sum(WPFINWGT[participate_active == "Yes"], na.rm = TRUE) / 
        sum(WPFINWGT, na.rm = TRUE) * 100
    )
}

# Calculate for all waves
results_2001 <- calculate_weighted_shares(sipp_2001_w7_proc)
results_2004 <- calculate_weighted_shares(sipp_2004_w7_proc)
results_2008_w3 <- calculate_weighted_shares(sipp_2008_w3_proc)
results_2008_w11 <- calculate_weighted_shares(sipp_2008_w11_proc)

# Combine into one table
historical_results <- bind_rows(
  results_2001,
  results_2004,
  results_2008_w3,
  results_2008_w11
) %>%
  arrange(year)

# Display
print(historical_results)

# Save to CSV
write_csv(historical_results, paste0("output/", Sys.Date(), "_sipp_historical.csv"))

# Pretty print with formatting
historical_results %>%
  mutate(across(where(is.numeric) & !n, ~round(.x, 1))) %>%
  print()