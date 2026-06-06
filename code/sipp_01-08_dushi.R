library(tidyverse)

# ---------------------------------------------------------------------------
# Function to create pension access and participation variables for SIPP
# ---------------------------------------------------------------------------
process_sipp_wave <- function(sipp_data) {
  
  sipp_data %>%
    # --- Sample restriction: working age and in the earnings universe ---
    filter(TAGE >= 21 & TAGE <= 64,
           EARPUNV == 1) %>%
    
    # --- Primary-job worker class ---
    mutate(
      hrs = pmax(EJBHRS1, EJBHRS2),
      class_main = case_when(
        # reclassify as self-employed if own-business hours dominate
        EHRSBS1 > hrs | EHRSBS2 > hrs ~ 7,
        # use job-2 class when job 2 has more hours than job 1
        EJBHRS2 > EJBHRS1             ~ ECLWRK2,
        TRUE                          ~ ECLWRK1
      )
    ) %>%
    
    # --- Restrict to private-sector wage and salary workers ---
    filter(class_main %in% 1:2) %>%
    
    # --- Access and participation variables ---
    mutate(
      # === ACCESS ===
      # offered any tax-advantaged plan: an employer pension (EPENSNYN) OR a
      # tax-deferred plan captured by the E3TAXDEF follow-up. Anyone without
      # either positive indicator is "no access" (mirrors Stata's OR logic).
      ANY_RETIREMENT_ACCESS = case_when(
        EPENSNYN == 1 | E3TAXDEF == 1 ~ "access",
        TRUE                          ~ "no access"
      ),
      
      # === PARTICIPATION ===
      # DB: traditional/both plan types, or account-balance plans that are
      # NOT tax-deferred (cash-balance / hybrid)
      has_db = as.integer(
        (E1PENTYP %in% c(1, 3)) | (E1PENTYP == 2 & E1TAXDEF == 2) |
          (E2PENTYP %in% c(1, 3)) | (E2PENTYP == 2 & E2TAXDEF == 2)
      ),
      # DC: contributing to a tax-deferred plan at job 1 or 2, or participating
      # in the third-employer plan
      has_dc = as.integer(
        E1TAXDEF == 1 | E2TAXDEF == 1 | E3PARTIC == 1
      ),
      # participation = any plan (DB or DC)
      participation = as.integer(has_db | has_dc)
    )
}

# Apply to all waves
sipp_2001_w7_proc  <- process_sipp_wave(sipp_2001_w7)
sipp_2004_w7_proc  <- process_sipp_wave(sipp_2004_w7)
sipp_2008_w3_proc  <- process_sipp_wave(sipp_2008_w3)
sipp_2008_w11_proc <- process_sipp_wave(sipp_2008_w11)

# ---------------------------------------------------------------------------
# Function to summarize a wave (unweighted counts and percentages)
# ---------------------------------------------------------------------------
summarize_wave <- function(sipp_data, year, wave) {
  cat("\n=== ", year, " Wave ", wave, " ===\n", sep = "")
  cat("N =", nrow(sipp_data), "\n\n")
  
  cat("Access:\n")
  access_table <- table(sipp_data$ANY_RETIREMENT_ACCESS, useNA = "ifany")
  print(access_table)
  cat("Percentages:\n")
  print(round(prop.table(access_table) * 100, 1))
  
  cat("\nParticipation:\n")
  part_table <- table(sipp_data$participation, useNA = "ifany")
  print(part_table)
  cat("Percentages:\n")
  print(round(prop.table(part_table) * 100, 1))
}

# Summarize all waves
summarize_wave(sipp_2001_w7_proc,  2001,  7)
summarize_wave(sipp_2004_w7_proc,  2004,  7)
summarize_wave(sipp_2008_w3_proc,  2008,  3)
summarize_wave(sipp_2008_w11_proc, 2008, 11)

# ---------------------------------------------------------------------------
# Function to calculate weighted shares
# ---------------------------------------------------------------------------
calculate_weighted_shares <- function(sipp_data, wave_label) {
  
  year <- sipp_data$RHCALYR[1]
  
  sipp_data %>%
    summarise(
      year = year,
      wave = wave_label,
      n    = n(),
      
      # Access share (weighted)
      access_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS == "access"], na.rm = TRUE) /
        sum(WPFINWGT, na.rm = TRUE) * 100,
      
      no_access_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS == "no access"], na.rm = TRUE) /
        sum(WPFINWGT, na.rm = TRUE) * 100,
      
      # Participation share (weighted)
      participate_share = sum(WPFINWGT[participation == 1], na.rm = TRUE) /
        sum(WPFINWGT, na.rm = TRUE) * 100
    )
}

# Calculate for all waves (wave label distinguishes the two 2008 waves)
results_2001     <- calculate_weighted_shares(sipp_2001_w7_proc,  7)
results_2004     <- calculate_weighted_shares(sipp_2004_w7_proc,  7)
results_2008_w3  <- calculate_weighted_shares(sipp_2008_w3_proc,  3)
results_2008_w11 <- calculate_weighted_shares(sipp_2008_w11_proc, 11)

# Combine into one table
historical_results <- bind_rows(
  results_2001,
  results_2004,
  results_2008_w3,
  results_2008_w11
) %>%
  arrange(year, wave)

# Display
print(historical_results)

# Save to CSV
write_csv(historical_results, paste0("output/", Sys.Date(), "_sipp_historical.csv"))