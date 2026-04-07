# Clear environment
rm(list = ls())

getwd()

library(haven)
library(dplyr)
library(plotly)
library(tidyr)
library(openxlsx)
library(ggplot2)

# Initialize results table
results <- data.frame(
  year = integer(),
  access_share = numeric(),
  access_or_eventual_share = numeric(),
  participate_share = numeric()
)

years <- 2021:2024

for (yr in years) {
  
  cat("Processing year:", yr, "\n")
  
  # Read ONLY the needed columns directly — avoids loading the full file
  keep_cols <- c("SHHADID", "SPANEL", "SSUID", "SWAVE", "PNUM", "MONTHCODE", "WPFINWGT",
                 "TAGE", "EEDUC", "ESEX", "ERACE", "TMETRO_INTV", "EJB1_JBORSE", "EJB1_CLWRK",
                 "TPTOTINC", "EMJOB_401", "EMJOB_IRA", "EMJOB_PEN", "EOWN_THR401",
                 "EOWN_IRAKEO", "EOWN_PENSION", "ESCNTYN_401", "EECNTYN_401", "EORIGIN",
                 "TJB1_JOBHRS1", "ESCNTYN_PEN", "ESCNTYN_IRA", "EECNTYN_IRA", "EPJOB_401", 
                 "EPJOB_IRA", "EPJOB_PEN", "EPENSNYN", "EINCPENS", "RMESR", "RSNAP_YRYN", 
                 "TJB1_OCC", "THNETWORTH", "TNETWORTH")
  
  # Read the data
  sipp <- read_sas(paste0("data/pu", yr, ".sas7bdat"), col_select = all_of(keep_cols))
  
  sipp <- sipp %>%
    filter(MONTHCODE == 12) %>% # Retirement data is collected in December, so we can drop all other months here
    mutate(
      EDUCATION = case_when(
        EEDUC >=31 & EEDUC <= 39 ~ "High School or less",
        EEDUC >= 40 & EEDUC <=42 ~ "Some college",
        EEDUC >= 43 & EEDUC <= 46 ~ "Bachelor's degree or higher",
        TRUE ~ "Missing"
      ),
      SEX = case_when(
        ESEX == 1 ~ "Male",
        ESEX == 2 ~ "Female",
        TRUE ~ "Missing"
      ),
      RACE = case_when(
        ERACE == 1 & EORIGIN ==2 ~ "Non-Hispanic White",
        ERACE == 2 & EORIGIN ==2 ~ "Non-Hispanic Black",
        ERACE == 3 & EORIGIN ==2 ~ "Asian",
        EORIGIN == 1 ~ "Hispanic",
        ERACE == 4 & EORIGIN == 2 ~ "Mixed/Other",
        TRUE ~ "Missing"
      ),
      EMPLOYMENT_TYPE = case_when(
        EJB1_JBORSE == 1 ~ "Employer", # Works for an employer
        EJB1_JBORSE == 2 ~ "Self-employed (owns a business)",
        EJB1_JBORSE == 3 ~ "Other work arrangement",
        TRUE ~ "Missing"
      ),
      CLASS_OF_WORKER = case_when(
        EJB1_CLWRK == 1 ~ "Federal government employee",
        EJB1_CLWRK == 2 ~ "Active duty military",
        EJB1_CLWRK == 3 ~ "State government employee",
        EJB1_CLWRK == 4 ~ "Local government employee",
        EJB1_CLWRK == 5 ~ "Employee of a private, for-profit company",
        EJB1_CLWRK == 6 ~ "Employee of a private, not-for-profit company",
        EJB1_CLWRK == 7 ~ "Self-employed in own incorporated business",
        EJB1_CLWRK == 8 ~ "Self-employed in own not incorporated business",
        TRUE ~ "Missing"
      ),
      TOTYEARINC = TPTOTINC*12,
      ANY_RETIREMENT_ACCESS = case_when(
        # Check for access across all three types
        # 401(k)/Thrift access
        (EOWN_THR401 == 1 & EMJOB_401 == 1) |
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 1) |
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        (EOWN_THR401 == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        # IRA/Keogh access
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 1) |
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 1) |
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        (EOWN_IRAKEO == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        # Pension access
        (EOWN_PENSION == 1 & EMJOB_PEN == 1) |
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 1) |
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        (EOWN_PENSION == 2 & EPENSNYN == 1 & EINCPENS == 1) ~ "access",
        
        # Check for "eventual access" (only if no "access" found above)
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_THR401 == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_IRAKEO == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_PENSION == 2 & EPENSNYN == 1 & EINCPENS == 2) ~ "eventual access",
        
        # Check for "no access" (all three types must be "no access")
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 2 & EPENSNYN == 2) |
        (EOWN_THR401 == 2 & EPENSNYN == 2) &
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 2 & EPENSNYN == 2) |
        (EOWN_IRAKEO == 2 & EPENSNYN == 2) &
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 2 & EPENSNYN == 2) |
        (EOWN_PENSION == 2 & EPENSNYN == 2) ~ "no access",
        
        TRUE ~ "Missing"
      ),
      PARTICIPATING = case_when(
        ESCNTYN_401 == 1 ~ "Yes", # During the reference period, respondent contributed to the 401k, 403b, 503b, or Thrift Savings Plan account(s) provided through their main employer or business.
        ESCNTYN_PEN == 1 ~ "Yes", # During the reference period, respondent contributed to the defined-benefit or cash balance plan(s) provided through their main employer or business.
        ESCNTYN_IRA == 1 ~ "Yes", # During the reference period, respondent contributed to the IRA or Keogh account(s) provided through their main employer or business.
        ESCNTYN_401 == 2 ~ "No",
        ESCNTYN_PEN == 2 ~ "No",
        ESCNTYN_IRA == 2 ~ "No",
        EOWN_THR401  == 2 ~ "No",
        EOWN_IRAKEO  == 2 ~ "No",
        EOWN_PENSION == 2 ~ "No",
        TRUE ~ "Missing"
      ),
      # This isn't necessarily matching--just employer contributions
      # MATCHING = case_when(
      #   EECNTYN_401 == 1 ~ "Yes", # Main employer or business contributed to respondent's 401k, 403b, 503b, or Thrift Savings Plan account(s) during the reference period.
      #   EECNTYN_IRA == 1  ~ "Yes", # Main employer or business contributed to respondent's IRA or Keogh account(s) during the reference period.
      #   EECNTYN_401 == 2 ~ "No",
      #   EECNTYN_IRA == 2 ~ "No",
      #   EOWN_THR401  == 2 ~ "No",
      #   EOWN_IRAKEO  == 2 ~ "No",
      #   EOWN_PENSION == 2 ~ "No",
      #   # is.na(EECNTYN_401) ~ "No",
      #   TRUE ~ "Missing"
      # ),
      # METRO_STATUS = case_when(
      #   TMETRO_INTV == 1 ~ "Metropolitan area",
      #   TMETRO_INTV == 2 ~ "Nonmetropolitan area",
      #   TMETRO_INTV == 3 ~ "Not identified",
      #   TRUE ~ NA
      # ),
      FULL_PART_TIME = case_when( # Define full time workers as those working at least 35 hours
        TJB1_JOBHRS1 >=35 ~ "full time",
        TJB1_JOBHRS1 >0 & TJB1_JOBHRS1< 35 ~ "part time",
        TRUE ~ NA
      ),
      in_age_range = case_when(
        TAGE >= 18 & TAGE <= 65 ~ "yes",
        TAGE >= 0 & TAGE <= 17 ~ "no",
        TAGE >= 66 & TAGE <= 100 ~ "no",
        TRUE ~ NA 
      ) # 18-65 ages
    )
  
  # Filter to analysis population
  sipp <- sipp %>%
    filter(EMPLOYMENT_TYPE == "Employer") %>%
    filter(CLASS_OF_WORKER %in% c("Employee of a private, for-profit company",
                                  "Employee of a private, not-for-profit company")) %>%
    filter(!is.na(FULL_PART_TIME)) %>%
    filter(TPTOTINC > 0) %>%
    filter(ANY_RETIREMENT_ACCESS != "Missing") %>%
    filter(PARTICIPATING != "Missing") %>%
    #filter(MATCHING != "Missing") %>%
    filter(in_age_range == "yes")
  
  # --- Helper to compute retirement access/participation shares ---
  get_retirement_shares <- function(df) {
    df %>%
      summarise(
        # Access (excludes "eventual access" and "no access")
        access_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS == "access"], na.rm = TRUE) / 
          sum(WPFINWGT, na.rm = TRUE) * 100,
        
        # Access OR Eventual Access (excludes only "no access")
        access_or_eventual_share = sum(WPFINWGT[ANY_RETIREMENT_ACCESS %in% c("access", "eventual access")], na.rm = TRUE) / 
          sum(WPFINWGT, na.rm = TRUE) * 100,
        
        # Participation
        participate_share = sum(WPFINWGT[PARTICIPATING == "Yes"], na.rm = TRUE) / 
          sum(WPFINWGT, na.rm = TRUE) * 100
      )
  }
  
  # Compute the three shares (no distinction between full- and part-time)
  row <- data.frame(
    year                     = yr,
    access_share             = get_retirement_shares(sipp)$access_share,
    access_or_eventual_share = get_retirement_shares(sipp)$access_or_eventual_share,
    participate_share        = get_retirement_shares(sipp)$participate_share
  )
  
  results <- bind_rows(results, row)
  
  # Clean up everything except the results table and loop variables
  rm(sipp, row)
  gc()
  
  cat("Done with year:", yr, "\n\n")
}

print(results)

# Export to CSV
setwd('~/rprojects/sipp_retirement_timeseries/output/')
write.csv(results, "20260407_sipp_2021-24.csv", row.names = FALSE)


