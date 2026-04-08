library(tidyverse)

# Function to create pension access and participation variables for historical SIPP
create_pension_vars <- function(sipp_data) {
  
  sipp_data %>%
    mutate(
      # === ACCESS VARIABLE ===
      ANY_RETIREMENT_ACCESS = case_when(
        # Primary/Secondary Plan Access
        EINCPENS == 1 ~ "access",
        
        EINCPENS == 2 & (ENOINA04 == 1 | ENOINA06 == 1 | ENOINA07 == 1 | 
                           ENOINA08 == 1 | ENOINA10 == 1 | ENOINA11 == 1 | 
                           ENOINA12 == 1 | ENOINA13 == 1) ~ "access",
        
        EINCPENS == 2 & (ENOINA02 == 1 | ENOINA03 == 1 | ENOINA05 == 1 | 
                           ENOINA09 == 1) ~ "eventual access",
        
        EINCPENS == 2 & ENOINA01 == 1 ~ "no access",
        
        # Access via backup questions to verify presence/absence of tax-deferred DC plan
        E3PARTIC == 1 ~ "access",
        
        E3PARTIC == 2 & (ENOINB04 == 1 | ENOINB06 == 1 | ENOINB07 == 1 | 
                           ENOINB08 == 1 | ENOINB10 == 1 | ENOINB11 == 1 | 
                           ENOINB12 == 1 | ENOINB13 == 1) ~ "access",
        
        E3PARTIC == 2 & (ENOINB02 == 1 | ENOINB03 == 1 | ENOINB05 == 1 | 
                           ENOINB09 == 1) ~ "eventual access",
        
        E3PARTIC == 2 & ENOINB01 == 1 ~ "no access",
        
        # No tax-deferred plan available
        EPENSNYN == 2 & E3TAXDEF == 2 ~ "no access",
        
        # Missing cases
        EINCPENS == 2 & ENOINA14 == 1 ~ NA_character_,
        E3PARTIC == 2 & ENOINB14 == 1 ~ NA_character_,
        
        TRUE ~ NA_character_
      ),
      
      # === PARTICIPATION VARIABLES ===
      # Loose participation (included)
      participate = case_when(
        EINCPENS == 1 | E3PARTIC == 1 ~ "Yes",
        TRUE ~ "No"
      ),
      
      # Strict participation (actively contributing)
      participate_active = case_when(
        E1PENCTR == 1 | E2PENCTR == 1 ~ "Yes",
        TRUE ~ "No"
      )
    )
}

# Apply to all four waves
sipp_2001_w7 <- create_pension_vars(sipp_2001_w7)
sipp_2004_w7 <- create_pension_vars(sipp_2004_w7)
sipp_2008_w3 <- create_pension_vars(sipp_2008_w3)
sipp_2008_w11 <- create_pension_vars(sipp_2008_w11)

# Check distributions
cat("=== 2001 Wave 7 ===\n")
cat("Access:\n")
print(table(sipp_2001_w7$ANY_RETIREMENT_ACCESS, useNA = "ifany"))
cat("\nParticipation (loose):\n")
print(table(sipp_2001_w7$participate, useNA = "ifany"))
cat("\nParticipation (active):\n")
print(table(sipp_2001_w7$participate_active, useNA = "ifany"))

cat("\n=== 2004 Wave 7 ===\n")
cat("Access:\n")
print(table(sipp_2004_w7$ANY_RETIREMENT_ACCESS, useNA = "ifany"))
cat("\nParticipation (loose):\n")
print(table(sipp_2004_w7$participate, useNA = "ifany"))
cat("\nParticipation (active):\n")
print(table(sipp_2004_w7$participate_active, useNA = "ifany"))

cat("\n=== 2008 Wave 3 ===\n")
cat("Access:\n")
print(table(sipp_2008_w3$ANY_RETIREMENT_ACCESS, useNA = "ifany"))
cat("\nParticipation (loose):\n")
print(table(sipp_2008_w3$participate, useNA = "ifany"))
cat("\nParticipation (active):\n")
print(table(sipp_2008_w3$participate_active, useNA = "ifany"))

cat("\n=== 2008 Wave 11 ===\n")
cat("Access:\n")
print(table(sipp_2008_w11$ANY_RETIREMENT_ACCESS, useNA = "ifany"))
cat("\nParticipation (loose):\n")
print(table(sipp_2008_w11$participate, useNA = "ifany"))
cat("\nParticipation (active):\n")
print(table(sipp_2008_w11$participate_active, useNA = "ifany"))