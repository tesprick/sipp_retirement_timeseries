# sipp_21-24.R
# Retirement plan access and participation, SIPP 2021–2024
#
# Methodology alignment with sipp_01-08_dushi.R:
#   - Age:        21–64
#   - Sample:     private-sector wage and salary workers (for-profit + nonprofit)
#                 matches ECLWRK %in% 1:2 filter in 2001-08
#   - Earnings universe proxy: TJB1_JOBHRS1 > 0 & TPTOTINC > 0
#                 (2001-08 used EARPUNV == 1; no direct equivalent post-2014)
#   - Denominator: full post-filter sample, including "Missing" outcome obs.
#                 In 2001-08 the TRUE catchall in case_when ensures no missing
#                 values exist, so the full sample is always in the denominator.
#                 "Missing" in 2021-24 mostly reflects skip-pattern non-response
#                 (person was never asked because they have no plans), effectively
#                 equivalent to "no access" / not participating.
#   - Weight:     WPFINWGT (same name in both panels)
#   - Reference month: December (MONTHCODE == 12); retirement module collected
#                 annually in December in the post-2014 SIPP design
#
# Bugs fixed vs. original sipp_21-24.R:
#   1. Age range: changed from TAGE 18-65 to 21-64
#   2. PARTICIPATING case_when order: "Yes" conditions were interleaved with
#      "No" conditions, so a person with ESCNTYN_401 == 2 but ESCNTYN_PEN == 1
#      would be misclassified as "No" (case_when returns first match). Fixed by
#      consolidating all "Yes" conditions into one OR expression evaluated first.
#   3. "no access" operator precedence: bare | and & without grouping collapsed
#      to A | (B & C) | (D & E) | F due to R's &-before-| precedence, making the
#      condition fire when ANY ONE plan type showed no access. Fixed by wrapping
#      each plan-type sub-expression in parentheses and joining with &.
#   4. "Missing" filtered from denominator: removed filter(ANY_RETIREMENT_ACCESS
#      != "Missing") and filter(PARTICIPATING != "Missing"), which inflated both
#      shares by shrinking the denominator.
#   5. Output: added no_access_share to match 2001-08 output structure.
#   6. keep_cols: removed variables read but unused in the final analysis
#      (EEDUC, ESEX, ERACE, EORIGIN, TMETRO_INTV, RSNAP_YRYN, TJB1_OCC,
#       THNETWORTH, TNETWORTH, EECNTYN_401, EECNTYN_IRA).
#   7. Removed stale debug line: print(table(sipp_2001_w7$RMHRSWK))

rm(list = ls())

library(haven)
library(tidyverse)

# Initialize results table
results <- data.frame(
  year                     = integer(),
  n                        = integer(),
  access_share             = numeric(),
  access_or_eventual_share = numeric(),
  no_access_share          = numeric(),
  participate_share        = numeric()
)

years <- 2021:2024

for (yr in years) {

  cat("Processing year:", yr, "\n")

  # Read only the columns needed for sample construction and outcome variables.
  # Demographic variables (education, sex, race) retained in keep_cols_extra
  # below if you need them for subgroup cuts; they're not used here.
  keep_cols <- c(
    # Identifiers and weights
    "SSUID", "SPANEL", "SWAVE", "PNUM", "MONTHCODE", "WPFINWGT",
    # Demographics used in sample filters
    "TAGE",
    # Job characteristics used in sample filters
    "EJB1_JBORSE", "EJB1_CLWRK", "TJB1_JOBHRS1",
    # Income (earnings universe proxy)
    "TPTOTINC",
    # Plan ownership — routes respondents through plan-specific questions
    "EOWN_THR401", "EOWN_IRAKEO", "EOWN_PENSION",
    # Plan-job linkage — current main job vs. another/previous job
    "EMJOB_401", "EMJOB_IRA", "EMJOB_PEN",
    "EPJOB_401", "EPJOB_IRA", "EPJOB_PEN",
    # Pension catch-all and current/previous employer distinction
    "EPENSNYN", "EINCPENS",
    # Contribution variables (participation)
    "ESCNTYN_401", "ESCNTYN_PEN", "ESCNTYN_IRA"
  )

  sipp <- read_sas(paste0("data/pu", yr, ".sas7bdat"),
                   col_select = all_of(keep_cols))

  # ── Reference month ────────────────────────────────────────────────────────
  # Retirement module is collected in December in the post-2014 SIPP design.
  # Analogous to filtering to SREFMON == 4 (last reference month) in 2001-08.
  sipp <- sipp %>%
    filter(MONTHCODE == 12)

  # ── Sample construction ────────────────────────────────────────────────────
  sipp <- sipp %>%
    filter(
      # Age 21–64 — matches TAGE >= 21 & TAGE <= 64 in 2001-08
      TAGE >= 21, TAGE <= 64,
      # Wage and salary workers only (excludes self-employed, unpaid)
      # Matches ECLWRK %in% 1:2 in 2001-08 after class_main reclassification
      EJB1_JBORSE == 1,
      # Private sector only: for-profit (5) and nonprofit (6)
      # Matches class_main %in% 1:2 in 2001-08 (private wage-and-salary)
      EJB1_CLWRK %in% c(5, 6),
      # Earnings universe proxy (2001-08 used EARPUNV == 1; not available post-2014)
      TJB1_JOBHRS1 > 0,
      TPTOTINC     > 0
    )

  # ── Outcome variables ──────────────────────────────────────────────────────
  sipp <- sipp %>%
    mutate(

      # ── ACCESS ──────────────────────────────────────────────────────────────
      # Three categories: "access" (current employer), "eventual access"
      # (previous employer only), "no access". "eventual access" has no analog
      # in the 2001-08 design; it is reported separately below.
      #
      # Routing logic: EOWN_* flags whether a plan type exists; EMJOB_* whether
      # it is through the current main job; EPJOB_* whether through another
      # current or previous job; EPENSNYN / EINCPENS provide a pension catch-all
      # for respondents routed away from the plan-specific questions.
      ANY_RETIREMENT_ACCESS = case_when(

        # -- Current employer access (any plan type) --
        (EOWN_THR401 == 1 & EMJOB_401 == 1) |
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 1) |
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        (EOWN_THR401 == 2 & EPENSNYN == 1 & EINCPENS == 1) |

        (EOWN_IRAKEO == 1 & EMJOB_IRA == 1) |
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 1) |
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        (EOWN_IRAKEO == 2 & EPENSNYN == 1 & EINCPENS == 1) |

        (EOWN_PENSION == 1 & EMJOB_PEN == 1) |
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 1) |
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 2 & EPENSNYN == 1 & EINCPENS == 1) |
        (EOWN_PENSION == 2 & EPENSNYN == 1 & EINCPENS == 1) ~ "access",

        # -- Previous employer only (eventual access) --
        (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_THR401 == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_IRAKEO == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 2 & EPENSNYN == 1 & EINCPENS == 2) |
        (EOWN_PENSION == 2 & EPENSNYN == 1 & EINCPENS == 2) ~ "eventual access",

        # -- No access: ALL three plan types must be negative --
        # FIX: original code had bare | and & without grouping; due to R's
        # &-before-| precedence that collapsed to A|(B&C)|(D&E)|F, which fires
        # when any ONE plan type shows no access. Each plan type's sub-expression
        # is now wrapped in parentheses and the three are joined with &.
        (
          (EOWN_THR401 == 1 & EMJOB_401 == 2 & EPJOB_401 == 2 & EPENSNYN == 2) |
          (EOWN_THR401 == 2 & EPENSNYN == 2)
        ) &
        (
          (EOWN_IRAKEO == 1 & EMJOB_IRA == 2 & EPJOB_IRA == 2 & EPENSNYN == 2) |
          (EOWN_IRAKEO == 2 & EPENSNYN == 2)
        ) &
        (
          (EOWN_PENSION == 1 & EMJOB_PEN == 2 & EPJOB_PEN == 2 & EPENSNYN == 2) |
          (EOWN_PENSION == 2 & EPENSNYN == 2)
        ) ~ "no access",

        TRUE ~ "Missing"
      ),

      # ── PARTICIPATION ────────────────────────────────────────────────────────
      # Active contribution during the reference period.
      # Analogous to 2001-08: has_dc = E1TAXDEF==1 | E2TAXDEF==1 | E3PARTIC==1.
      #
      # FIX: original code interleaved "Yes" and "No" conditions across three
      # plan types. Because case_when returns the first match, ESCNTYN_401 == 2
      # (no 401k contribution) would fire before ESCNTYN_PEN == 1 (active
      # pension contribution), misclassifying the person as "No." Fixed by
      # consolidating all "Yes" conditions into one OR expression that is
      # evaluated before any "No" condition.
      PARTICIPATING = case_when(
        # Any contribution → "Yes" (must be checked before all "No" conditions)
        ESCNTYN_401 == 1 | ESCNTYN_PEN == 1 | ESCNTYN_IRA == 1 ~ "Yes",
        # Explicitly recorded non-contribution → "No"
        ESCNTYN_401 == 2 | ESCNTYN_PEN == 2 | ESCNTYN_IRA == 2 ~ "No",
        # Owns no plan of any type → "No"
        EOWN_THR401 == 2 & EOWN_IRAKEO == 2 & EOWN_PENSION == 2 ~ "No",
        TRUE ~ "Missing"
      )

    )

  # ── Diagnostic output (mirrors summarize_wave() in sipp_01-08_dushi.R) ────
  cat("\n===", yr, "— unweighted counts ===\n")
  cat("N =", nrow(sipp), "\n\n")
  cat("Access:\n")
  print(table(sipp$ANY_RETIREMENT_ACCESS, useNA = "ifany"))
  cat("\nParticipation:\n")
  print(table(sipp$PARTICIPATING, useNA = "ifany"))
  cat("\n")

  # ── Weighted shares ────────────────────────────────────────────────────────
  # Denominator = all workers passing the sample filters, including "Missing"
  # outcome observations. This matches the 2001-08 approach, where the TRUE
  # catchall eliminates missing values and the full post-filter sample is always
  # in the denominator. Filtering out "Missing" before dividing would inflate
  # both access and participation shares.
  total_wgt <- sum(sipp$WPFINWGT, na.rm = TRUE)

  row <- data.frame(
    year  = yr,
    n     = nrow(sipp),

    # Access through current employer
    access_share = sum(sipp$WPFINWGT[sipp$ANY_RETIREMENT_ACCESS == "access"],
                       na.rm = TRUE) / total_wgt * 100,

    # Access OR eventual access (plan exists, any employer)
    # No direct analog in 2001-08; reported as a supplementary series.
    access_or_eventual_share = sum(
      sipp$WPFINWGT[sipp$ANY_RETIREMENT_ACCESS %in% c("access", "eventual access")],
      na.rm = TRUE) / total_wgt * 100,

    # No access (added to match 2001-08 output structure)
    no_access_share = sum(sipp$WPFINWGT[sipp$ANY_RETIREMENT_ACCESS == "no access"],
                          na.rm = TRUE) / total_wgt * 100,

    # Active contribution
    participate_share = sum(sipp$WPFINWGT[sipp$PARTICIPATING == "Yes"],
                            na.rm = TRUE) / total_wgt * 100
  )

  results <- bind_rows(results, row)

  rm(sipp, row, total_wgt)
  gc()

  cat("Done with year:", yr, "\n\n")
}

print(results)

write.csv(results,
          paste0("output/", Sys.Date(), "_sipp_2021-24.csv"),
          row.names = FALSE)
