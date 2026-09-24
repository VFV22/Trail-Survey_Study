# ============================================================================
# Recompute D-error for the original Ngene design vs. the manually
# rebalanced ("fielded") design.
#
# WHY THIS SCRIPT EXISTS
# -----------------------
# Ngene reports a D-error for the design it generates (0.167848, see the
# "Model | MNL" sheet of "ALM Design - 44.xlsx"). After that design was
# manually edited for attribute-level balance across blocks (see
# "Ngene  Attribute balance.R"), the reported 0.167848 value no longer
# describes the design respondents actually saw. This script recomputes
# D-error directly from the design matrix, using the *same* model
# specification Ngene used to generate the design, so the result is
# directly comparable to the original 0.167848.
#
# MODEL SPECIFICATION (confirmed to match Ngene's "Model | MNL" sheet)
# ----------------------------------------------------------------------
#   - MNL, generic (non-alternative-specific) parameters
#   - J = 2 alternatives per choice task (no opt-out in the efficiency calc)
#   - Habitat quality, Trail condition, Crowding: 3 levels each, dummy-coded
#     against level 1 as the reference (matches Ngene's "d0"/"d1" naming)
#   - Cost: continuous / linear
#   - Zero priors (all "Fixed prior value" = 0 in the Ngene export)
#
# WHY THE ZERO-PRIOR FORMULA WORKS
# ----------------------------------
# At beta = 0, every alternative in a choice task is equally likely
# (p = 1/J). For a binary task (J = 2) the MNL Fisher information
# contribution collapses to a simple closed form:
#     M_task = 0.25 * (x1 - x2) %*% t(x1 - x2)
# where x1, x2 are the coded attribute vectors of the two alternatives.
# Summing over all S tasks gives the design's total information matrix M.
#     D-error = det(M)^(-1/K),   K = number of parameters
# This is validated below: it reproduces Ngene's reported 0.167848 exactly.
# ============================================================================

library(tidyverse)
library(readxl)

# ---- 1. Load the original Ngene design ------------------------------------
design <- read_excel("0. Data/ALM Design - 44.xlsx") %>%
  mutate(across(c(`Choice situation`, Block,
                  alt1.forest_alt, alt1.trail_alt, alt1.crowd_alt, alt1.cost_alt,
                  alt2.forest_alt, alt2.trail_alt, alt2.crowd_alt, alt2.cost_alt),
                as.numeric)) %>%
  rename(
    task       = `Choice situation`, block = Block,
    A1_habitat = alt1.forest_alt, A1_trail = alt1.trail_alt,
    A1_crowd   = alt1.crowd_alt,  A1_cost  = alt1.cost_alt,
    A2_habitat = alt2.forest_alt, A2_trail = alt2.trail_alt,
    A2_crowd   = alt2.crowd_alt,  A2_cost  = alt2.cost_alt
  )

# ---- 2. Dummy-coding helper (base = level 1) -------------------------------
dummy_code <- function(level) c(d0 = as.numeric(level == 2),
                                 d1 = as.numeric(level == 3))

code_alt <- function(habitat, trail, crowd, cost) {
  c(dummy_code(habitat), dummy_code(trail), dummy_code(crowd), cost = cost)
}

# ---- 3. Zero-prior MNL D-error for a design data frame ---------------------
d_error <- function(df, label = "") {
  K <- 7  # 2 habitat + 2 trail + 2 crowd + 1 cost
  M <- matrix(0, K, K)
  for (i in seq_len(nrow(df))) {
    r  <- df[i, ]
    x1 <- code_alt(r$A1_habitat, r$A1_trail, r$A1_crowd, r$A1_cost)
    x2 <- code_alt(r$A2_habitat, r$A2_trail, r$A2_crowd, r$A2_cost)
    d  <- matrix(x1 - x2, ncol = 1)
    M  <- M + 0.25 * (d %*% t(d))     # J = 2, zero-prior MNL contribution
  }
  detM <- det(M)
  Derr <- detM^(-1 / K)
  cat(sprintf("%-58s det(M) = %10.4g   D-error = %.6f\n", label, detM, Derr))
  invisible(Derr)
}

# ---- 4. Attribute-level balance diagnostic (sum of squared deviations) -----
imbalance_score <- function(df) {
  long <- bind_rows(
    df %>% transmute(block, habitat = A1_habitat, trail = A1_trail, crowd = A1_crowd, cost = A1_cost),
    df %>% transmute(block, habitat = A2_habitat, trail = A2_trail, crowd = A2_crowd, cost = A2_cost)
  ) %>%
    pivot_longer(c(habitat, trail, crowd, cost), names_to = "attribute", values_to = "level")

  counts <- long %>% count(block, attribute, level, name = "count")
  totals <- counts %>%
    group_by(attribute, level) %>%
    summarise(global_total = sum(count), .groups = "drop") %>%
    mutate(ideal_per_block = global_total / n_distinct(df$block))

  counts %>%
    left_join(totals, by = c("attribute", "level")) %>%
    mutate(sq_dev = (count - ideal_per_block)^2) %>%
    summarise(score = sum(sq_dev)) %>%
    pull(score)
}

# ---- 5. Validate against Ngene's reported D-error --------------------------
cat("=== Validation against Ngene's reported D-error (0.167848) ===\n")
d_orig <- d_error(design, "Original Ngene design (as generated)")
cat(sprintf("Imbalance score, original design: %.2f\n\n", imbalance_score(design)))
# Both should match: D-error = 0.167848, imbalance score ~= 19.9

# ---- 6. Apply the manual attribute-balance edits ---------------------------
# (identical to Section 5 of "Ngene  Attribute balance.R")
edited <- design
edited$A1_crowd[edited$task == 11]   <- 2
edited$A2_habitat[edited$task == 4]  <- 1
edited$A2_habitat[edited$task == 6]  <- 2
edited$A1_habitat[edited$task == 6]  <- 2
edited$A2_habitat[edited$task == 14] <- 1
edited$A1_cost[edited$task == 4]     <- 2
edited$A1_cost[edited$task == 9]     <- 50
edited$A2_cost[edited$task == 13]    <- 20
edited$A2_trail[edited$task == 10]   <- 2
edited$A1_trail[edited$task == 14]   <- 2

cat("=== After manual attribute-balance edits (original $ coding) ===\n")
d_bal <- d_error(edited, "Balance-adjusted design")
cat(sprintf("Imbalance score, after edits: %.2f\n\n", imbalance_score(edited)))

# ---- 7. Apply the pilot-informed cost re-levelling -> the design as fielded
# (identical to Section 9 of "Ngene  Attribute balance.R")
final <- edited %>%
  mutate(
    A1_cost = case_when(A1_cost == 50 ~ 80, A1_cost == 20 ~ 40, A1_cost == 10 ~ 20,
                         A1_cost == 5  ~ 10, A1_cost == 2  ~ 5,  TRUE ~ A1_cost),
    A2_cost = case_when(A2_cost == 50 ~ 80, A2_cost == 20 ~ 40, A2_cost == 10 ~ 20,
                         A2_cost == 5  ~ 10, A2_cost == 2  ~ 5,  TRUE ~ A2_cost)
  )

cat("=== Fully fielded design (balance edits + cost re-levelling) ===\n")
d_final <- d_error(final, "Fielded design")
cat("\n")
d
