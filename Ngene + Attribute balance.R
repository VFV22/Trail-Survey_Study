# ------------------------------
# 0 — User settings / file paths
# ------------------------------
# Edit this path if your Ngene export is in a different location
ngene_file <- "~/Documents/GitHub/Trail-Survey_Study/ALM Design - 44.xlsx"

# ------------------------------
# 0 — Libraries
# ------------------------------
library(tidyverse)   # dplyr, tidyr, ggplot2, readr etc.
library(magrittr)    # pipe (%<>%) used in original script
library(readxl)      # read_excel
library(corrplot)    # corrplot visualization

# ------------------------------
# 0 — Load data
# ------------------------------
original.ngene <- read_excel(ngene_file)
design <- read_excel("~/Downloads/ALM Design - 44.xlsx" )  # we will modify 'design' in-place


# ------------------------------
# 1 — Preprocessing (convert types, rename)
# ------------------------------

# Convert relevant columns to numeric (same columns you used originally)
design %<>%
  mutate(across(c(`Choice situation`, Block,
                  alt1.forest_alt, alt1.trail_alt, alt1.crowd_alt, alt1.cost_alt,
                  alt2.forest_alt, alt2.trail_alt, alt2.crowd_alt, alt2.cost_alt),
                as.numeric))

# Rename the columns for clarity (makes later code more readable)
 design %<>%
  rename(
    block = Block,
    A1_habitat = alt1.forest_alt,
    A1_trail   = alt1.trail_alt,
    A1_crowd   = alt1.crowd_alt,
    A1_cost    = alt1.cost_alt,
    A2_habitat = alt2.forest_alt,
    A2_trail   = alt2.trail_alt,
    A2_crowd   = alt2.crowd_alt,
    A2_cost    = alt2.cost_alt
  )

# ------------------------------
# Helper: reshape to long format
# ------------------------------
# This avoids repeating the same pivoting code multiple times.
reshape_to_long <- function(design_df) {
  design_df %>%
    pivot_longer(
      cols = starts_with("A"),
      names_to = c("alt", "attribute"),
      names_sep = "_",
      values_to = "level"
    ) %>%
    pivot_wider(names_from = attribute, values_from = level)
}

# initial long-format view
design_long <- reshape_to_long(design)

# ------------------------------
# 2 — Initial balance summary & heatmap for original Ngene data 
# ------------------------------
balance_summary_original <- design_long %>%
  pivot_longer(cols = c(habitat, trail, crowd, cost),
               names_to = "attribute", values_to = "level") %>%
  group_by(block, attribute, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(attribute, level, block)

# Heatmap of counts by block x level for each attribute from original Ngene data 
ggplot(balance_summary_original, aes(x = as.factor(block), y = as.factor(level), fill = count)) +
  geom_tile(color = "white") +
  facet_wrap(~attribute, scales = "free_y") +
  scale_fill_viridis_c() +
  labs(x = "Block", y = "Level", fill = "Count",
       title = "Attribute-Level Frequency by Block (Initial)") +
  theme_minimal()


# ----------------------------------
# 3 — Diagnostics of Original Ngene 
# ----------------------------------

# Global totals x ideal per block
global_totals_original <- balance_summary_original %>%
  group_by(attribute, level) %>%
  summarise(global_total = sum(count), .groups = "drop") %>%
  mutate(ideal_per_block = global_total / n_distinct(balance_summary_original$block))

# Merge and compute deviations
diagnostics_original <- balance_summary_original %>%
  left_join(global_totals_original, by = c("attribute", "level")) %>%
  mutate(deviation = count - ideal_per_block,
         abs_dev = abs(deviation),
         sq_dev = deviation^2)

# Imbalance score (sum of squared deviations)
imbalance_score <- sum(diagnostics_original$sq_dev)
cat("Imbalance score (sum of squared deviations):", imbalance_score, "\n")

#Imbalance score is 19, ideal complete balance is 0 

# Per-attribute summary
attr_summary_originals <- diagnostics_original %>%
  group_by(attribute) %>%
  summarise(total_abs_dev = sum(abs_dev),
            mean_abs_dev = mean(abs_dev),
            var_across_blocks = var(count),
            max_abs_dev = max(abs_dev),
            .groups = "drop")


#Diagnostic checks on the original Ngene export showed meaningful imbalance in how attribute levels were distributed across the four respondent blocks (sum of squared deviations = 19.93). 
#In particular, the habitat attribute was unevenly allocated (total absolute deviation = 8, mean absolute deviation = 0.67, maximum absolute deviation = 1.75), 
#and crowding also showed elevated imbalance (total_abs_dev = 6.5, mean_abs_dev = 0.54, max_abs_dev = 1.25). 
#These deviations indicate that some attribute levels were substantially over- or under-represented in specific blocks. 


#Statistical consequences of imbalance

#Higher SEs (less precise estimates) for levels that appear infrequently in the blocks actually seen by respondents.
#Potential bias in subgroup comparisons. If residents are assigned mostly block 1 and block 1 has more of Habitat level 3, 
#a higher estimated preference for Habitat in residents might be a design artefact.
#Poor power to detect differences in WTP across groups for attributes with unbalanced representation.

# ------------------------------
# 4 — Correlation diagnostics & visualization
# ------------------------------

# Build a numeric design for correlations
design_num_original <- design_long %>%
  mutate(across(c(habitat, trail, crowd), as.numeric))

# Correlation of attributes (Pearson)
cor_vars_original<- design_num_original %>% select(habitat, trail, crowd, cost)


# Correlation including block
print(cor(cbind(block = as.numeric(design_num_original$block), cor_vars_original)))

# A deeper correlation matrix (effects-coded dummies) for block<->attribute confounding
corr_data_original <- design_long %>% select(block, habitat, trail, crowd, cost)
corr_matrix_original <- model.matrix(~ ., data = corr_data_original)[, -1]
corr_mat_original <- cor(corr_matrix_original)

# Visualize correlation matrix
corrplot(corr_mat_original, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


#Based on the correlation matrix, the Light-to-moderate off-diagonal shading indicates non-zero associations 
#between block and some attributes (notably habitat and cost), 
#motivating targeted rebalancing to avoid block-driven confounding in subgroup analyses.




#For this reason, we apply manual tweaks (small targeted swaps) to lower the imbalance score 
#as well as the overall balance across all attributes.

#Small targeted swaps preserve the D-efficient structure of the Ngene design while removing design-induced confounding;
#this approach improves precision and validity for subgroup comparisons without materially changing the set of plausible 
#choice tasks respondents see.

# ------------------------------
# 5 — Rebalancing edits (original edits preserved)
# ------------------------------
# Goal: Have each level appear ~2–3 times per block for 3-level attributes
#By lowering the imbalance score and the correlation between habitat and cost 

## ----- Crowding -----
# Reduce overrepresentation of Crowd level 1 in Block 3:
# Change Alt1 crowd level at row 11 from 1 -> 2
crowd_block3 <- design %>%
  filter(block == 3 & (A1_crowd == 1 | A2_crowd == 1))
design$A1_crowd[11] <- 2

# Recompute long & summary
design_long <- reshape_to_long(design)
balance_summary <- design_long %>%
  pivot_longer(cols = c(habitat, trail, crowd, cost),
               names_to = "attribute", values_to = "level") %>%
  group_by(block, attribute, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(attribute, level, block)

# Plot updated counts
ggplot(balance_summary, aes(x = as.factor(block), y = as.factor(level), fill = count)) +
  geom_tile(color = "white") +
  facet_wrap(~attribute, scales = "free_y") +
  scale_fill_viridis_c() +
  labs(x = "Block", y = "Level", fill = "Count",
       title = "Attribute-Level Frequency by Block (After Crowd Edit)") +
  theme_minimal()

## ----- Trail condition -----
# Trail is shown as a balanced design - NO need to alter the levels
# Based on the original diagnostics, trail condition has lowest total absolute deviation. 

## ----- Habitat quality -----
# Rebalance habitat counts across blocks:
# - Increase habitat level 1 in Block 1 by editing Alt2 habitat at row 14
design$A2_habitat[4] <- 1
# - Increase habitat level 2 in Block 2 by editing Alt2 habitat at row 6
design$A2_habitat[6] <- 2
# - Increase habitat level 2 in Block 2 by editing Alt1 habitat at row 6
design$A1_habitat[6] <- 2
# - Increase habitat level 1 in Block 4 by editing Alt2 habitat at row 14
design$A2_habitat[14] <- 1


# Recompute long & summary
design_long <- reshape_to_long(design)
balance_summary <- design_long %>%
  pivot_longer(cols = c(habitat, trail, crowd, cost),
               names_to = "attribute", values_to = "level") %>%
  group_by(block, attribute, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(attribute, level, block)

# Plot updated counts
ggplot(balance_summary, aes(x = as.factor(block), y = as.factor(level), fill = count)) +
  geom_tile(color = "black") +
  facet_wrap(~attribute, scales = "free_y") +
  scale_fill_viridis_c() +
  labs(x = "Block", y = "Level", fill = "Count",
       title = "Attribute-Level Frequency by Block (After Habitat Edits)") +
  theme_minimal()

## ----- Cost -----
# Adjust cost frequency counts (balancing)
# - Block 1: increase $2 count (Alt1 cost row 4 -> 2)
design$A1_cost[4] <- 2
# - Block 3: increase $50 count (Alt1 cost row 9 -> 50)
design$A1_cost[9] <- 50
# - Block 4 : Reduce one count of $10 in Block 4 and increase $20
design$A2_cost[13] <- 20

## -----Change of levels that only consists of status quo (1,1,1) across attributes 

#Choice situation 10, block 3, has level of 1,1,1 (status quo) across attributes. This does not provide useful analysis 
#when comparing between alternatives. Ideally there needs to be an improvement of level from any of the attributes 
#To ensure balance of distribution, increase one level of trail condition from 1 to 2

design$A2_trail[10] <- 2

#Choice situation 14,block 4, has level of 1,1,1 (status quo) across attributes. This does not provide useful analysis 
#when comparing between alternatives. Ideally there needs to be an improvement of level from any of the attributes 
#To ensure balance of distribution, increase one level of habitat quality from 1 to 2

design$A1_trail[14] <-2

# Recompute
design_long <- reshape_to_long(design)
balance_summary <- design_long %>%
  pivot_longer(cols = c(habitat, trail, crowd, cost),
               names_to = "attribute", values_to = "level") %>%
  group_by(block, attribute, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(attribute, level, block)

# Plot
ggplot(balance_summary, aes(x = as.factor(block), y = as.factor(level), fill = count)) +
  geom_tile(color = "black") +
  facet_wrap(~attribute, scales = "free_y") +
  scale_fill_viridis_c() +
  labs(x = "Block", y = "Level", fill = "Count",
       title = "Attribute-Level Frequency by Block (After Cost Edits)") +
  theme_minimal()



# ----------------------------------------------------------------------
# 6 — Diagnostics of Edited Original Ngene for Attribute Balance (final)
# ----------------------------------------------------------------------

# Compute level frequencies per block (final)
balance_summary_edited <- design_long %>%
  pivot_longer(cols = c(habitat, trail, crowd, cost),
               names_to = "attribute", values_to = "level") %>%
  group_by(block, attribute, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(attribute, level, block)

# Global totals & ideal per block
global_totals_edited <- balance_summary_edited %>%
  group_by(attribute, level) %>%
  summarise(global_total = sum(count), .groups = "drop") %>%
  mutate(ideal_per_block = global_total / n_distinct(balance_summary$block))

# Merge and compute deviations
diagnostics_edited <- balance_summary_edited %>%
  left_join(global_totals_edited, by = c("attribute", "level")) %>%
  mutate(deviation = count - ideal_per_block,
         abs_dev = abs(deviation),
         sq_dev = deviation^2)

# Imbalance score (sum of squared deviations)
imbalance_score_edited <- sum(diagnostics_edited$sq_dev)
cat("Imbalance score (sum of squared deviations):", imbalance_score_edited, "\n")

# Per-attribute summary
attr_summary_edited <- diagnostics_edited %>%
  group_by(attribute) %>%
  summarise(total_abs_dev = sum(abs_dev),
            mean_abs_dev = mean(abs_dev),
            var_across_blocks = var(count),
            max_abs_dev = max(abs_dev),
            .groups = "drop")


# ------------------------------
# 7 — Correlation diagnostics & visualization
# ------------------------------

# Build a numeric design for correlations
design_num_edited <- design_long %>%
  mutate(across(c(habitat, trail, crowd), as.numeric))

# Correlation of attributes (Pearson)
cor_vars_edited <- design_num_edited %>% select(habitat, trail, crowd, cost)


# Correlation including block
print("Correlation including block:")
print(cor(cbind(block = as.numeric(design_num_edited$block), cor_vars_edited)))

# A deeper correlation matrix (effects-coded dummies) for block<->attribute confounding
corr_data_edited <- design_long %>% select(block, habitat, trail, crowd, cost)
corr_matrix_edited <- model.matrix(~ ., data = corr_data_edited)[, -1]
corr_mat_edited <- cor(corr_matrix_edited)

# Visualize correlation matrix
corrplot(corr_mat_edited, method = "color", type = "upper", tl.col = "black", tl.srt = 45)



#After scripted balancing edits the final design shows imbalance = 10 from the original imbalance of 19.93, 
#lower per-attribute deviations (habitat mean_abs_dev = 0.25), and reduced habitat–cost correlation (r ≈ 0.18), 
#comparing to r= 0.22 from original dataset. 


# ------------------------------
# 8 — Visual checks: cost by attribute boxplots (final)
# ------------------------------
ggplot(design_long, aes(x = factor(habitat), y = cost)) +
  geom_jitter(height = 0.2, width = 0.15, alpha = 0.6) +
  geom_boxplot(alpha = 0.2) +
  labs(x = "Habitat level", y = "Cost", title = "Cost distribution by Habitat (final design)")

ggplot(design_long, aes(x = factor(trail), y = cost)) +
  geom_jitter(height = 0.2, width = 0.15, alpha = 0.6) +
  geom_boxplot(alpha = 0.2) +
  labs(x = "Trail level", y = "Cost", title = "Cost distribution by Trail (final design)")

ggplot(design_long, aes(x = factor(crowd), y = cost)) +
  geom_jitter(height = 0.2, width = 0.15, alpha = 0.6) +
  geom_boxplot(alpha = 0.2) +
  labs(x = "Crowd level", y = "Cost", title = "Cost distribution by Crowding (final design)")


# ------------------------------
# 9 — Reshape to long format for merging purposes
# ------------------------------
# Reshape to long format
design_long.merge <- design %>%
  pivot_longer(
    cols = starts_with("A"),
    names_to = c("alt", "attribute"),
    names_pattern = "A(\\d+)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(`Choice situation`, block, alt),
    names_from = attribute,
    values_from = value
  ) %>%
  rename(
    Choice.Task = `Choice situation`,
    Alternative = alt,
    Trail_Condition = trail,
    Habitat_Quality = habitat,
    Crowding = crowd,
    Cost = cost
  ) %>%
  mutate(
    Alternative = as.integer(Alternative)
  )

# Add None alternative (alt = 3)
design_none <- design_long.merge%>%
  distinct(Choice.Task, block) %>%
  mutate(
    Alternative = 3,
    Trail_Condition = 0,
    Habitat_Quality  = 0,
    Crowding = 0,
    Cost = 0
  )

# Combine and sort
design_final<- bind_rows(design_long.merge, design_none) %>%
  arrange(Choice.Task, Alternative)

#Change 50 $ to 40$ for Cost - based on reveal preferences analysis on highest WTP of consumer surplus 
design_final %<>%
  mutate(Cost = ifelse(Cost == 50, 40, Cost))

design %<>%
  mutate(A1_cost = ifelse(A1_cost== 50, 40, A1_cost),
         A2_cost= ifelse(A2_cost== 50, 40, A2_cost))


# ------------------------------
# 10 — Export final layout (final)
# ------------------------------
write.csv(design, "ngene.output_edited.csv",row.names = FALSE)
write.csv(design_final, "Ngene_reshape_edited.csv", row.names = FALSE)


# ------------------------------
# End of script
# ------------------------------