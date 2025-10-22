# ------------------------------
# 0 — Libraries  #####
# ------------------------------
library(tidyverse)   # dplyr, tidyr, ggplot2, readr etc.
library(magrittr)    # pipe (%<>%) used in original script
library(readxl)      # read_excel
library(corrplot)    # corrplot visualization
library(tibble)    # produce data frames
library(janitor)    # cleaning and tidying data
library(gt)         # produce table of results
library(mlogit)   # Function for mlogit 
library(car)      #Checking for collinearity
#library(patchwork)  #combine multiple plots 

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

source("Test Survey /2. Processing /Processing.R")



# ----------------------------------
# 2 — Generate variables for MNL
# ----------------------------------

# Create Choice Binary as dependent variable 

merge.data %<>%
  mutate(
    Choice.Binary = as.numeric(as.character(Choice.Binary))
  )

# Filter NA to remove unselected alternatives and create mlogit data
mlogit_clean <- merge.data %>% 
  filter(!is.na(Chosen.Alternative))

# Generate unique ID for each choice task x block and RID 

mlogit_clean %<>%
  mutate(
    ChoiceSetID = interaction(block, Choice.Task, RID, drop = TRUE)
  )

# Mutate variables to fit with MNL format 

mlogit_clean %<>%
  mutate(
    ChoiceSetID = as.character(ChoiceSetID),
    Alternative = as.character(Alternative),
    RID         = as.character(RID)
  )


# Convert attributes to factors with proper baseline (e.g., Low)
# mlogit_clean %<>%
#   mutate(
#     Habitat_Quality = factor(Habitat_Quality, levels = c("Low", "Medium", "High")),
#     Trail_Condition = factor(Trail_Condition, levels = c( "Low", "Medium", "High")),
#     Crowding = factor(Crowding, levels = c( "Low", "Medium", "High"))
#   )


# Generate new variable for main attributes variables in numeric format

mlogit_clean %<>%
  mutate(
    Habitat_Quality_Num = case_when(
      Habitat_Quality == "None" ~ 0,
      Habitat_Quality == "Low" ~ 1,
      Habitat_Quality == "Medium" ~ 2,
      Habitat_Quality == "High" ~ 3
    ),
    Crowding_Num = case_when(
      Crowding == "None" ~ 0,
      Crowding == "Low" ~ 1,
      Crowding == "Medium" ~ 2,
      Crowding == "High" ~ 3
    ),
    Trail_Condition_Num = case_when(
      Trail_Condition == "None" ~ 0,
      Trail_Condition == "Low" ~ 1,
      Trail_Condition == "Medium" ~ 2,
      Trail_Condition == "High" ~ 3
    )
    
  )


# ----------------------------------
# 2 — Convert data to MNL format
# ----------------------------------

# convert data into mlogit.data format
mnl.data <- mlogit.data(
 mlogit_clean,
  choice   = "Choice.Binary",
  shape    = "long",
  chid.var = "ChoiceSetID",   # now globally unique
  alt.var  = "Alternative",
  id.var   = "RID", 
)


# ----------------------------------
# 3 — Run regressions - MNL 
# ----------------------------------

# Run regression - MNL 
mnl.model <- mlogit(
  Choice.Binary ~ Cost + Habitat_Quality + Trail_Condition + Crowding,
  data = mnl.data,
  reflevel = "3"
)




# Not working with attribute levels as factors 
# This is because we treat "None" as another level, leading to perfect collinearity 
# Where the model couldn't distinguish between opt-out alternative and the "None" baseline.




# ----------------------------------
# 4 — Run regressions - Logit and conditional logit
# ----------------------------------

# Run regression - Logit
logit.model <- glm (Choice.Binary ~ Cost + Habitat_Quality + Trail_Condition + Crowding,
                    data=mnl.data,
                    family = binomial(link="logit"))

summary(logit.model)



# ----------------------------------
# 3 — Run regressions - MNL (numeric)  
# ----------------------------------

# Run regression as numeric for attributes 

mix_model <- mlogit(
  Choice.Binary ~ Habitat_Quality_Num + Crowding_Num + Trail_Condition_Num + Cost,
  data = mnl.data
)

summary(mix_model)

# It works because numeric scales collapsed the categorical design into a linear score
# No perfect collinearity → optimization succeeds.
# But this comes at a theoretical cost: we are assuming that the utility difference between "None" → "Low" is the same 
# as "Low" → "Medium" → "High" (a linear, cardinal scale).

# Insight 
# Cost is counterintuitive 
# Log-likelihood = -471.99, McFadden R² ≈ 0.005 → extremely poor fit.

# Red flag about design redundancy (the issue we saw with “None”), 
# or that treating the levels as numeric distorted the relationships.
# Could be the reason why cost is counterintuitive 

# ----------------------------------
# 4 — Generate Graphs
# ----------------------------------

# 
# # Create a tidy table of regression results
tidy_results <- tidy(mix_model)
# 
# # Display it as a table
print(tidy_results)
# 
# 
tidy_results %>%
  kbl(digits = 3, caption = "Regression Results") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

