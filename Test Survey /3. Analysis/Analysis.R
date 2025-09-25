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
#library(patchwork)  #combine multiple plots 

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

source("Test Survey /2. Processing /Processing.R")


#Run full regression
lm_cost <- lm(Cost ~ Habitat_Quality + Trail_Condition + Crowding, data = merge.data)
summary(lm_cost)

print(model.4)
#Graph regression


# 
# # Create a tidy table of regression results
tidy_results <- tidy(model.4)
# 
# # Display it as a table
print(tidy_results)
# 
# 
tidy_results %>%
  kbl(digits = 3, caption = "Regression Results") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
#  



