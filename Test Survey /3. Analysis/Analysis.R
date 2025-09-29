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
#library(patchwork)  #combine multiple plots 

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

source("Test Survey /2. Processing /Processing.R")


merge.data %<>%
  mutate(
    Alternative   = factor(Alternative),              # factor for alternatives
    Choice.Task   = as.numeric(as.character(Choice.Task)), # numeric for choice set

  )

merge.data %<>%
  mutate(
    Choice.Binary = as.numeric(as.character(Choice.Binary))
  )


mlogit_clean <- merge.data %>% 
  filter(!is.na(Chosen.Alternative))

mlogit_clean%<>%
  mutate(
    # assuming you have a column Block (1–4), otherwise calculate it
    ChoiceSetID = interaction(block, Choice.Task, RID, drop = TRUE)
  )


#Run regression

# convert data into mlogit.data format
mnl.data <- mlogit.data(
 mlogit_clean,
  choice   = "Choice.Binary",
  shape    = "long",
  chid.var = "ChoiceSetID",   # now globally unique
  alt.var  = "Alternative",
  id.var   = "RID"
)


# Try with fewer random parameters first
mnl.model <- mlogit(Choice.Binary ~ Cost + Habitat_Quality + Trail_Condition + Crowding | 0,
                    data = mnl.data)
summary(mnl.model)

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



