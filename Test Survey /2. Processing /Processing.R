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

source("Test Survey /1. Cleaning /Cleaning.R")


#------------------------------------------------------:
# (2): Pivot for DCE #####
#------------------------------------------------------:

# Pivot longer 
Reshape.long <- pivot_longer(test_raw.data, starts_with("Choice.Task_"),
                             names_to = "Choice.Task", 
                             values_to = "Chosen.Alternative")

# Load experimental design from edited ngene 
design_ngene <- read_csv("Ngene_reshape_edited.csv") 


# Match data format for before merging

# Choice Task to numeric  
Reshape.long %<>%
  mutate(Choice.Task = gsub("Choice.Task_", "", Choice.Task),   # remove the text
         Choice.Task = as.numeric(Choice.Task))                 # convert to numeric    

# Chosen.Scenario to numeric 

Reshape.long <- Reshape.long %>%
  mutate(
    Chosen.Alternative = case_when(
      Chosen.Alternative == "Alternative one"   ~ 1,
      Chosen.Alternative == "Alternative two"   ~ 2,
      Chosen.Alternative == "None of the option" ~ 3
    )
  )     


# Merge 

merge.data <- merge(Reshape.long, design_ngene, by=c("Choice.Task" = "Choice.Task", "Chosen.Alternative" = "Chosen.Alternative"),
            all.y=TRUE) # all.y=TRUE - keep all rows from the first (left) dataset.

# Expand to include all 3 alternatives per respondent-task
merge.data %<>%
  mutate(Alternative = Chosen.Alternative) %>%   # create alt column
  group_by(RID, Choice.Task) %>%
  tidyr::complete(Alternative = 1:3) %>%
  ungroup()


# Create Independent variable Choice Binary 
merge.data %<>%
  mutate(Choice.Binary = coalesce(as.integer(Alternative == Chosen.Alternative), 0))


#------------------------------------------------------:
# (3): Reorder Columns #####
#------------------------------------------------------:
merge.data %<>%
  select(
    RID,
    Tourist, 
    Residents, 
    Choice.Task,
    Alternative,
    Chosen.Alternative, 
    Choice.Binary,
    block, 
    Habitat_Quality, 
    Trail_Condition, 
    Crowding, 
    Cost, 
    Certainty.CE_Scale, 
    Mgm.option_No.Text, 
    Mgm.option_With.Text, 
    starts_with("User.fee_"),
    Cost.Allocation,
    Cost.Allocation.Text, 
    Likelihood_Trail.Use, 
    Empirical.Expec_Trail.Use, 
    Normative.Expec_Trail.Use, 
    starts_with("Self.Eff_"), 
    starts_with("Collective.Eff_"), 
    Volunteer.hours, 
    Volunteer.WTP, 
    Pay_Access.Fee, 
    Participate_community.discussion, 
    Member_Env.Group,
    starts_with("Demo_"),
    starts_with("Hawaii_"),
    starts_with("Timer."), 
    everything(), 
  )

merge.data %<>% 
  arrange(RID)

#------------------------------------------------------:
# (4): Reclass Columns #####
#------------------------------------------------------:       

merge.data %<>%
  mutate(
    Trail_Condition = case_when(
      Trail_Condition == "0" ~ "None",
      Trail_Condition == "1" ~ "Low",
      Trail_Condition == "2" ~ "Medium",
      Trail_Condition == "3" ~ "High"
    ),
    Habitat_Quality = case_when(
      Habitat_Quality == "0" ~ "None",
      Habitat_Quality == "1" ~ "Low",
      Habitat_Quality == "2" ~ "Medium",
      Habitat_Quality == "3" ~ "High"
    ),
    Crowding = case_when(
      Crowding == "0" ~ "None",
      Crowding == "1" ~ "Low",
      Crowding == "2" ~ "Medium",
      Crowding == "3" ~ "High"
    ),
    # convert all into ordered factors
    across(
      c(Trail_Condition, Habitat_Quality, Crowding),
      ~ factor(.x, levels = c("None", "Low", "Medium", "High"), ordered = TRUE)
    )
  )

merge.data %<>%
  mutate(
    across(
      c(
        Tourist, Residents, Choice.Task, Chosen.Alternative, block, Mgm.option_No.Text,
        User.fee_Y.N, User.fee_Natural.Features, User.fee_High.Tourist, User.fee_High.Maintance.Cost, 
        User.fee_Ecological.Sensitive, User.fee_Additional.Amenities, User.fee_Payment.Type, User.fee_Payment.Method, 
        Cost.Allocation, Volunteer.hours, Pay_Access.Fee, Participate_community.discussion, Member_Env.Group, 
        Demo_Age, Demo_Sex, Demo_Etnicity, Demo_Education, Demo_HH.Income, Demo_Employment, Hawaii_Island,
        Hawaii_Residence, Hawaii_Trail.Use, Hawaii_Trail.Use_Reason, Choice.Binary
      ),
      ~ factor(.x)   # apply factor() to each column
    )
  )

merge.data %<>%
  mutate(
    across(
      c(
        Certainty.CE_Scale, Likelihood_Trail.Use, Empirical.Expec_Trail.Use, Normative.Expec_Trail.Use,
        Self.Eff_UF_HQ, Self.Eff_UF_TC, Self.Eff_UF_Crowd, 
        Collective.Eff_UF_HQ, Collective.Eff_UF_TC, Collective.Eff_UF_Crowd,
        Volunteer.WTP
      ),
      ~ as.numeric(.x)   # convert to numeric
    )
  )







