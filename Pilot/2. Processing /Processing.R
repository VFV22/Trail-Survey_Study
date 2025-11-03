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

source("Documents/GitHub/Trail-Survey_Study/Pilot/1. Cleaning /Cleaning.R")

#------------------------------------------------------:
# (2): Pivot for DCE #####
#------------------------------------------------------:

# Pivot longer 
Reshape.long.1 <- Pilot %>% 
  pivot_longer(cols = starts_with("Choice.Task_"),
               names_to = "Choice.Task", 
               values_to = "Chosen.Alternative")%>%
  slice(rep(1:n(), each = 3)) %>%  # Repeat rows for scenarios
  mutate(Alternative = rep(1:3, length.out = n())) %>%  # Assign 1, 2, 3 for each scenario
  ungroup()



# Load experimental design from edited ngene 
design_ngene <- read_csv("~/Documents/GitHub/Trail-Survey_Study/Ngene_reshape_edited 11.18.56 PM.csv") 


# Match data format for before merging

# Choice Task to numeric  
Reshape.long.1 %<>%
  mutate(Choice.Task = gsub("Choice.Task_", "", Choice.Task),   # remove the text
         Choice.Task = as.numeric(Choice.Task))                 # convert to numeric    

# Chosen.Scenario to numeric 

Reshape.long.1%<>%
  mutate(
    Chosen.Alternative = case_when(
      Chosen.Alternative == "Alternative one"   ~ 1,
      Chosen.Alternative == "Alternative two"   ~ 2,
      Chosen.Alternative == "None of the options" ~ 3
    )
  )     


#homme testing lab survey 
# Merge 

merge.data.1 <- merge(Reshape.long.1, design_ngene, 
                    by=c("Choice.Task" = "Choice.Task", "Alternative"= "Alternative"),all.y = TRUE) # all.y=TRUE - keep all rows from the first (left) dataset.


# Create Independent variable Choice Binary 
merge.data.1 %<>%
  mutate(Choice.Binary = coalesce(as.integer(Chosen.Alternative == Alternative), 0))



#------------------------------------------------------:
# (3): Reorder Columns #####
#------------------------------------------------------:
merge.data.1 %<>%
  select(
    RID,
   Zipverified, 
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
    Member_Env.Group,
    starts_with("Demo_"),
    starts_with("Hawaii_"),
    starts_with("Timer."), 
    everything(), 
  )

merge.data.1 %<>% 
  arrange(RID)

#------------------------------------------------------:
# (4): Reclass Columns #####
#------------------------------------------------------:       

merge.data.1 %<>%
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

merge.data.1 %<>%
  mutate(
    across(
      c(
        Zipverified, Choice.Task, Chosen.Alternative, block, Mgm.option_No.Text,
        User.fee_Y.N, User.fee_Natural.Features, User.fee_High.Tourist, User.fee_High.Maintance.Cost, 
        User.fee_Ecological.Sensitive, User.fee_Additional.Amenities, User.fee_Payment.Type, User.fee_Payment.Method, 
        Cost.Allocation, Volunteer.hours, Pay_Access.Fee, Member_Env.Group, 
        Demo_Age, Demo_Sex, Demo_Etnicity, Demo_Education, Demo_HH.Income, Demo_Employment, HI_Island.Visit,HI_Island.Live,
        HI_Residence, HI_Trail.Use, HI_Trail.Use_Reason, Choice.Binary
      ),
      ~ factor(.x)   # apply factor() to each column
    )
  )

merge.data.1 %<>%
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


# Filter NA to remove unselected alternatives
merge.data.1 %<>% 
  filter(!is.na(Chosen.Alternative))


#------------------------------------------------------:
# (5): Sanity Check #####
#------------------------------------------------------:       

# Summarize block distribution 
summary_block.distribution <- merge.data.1 %>%
  distinct(RID, .keep_all = TRUE) %>% 
  group_by(Zipverified, block) %>% 
  count() %>% 
  group_by(Zipverified) %>% 
  mutate(percentage = n / sum(n) * 100)  # percentage within each Zipverified group

# Plot as bar chart with percentages
ggplot(summary_block.distribution, aes(x = block, y = percentage, fill = Zipverified)) +
  geom_col(position = "dodge") +  # side-by-side bars
  labs(
    title = "Percentage per Block by Zipverified",
    x = "Block",
    y = "Percentage (%)",
    fill = "Zip Verified"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(summary_block.distribution)

# Count how many times each alternative was chosen overall
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Alternative)

# Check balance within each block (to make sure design isn’t skewed)
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(block, Alternative)

# Check balance per choice task 

merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Choice.Task, Alternative) %>% 
  mutate(pct = n / sum(n)*100)

# Visualize balance across alternatives 
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Alternative) %>%
  ggplot(aes(x = Alternative, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Balance check: Alternative choices",
    x = "Alternative",
    y = "Count of times chosen"
  )

# Visualize balance of attribute levels - Habitat Quality

#Check distribution of choices for TC
merge.data.1 %>%
  count(Alternative, Habitat_Quality) %>%
  group_by(Habitat_Quality) %>%
  summarise(total_shown = sum(n)) %>%
  arrange(desc(Habitat_Quality))

merge.data.1 %>%
  count(Habitat_Quality) %>%
  ggplot(aes(x = Habitat_Quality, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Shown counts per Habitat_Quality")

# Visualize balance of attribute levels - Trail Condition 

#Check distribution of choices for TC
merge.data.1 %>%
  count(Alternative, Trail_Condition) %>%
  group_by(Trail_Condition) %>%
  summarise(total_shown = sum(n)) %>%
  arrange(desc(Trail_Condition))

merge.data.1 %>%
  count(Trail_Condition) %>%
  ggplot(aes(x = Trail_Condition, y = n)) +
  geom_col(fill = "skyblue") +
  labs(title = "Shown counts per Trail.Condition")

# Visualize balance of attribute levels - Crowding 

#Check distribution of choices for TC
merge.data.1 %>%
  count(Alternative, Crowding) %>%
  group_by(Crowding) %>%
  summarise(total_shown = sum(n)) %>%
  arrange(desc(Crowding))


merge.data.1 %>%
  count(Crowding) %>%
  ggplot(aes(x = Crowding, y = n)) +
  geom_col(fill = "darkblue") +
  labs(title = "Shown counts per Crowding")


# Visualize balance of attribute levels - Cost

merge.data.1 %>%
  count(Alternative, Cost) %>%
  group_by(Cost) %>%
  summarise(total_shown = sum(n)) %>%
  arrange(desc(Cost))

merge.data.1 %>%
  count(Cost) %>%
  ggplot(aes(x = Cost, y = n)) +
  geom_col(fill = "darkorange") +
  labs(title = "Shown counts per Cost")









