
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
library(apollo)
library(knitr)
#library(patchwork)  #combine multiple plots 

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

source("3. Analysis /Analysis.R")


# -------------------------------------------------
# 2 — Generate Socio-economic and demographic table 
# ------------------------------------------------
resp_data <- mlogit_clean %>%
  distinct(RID, .keep_all = TRUE)



## Age 

resp_data <- mlogit_clean %>%
  distinct(RID, .keep_all = TRUE) %>%
  mutate(
    Age_mid = case_when(
      Demo_Age == "18-24 years old" ~ 21,
      Demo_Age == "25-34 years old" ~ 29.5,
      Demo_Age == "35-44 years old" ~ 39.5,
      Demo_Age == "45-54 years old" ~ 49.5,
      Demo_Age == "55-64 years old" ~ 59.5,
      Demo_Age == "65+ years old"   ~ 70,
      TRUE ~ NA_real_
    )
  )

age_summary <- resp_data %>%
  summarise(
    mean = mean(Age_mid, na.rm = TRUE),
    sd   = sd(Age_mid, na.rm = TRUE)
  ) %>%
  mutate(
    Demo = "Age",
    Description = "Respondent age (years, midpoint of categories)",
    `Mean (SD)` = paste0(
      round(mean, 2),
      " (",
      round(sd, 2),
      ")"
    )
  ) %>%
  select(Demo, Description,  `Mean (SD)`)

# Sex
sex_summary <- resp_data %>%
  mutate(Male = if_else(Demo_Sex == "Male", 1, 0)) %>%
  summarise(
    mean = mean(Male, na.rm = TRUE),
    sd   = sd(Male, na.rm = TRUE)
  ) %>%
  mutate(
    Demo = "Sex",
    Description = "Sex of respondents (1 = male, 0 = female)",
    `Mean (SD)` = paste0(
      round(mean, 2),
      " (",
      round(sd, 2),
      ")"
    )
  ) %>%
  select(Demo, Description,  `Mean (SD)`)


# Residents v Tourist

residence_summary <- resp_data %>%
  mutate(Residents = if_else(Zipverified == "Resident", 1, 0)) %>%
  summarise(
    mean = mean(Residents, na.rm = TRUE),
    sd   = sd(Residents, na.rm = TRUE)
  ) %>%
  mutate(
    Demo = "Residence Status",
    Description = "Residence of respondents (1 = Residents, 0 = Tourist)",
    `Mean (SD)` = paste0(
      round(mean, 2),
      " (",
      round(sd, 2),
      ")"
    )
  ) %>%
  select(Demo, Description,  `Mean (SD)`)

# Used trail or not 
Use.Trail_summary <- resp_data %>%
  mutate(`Used Trail in Hawai'i` = if_else(HI_Trail.Use == "Yes", 1, 0)) %>%
  summarise(
    mean = mean(`Used Trail in Hawai'i`, na.rm = TRUE),
    sd   = sd(`Used Trail in Hawai'i`, na.rm = TRUE)
  ) %>%
  mutate(
    Demo = "Used Trails in Hawai'i",
    Description = "Respondents trail use(1 = Yes, 0 = No)",
    `Mean (SD)` = paste0(
      round(mean, 2),
      " (",
      round(sd, 2),
      ")"
    )
  ) %>%
  select(Demo, Description,  `Mean (SD)`)


## Income 

resp_data <- mlogit_clean %>%
  distinct(RID, .keep_all = TRUE) %>%
  mutate(
    Income = case_when(
      Demo_HH.Income == "Less than $25,000" ~ 1,
      Demo_HH.Income == "$25,000 -$49,999" ~ 2,
      Demo_HH.Income == "$50,000 -$74,999" ~ 3,
      Demo_HH.Income == "$75,000 -$99,999" ~ 4,
      Demo_HH.Income == "$100,000 -$149,999" ~ 5,
      Demo_HH.Income == "$150,000 or more" ~ 6,
      TRUE ~ NA_real_
    )
  )

income_summary <- resp_data %>%
  summarise(
    mean = mean(Income, na.rm = TRUE),
    sd   = sd(Income, na.rm = TRUE)
  ) %>%
  mutate(
    Demo = "Income",
    Description = "Total household income per month (1 = less than $25,000, 6 = $150,000 or more)",
    `Mean (SD)` = paste0(
      round(mean, 2),
      " (",
      round(sd, 2),
      ")"
    )
  ) %>%
  select(Demo, Description,  `Mean (SD)`)


## Income 

resp_data <- mlogit_clean %>%
  distinct(RID, .keep_all = TRUE) %>%
  mutate(
    Education = case_when(
      Demo_Education == "Some high school or less" ~ 1,
      Demo_Education == "High School diploma or GED" ~ 2,
      Demo_Education == "Some college, but no degree" ~ 3,
      Demo_Education == "Associates or technical degree" ~ 4,
      Demo_Education == "Bachelor's degree" ~ 5,
      Demo_Education == "Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)" ~ 6,
      TRUE ~ NA_real_
    )
  )

education_summary <- resp_data %>%
  summarise(
    mean = mean(Education, na.rm = TRUE),
    sd   = sd(Education, na.rm = TRUE)
  ) %>%
  mutate(
    Demo = "Education",
    Description = "Education of respondents(1 = Some high school or less, 6 = Graduate or professional degree (MA, MS, MBA, PhD, etc.)",
    `Mean (SD)` = paste0(
      round(mean, 2),
      " (",
      round(sd, 2),
      ")"
    )
  ) %>%
  select(Demo, Description,  `Mean (SD)`)




#Combine 
socio_demo_table <- bind_rows(
  residence_summary,
  Use.Trail_summary,
  age_summary,
  sex_summary,
  income_summary,
  education_summary
)

# -------------------------------------------------
# 3 — Generate Table to support User fee, cost allocation etc
# ------------------------------------------------

group_N <- resp_data %>%
  count(Zipverified) %>%
  tidyr::pivot_wider(
    names_from = Zipverified,
    values_from = n
  )


#Willing to pay user fee
resp_data %<>%
  mutate(
    WTP_fee = if_else(User.fee_Y.N == "Yes", 1, 0)
  )

make_policy_row <- function(data, var, label) {
  data %>%
    group_by(Zipverified) %>%
    summarise(
      percent = mean({{ var }}, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = Zipverified,
      values_from = percent
    ) %>%
    mutate(
      Question = label,
      Resident = paste0(round(Resident, 1), "%"),
      Tourist  = paste0(round(Tourist, 1), "%")
    ) %>%
    select(Question, Resident, Tourist)
}


row_fee <- make_policy_row(
  resp_data,
  WTP_fee,
  "Willing to pay a user fee to support trail management"
)

#Cost allocation

resp_data %<>%
  mutate(
    Alloc_res_only  = if_else(Cost.Allocation == "Only Residents", 1, 0),
    Alloc_shared    = if_else(Cost.Allocation == "Both, equally", 1, 0),
    Alloc_res_major = if_else(Cost.Allocation == "Both, but majority residents", 1, 0),
    Alloc_vis_major = if_else(Cost.Allocation == "Both, but majority visitors", 1, 0),
    Alloc_vis_only  = if_else(Cost.Allocation == "Only Visitors", 1, 0)
  )

row_res_only <- make_policy_row(
  resp_data,
  Alloc_res_only,
  "Only Residents "
)

row_shared <- make_policy_row(
  resp_data,
  Alloc_shared,
  "Equal Split"
)

row_res_major <- make_policy_row(
  resp_data,
  Alloc_res_major,
  "Majority Residents"
)

row_vis_major <- make_policy_row(
  resp_data,
  Alloc_vis_major,
  "Majority Tourists"
)

row_vis_only <- make_policy_row(
  resp_data,
  Alloc_vis_only,
  "Only Tourists"
)

#Payment method

resp_data %<>%
  mutate(
    Pay.Type_Per.entry = if_else(User.fee_Payment.Type == "Per-entry fee (pay each time you use a trail)", 1, 0),
    Pay.Type_Daily.Pass = if_else(User.fee_Payment.Type == "Daily pass (one payment allows unlimited trail entries in a single day)", 1, 0),
    Pay.Type_Annual.Pass = if_else(User.fee_Payment.Type == "Annual pass (one payment allows unlimited trail entries for a year)", 1, 0),
    Pay.Type_Voluntary = if_else(User.fee_Payment.Type == "Voluntary donation (optional payment, not required for trail entry)", 1, 0),
  )

Pay.Type_Per.entry<- make_policy_row(
  resp_data,
  Pay.Type_Per.entry,
  "Per Entry fee"
)

Pay.Type_Daily.Pass<- make_policy_row(
  resp_data,
  Pay.Type_Daily.Pass,
  "Daily Pass"
)

Pay.Type_Annual.Pass <- make_policy_row(
  resp_data,
  Pay.Type_Annual.Pass ,
  "Annual Pass"
)

Pay.Type_Voluntary <- make_policy_row(
  resp_data,
  Pay.Type_Voluntary  ,
  "Voluntary donation"
)



#Combine table
User_fee_support <- dplyr::bind_rows(
  row_fee,
  row_res_only,
  row_shared,
  row_res_major,
  row_vis_major,
  row_vis_only, 
  Pay.Type_Per.entry, 
  Pay.Type_Daily.Pass, 
  Pay.Type_Annual.Pass, 
  Pay.Type_Voluntary
)

#Add N to combine table
User_fee_support %<>%
  rename_with(
    ~ glue("Resident (N = {group_N$Resident})"),
    Resident
  ) %>%
  rename_with(
    ~ glue("Tourist (N = {group_N$Tourist})"),
    Tourist
  )

#Section header
section_cost_alloc <- tibble::tibble(
  Question = "Cost allocation preferences for user fees",
  `Resident (N = 339)` = "",
  `Tourist (N = 1054)` = ""
)

section_payment <- tibble::tibble(
  Question = "Preferred payment type for user fees",
  `Resident (N = 339)` = "",
  `Tourist (N = 1054)` = ""
)

table_top <- User_fee_support %>% slice(1)
table_middle <- User_fee_support %>% slice(2:6)
table_bottom <- User_fee_support %>% slice(7:10)


User_fee_support_final <- dplyr::bind_rows(
  table_top,
  section_cost_alloc,
  table_middle,
  section_payment,
  table_bottom
)


User_fee_support_final %>%
  gt() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Question %in% c(
        "Cost allocation preferences for user fees",
        "Preferred payment type for user fees"
      )
    )
  )




