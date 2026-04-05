
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
library(kableExtra)
#library(patchwork)  #combine multiple plots 

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

source("3. Analysis/Analysis.R")


# -------------------------------------------------
# 2 — Generate Socio-economic and demographic table 
# ------------------------------------------------

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
    ),
    
    Male = if_else(Demo_Sex == "Male", 1, 0),
    
    Used_Trail = if_else(HI_Trail.Use == "Yes", 1, 0),
    
    Income = case_when(
      Demo_HH.Income == "Less than $25,000" ~ 1,
      Demo_HH.Income == "$25,000 -$49,999" ~ 2,
      Demo_HH.Income == "$50,000 -$74,999" ~ 3,
      Demo_HH.Income == "$75,000 -$99,999" ~ 4,
      Demo_HH.Income == "$100,000 -$149,999" ~ 5,
      Demo_HH.Income == "$150,000 or more" ~ 6,
      TRUE ~ NA_real_
    ),
    
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

make_summary <- function(data, var, label, description, use_median = FALSE) {
  data %>%
    group_by(Zipverified) %>%
    summarise(
      mean_val = if (use_median) round(median({{var}}, na.rm = TRUE), 2) else round(mean({{var}}, na.rm = TRUE), 2),
      sd_val   = round(sd({{var}}, na.rm = TRUE), 2),
      .groups  = "drop"
    ) %>%
    mutate(
      value = paste0(round(mean_val), " (", round(sd_val, 2), ")")
    ) %>%
    select(Zipverified, value) %>%
    pivot_wider(
      names_from  = Zipverified,
      values_from = value
    ) %>%
    mutate(
      Variables   = label,
      Description = description
    ) %>%
    select(Variables, Description, everything())
}

# ── percentage summary function for binary variables ─────────────────────────

make_pct_summary <- function(data, var, label, description) {
  data %>%
    group_by(Zipverified) %>%
    summarise(
      pct = mean({{var}}, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      value = paste0(round(pct, 0), "%")
    ) %>%
    select(Zipverified, value) %>%
    pivot_wider(
      names_from  = Zipverified,
      values_from = value
    ) %>%
    mutate(
      Variables   = label,
      Description = description
    ) %>%
    select(Variables, Description, everything())
}


# ── individual summaries ─────────────────────────────────────────────────────

age_summary <- make_summary(
  resp_data,
  Age_mid,
  "Age",
  "Respondent age (years, midpoint of categories)"
)

sex_summary <- make_summary(
  resp_data,
  Male,
  "Sex",
  "Sex of respondents (1 = male, 0 = Female)"
)

trail_summary <- make_summary(
  resp_data,
  Used_Trail,
  "Used Trails in Hawai'i",
  "Respondents trail use ( 1 = Yes, 0= No)"
)

income_summary <- make_summary(
  resp_data,
  Income,
  "Income",
  "Total household income (1 = less than 25000, 6 = 150000 or more)"
)

education_summary <- make_summary(
  resp_data,
  Education,
  "Education",
  "Highest education attained (ordinal scale)",
  use_median = TRUE   # since you were using median
)

# ── bind rows ────────────────────────────────────────────────────────────────

socio_demo_table <- bind_rows(
  age_summary,
  sex_summary,
  trail_summary,
  income_summary,
  education_summary
) %>%
  as.data.frame()


# ── footnote text ─────────────────────────────────────────────────────────────

footnote <- paste(
  "Notes:Education reported as Median (SD)."
  ,"<br>",
  "¹ Income levels: 1 = Less than $25,000; 2 = $25,000–$49,999;",
  "3 = $50,000–$74,999; 4 = $75,000–$99,999;",
  "5 = $100,000–$149,999; 6 = $150,000 or more.",
  "<br>",
  "² Education levels: 1 = Some high school or less; 2 = High school diploma or GED;",
  "3 = Some college, no degree; 4 = Associate's or technical degree;",
  "<br>",
  "5 = Bachelor's degree; 6 = Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, etc.).",
  "<br>"
)


# ── stargazer output ──────────────────────────────────────────────────────────

stargazer(
  socio_demo_table,
  type        = "html",
  summary     = FALSE,
  rownames    = FALSE,
  title       = "Socio-demographic Characteristics by Residency Status",
  notes       = footnote,
  notes.align = "l",
  notes.label = "",
  out         = "socio_demo_table.html"
)

# Read the HTML output
html_content <- readLines("socio_demo_table.html")

# Find the header row and replace it with a two-line header
old_header <- grep("<tr>.*Resident.*Tourist.*</tr>", html_content)

# Replace the single header row with a grouped two-row header
html_content <- gsub(
  pattern     = "<td>Resident</td>.*<td>Tourist</td>",
  replacement = "<td colspan='1'>Resident<br>Mean (SD)</td><td colspan='1'>Tourist<br>Mean (SD)</td>",
  x           = html_content
)

# Inject CSS
css_fix <- '<style>
  table { border-collapse: collapse; width: auto; }
  td, th { padding: 4px 10px; text-align: center; white-space: nowrap; }
  td:first-child, th:first-child { text-align: left; }
</style>'

html_content <- c(css_fix, html_content)
writeLines(html_content, "socio_demo_table.html")



# socio_demo_table  %>%
#   gt() 
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




