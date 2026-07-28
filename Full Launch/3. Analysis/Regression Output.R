
# Apollo output manual extract 

#Extract apollo model result 
est <- model$estimate

#Extract standard error
se <- sqrt(diag(model$robvarcov))

#Create coefficient table 
coef_table <- tibble(
  Parameter = names(est),
  Estimate  = est,
  SE        = se[names(est)]
)

#Add significance stars
coef_table <- coef_table %>%
  mutate(
    z = Estimate / SE,
    Stars = case_when(
      abs(z) > 3.29 ~ "***",
      abs(z) > 2.58 ~ "**",
      abs(z) > 1.96 ~ "*",
      TRUE ~ ""
    )
  )

#Label table name 
coef_table %<>%
  mutate(
    Variable = dplyr::recode(
      Parameter,
      b_Habitat_QualityMed  = "Habitat quality (Medium)",
      b_Habitat_QualityHigh = "Habitat quality (High)",
      b_Trail_ConditionMed  = "Trail condition (Medium)",
      b_Trail_ConditionHigh = "Trail condition (High)",
      b_CrowdingMed         = "Crowding (Medium)",
      b_CrowdingLow         = "Crowding (Low)",
      b_cost                = "Cost",
      b_resident            = "Resident",
      b_age                 = "Age",
      b_income              = "Income",
      b_sex                 = "Sex"
    )
  ) %>%
  filter(!is.na(Variable)) %>%
  mutate(
    MNL = sprintf("%.3f (%.3f)%s", Estimate, SE, Stars)
  ) %>%
  select(Variable, MNL)

