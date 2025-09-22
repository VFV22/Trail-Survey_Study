# ------------------------------
# 0 — User settings / file paths
# ------------------------------
# Edit this path if your Ngene export is in a different location
test_raw.data <- read_csv("Test Survey /0. Data/Trail Pilot - Forth draft - August 2025_September 18, 2025_16.01.csv")

# ------------------------------
# 1 — Libraries  #####
# ------------------------------
library(tidyverse)   # dplyr, tidyr, ggplot2, readr etc.
library(magrittr)    # pipe (%<>%) used in original script
library(readxl)      # read_excel
library(corrplot)    # corrplot visualization


#------------------------------------------------------:
# (2): Select and filter complete data #####
#------------------------------------------------------:

test_raw.data %<>% 
  filter(QID1=="Yes")

test_raw.data %<>%
  rename(Consent = QID1,
         Age=Q38,
         Sex=Q6,
         Etnicity=Q81,
         Education=Q82,
         HH.Income=Q84,
         Employment=Q85,
         ZIP = Q77, 
         Hawaii_Island=Q10,
         Hawaii_Residence=Q13,
         Hawaii_Trail.Use=Q90)
