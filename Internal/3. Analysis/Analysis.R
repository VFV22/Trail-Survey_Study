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

source("2. Processing /Processing.R")


# ----------------------------------
# 2 — Generate variables for MNL
# ----------------------------------

# Create Choice Binary as dependent variable 

merge.data.1 %<>%
  mutate(
    Choice.Binary = as.numeric(as.character(Choice.Binary))
  )

mlogit_clean <- merge.data.1

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

#Create mlogit for residents only 

mlogit_clean_Residents<-mlogit_clean %>%
  filter(Zipverified=="Resident")


#Create mlogit for tourist only 

mlogit_clean_Tourists<-mlogit_clean %>%
  filter(Zipverified=="Tourist")
  
  
  
  
  
  
