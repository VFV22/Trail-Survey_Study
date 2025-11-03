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

Pilot <- read_csv("Documents/GitHub/Trail-Survey_Study/Pilot/0. Data/Trail Survey - Final draft - October 2025_November 2, 2025_15.43.csv")


#------------------------------------------------------:
# (2): Select and filter complete data #####
#------------------------------------------------------:
# #Filter consented respondent 
# test_raw.data %<>% 
#   filter(QID1=="Yes")

# #Filter completed respondent 
# test_raw.data %<>% 
#   filter(Finished=="True")

#Remove non use columns 

Pilot  %<>%
  select(-RecipientLastName,-RecipientFirstName, -RecipientEmail, 
         -ExternalReference,-Progress, -ExternalReference, -DistributionChannel,
         -UserLanguage )


Pilot  %<>%
  rename(
        #RID
        Qualtrics.RID = ResponseId,
        
        #Consent 
        Consent = QID1,
        
         #Demographic 
        Demo_State = Q1,
        Demo_Visit.HI=Q101,
        Demo_Age=Q38,
         Demo_Sex=Q6,
         Demo_Etnicity=Q81,
         Demo_Education=Q82,
         Demo_HH.Income=Q84,
         Demo_Employment=Q85,
         HI_Island.Live=Q10,
        HI_Island.Visit=Q102,
         HI_Residence=Q13,
         HI_Trail.Use=Q90,
        HI_Trail.Use_Reason=Q11,
        HI_Trail.Use_Reason.Text=Q11_10_TEXT,
        HI_Trail.Use.Quant = Q103,
         
         #CE Intro timer 
         Timer.I_First.Click=`Q55_First Click`,
         Timer.I_Last.Click=`Q55_Last Click`,
         Timer.I_Submit.Click=`Q55_Page Submit`,
         Timer.I_Count.Click= `Q55_Click Count`,
         Timer.II_First.Click=`Q92_First Click`,
         Timer.II_Last.Click=`Q92_Last Click`,
         Timer.II_Submit.Click=`Q92_Page Submit`,
         Timer.II_Count.Click= `Q92_Click Count`, 
         
         #Choice task 
            #Block 1
         Choice.Task_1 = Q79, 
         Choice.Task_2 = Q60,
         Choice.Task_3 = Q61,
         Choice.Task_4 = Q62,
            #Block 2
         Choice.Task_5 = Q67, 
         Choice.Task_6 = Q66,
         Choice.Task_7 = Q65,
         Choice.Task_8 = Q63,
            #Block 3
         Choice.Task_9 = Q72,
         Choice.Task_10 = Q70, 
         Choice.Task_11 = Q69, 
         Choice.Task_12 = Q68,
            #Block 4 
         Choice.Task_13 = Q76,
         Choice.Task_14 = Q75,
         Choice.Task_15 = Q74,
         Choice.Task_16 = Q73,
         
        #Management option (text with pic v picture only)
        Mgm.option_With.Text = Q64,
        Mgm.option_No.Text = Q59,
        
        #Certainty follow up
        Certainty.CE_Scale = Q48_1, 
        
        #Payment Preferences 
        User.fee_Y.N = Q21, 
        User.fee_Natural.Features = Q22_1,
        User.fee_High.Tourist= Q22_2, 
        User.fee_High.Maintance.Cost= Q22_3, 
        User.fee_Ecological.Sensitive = Q22_4, 
        User.fee_Additional.Amenities = Q22_5,
        User.fee_Payment.Type = Q95,
        User.fee_Payment.Type.Text = Q95_7_TEXT,
        User.fee_Payment.Method = Q96, 
        User.fee_Payment.Method.Text = Q96_7_TEXT,
        
        #Attention check 
        Attention.Check =Q19, 
        
        #Cost allocation 
        Cost.Allocation = Q23,
        Cost.Allocation.Text = Q23_6_TEXT, 
        Likelihood_Trail.Use = Q24_1, 
        
        #Usage behavior 
        Empirical.Expec_Trail.Use = Q25_1, 
        Normative.Expec_Trail.Use = Q26_1, 
        Self.Eff_UF_HQ = Q27_2, 
        Self.Eff_UF_TC  = Q27_3,
        Self.Eff_UF_Crowd = Q27_4, 
        Collective.Eff_UF_HQ = Q56_2, 
        Collective.Eff_UF_TC = Q56_3, 
        Collective.Eff_UF_Crowd = Q56_4, 
        
        #Volunteering
        Volunteer.hours = Q34, 
        Volunteer.WTP = Q35,
        
        #Env profile 
        Pay_Access.Fee = Q33, 
        Member_Env.Group = Q86, 
        
        #End Survey 
        Comments_Survey = Q97, 
         RID = rid, 
  )

#Recode tourists and residents based on Zip verification 
Pilot$Zipverified <- ifelse(Pilot$Zipverified == "1", "Resident", "Tourist")

        
#Filter out testing RIDs
Pilot %<>% 
  filter(!is.na(RID) & grepl("^[0-9a-f\\-]+$", RID))

#Filter out incomplete / failed attention check 
Pilot %<>% 
  filter(gc==1)
