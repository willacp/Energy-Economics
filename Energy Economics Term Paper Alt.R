#################### FILE HISTORY ##################
##### Original file created by Willa Perlman 11/09/23
##### For use in PSU energy economics course term paper
##### ALTERNATIVE data analysis

################### LOAD PACKAGES ##################

library(tidyverse)
library(dplyr)
library(writexl)
library(readxl)
library(readr)
library(ggplot2)

#################### LOAD DATA ####################

RECS <- read_sas("/Users/willacperlman/recs2020_public_v5.sas7bdat")
OR_ACS <- read_csv("/Users/willacperlman/psam_h41.csv")

####### SELECT & RECODE RECS DATA #####

df <- RECS%>%
  filter(STATE_FIPS == '41')%>%
  select(TYPEHUQ, KOWNRENT, NOHEATDAYS, NOACDAYS, MONEYPY, HOUSEHOLDER_RACE, NHSLDMEM,
         NUMCHILD, NUMADULT2, ATHOME)%>%
  mutate(buildingType = case_when(
    TYPEHUQ == "1" ~ "Mobile home",
    TYPEHUQ == "2" ~ "Single-family detached",
    TYPEHUQ == "3" ~ "Single-family attached",
    TYPEHUQ == "4" ~ "Multi-family small",
    TYPEHUQ == "5" ~ "Multi-family large"),
    incomeGrp = case_when(
      MONEYPY >= 1 & MONEYPY <= 4 ~ "Under 15k",
      MONEYPY >= 4 & MONEYPY <= 6 ~ "Between 10 and 20k",
      MONEYPY >= 7 & MONEYPY <= 11 ~ "Between 15 and 50k",
      MONEYPY >= 12 & MONEYPY <= 14 ~ "Between 50 and 100k",
      MONEYPY >= 15 & MONEYPY <= 16 ~ "100k or more"
    ))

energyInsecure <- df%>%
  filter(NOHEATDAYS != -2)%>%
  group_by(buildingType)%>%
  summarise(mean(NOHEATDAYS))

energyInsecure2 <- df%>%
  filter(NOHEATDAYS != -2)%>%
  group_by(incomeGrp)%>%
  summarise(mean(NOHEATDAYS))

df2 <- OR_ACS%>%
  select(TYPEHUGQ, BLD, NP, HINCP, ADJINC, ADJHSG, HHLDRRAC1P, TEN, HFL, FULP, ELEP, GASP, FS, 
         WGTP, HUGCL, ADJHSG, HUPAC, LNGI, ELEFP, FULFP, RNTP)%>%
  mutate(adjustedIncome = HINCP*ADJINC,
         adjustedFuelSpend = FULP*ADJHSG,
         adjustedEleSpend = ELEP*ADJHSG,
         adjustedGasSpend = GASP*ADJHSG,
         energyBurden = adjustedFuelSpend/adjustedIncome,
         energyBurdenAmt = case_when(
           energyBurden >= .06 ~ 1,
           energyBurden >= .10 ~ 2))


  
  
  
  