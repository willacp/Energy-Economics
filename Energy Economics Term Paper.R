#################### FILE HISTORY ##################
##### Original file created by Willa Perlman 10/20/23
##### For use in PSU energy economics course term paper

################### LOAD PACKAGES ##################

library(tidyverse)
library(dplyr)
library(writexl)
library(readxl)
library(readr)
library(logitr)
library(mlogit)
library(AER)
library(ggplot2)
library(haven)
library(RegUtils)
library(maxLik)
library(miscTools)
library(censReg)

#################### LOAD DATA ####################

RECS <- read_sas("/Users/willacperlman/recs2020_public_v5.sas7bdat")
OR_ACS <- read_csv("/Users/willacperlman/psam_h41.csv")
### WA_ACS <- read_csv("/Users/willacperlman/psam_h53.csv")

########## ENERGY INSECURITY VARIABLES OF INTEREST #########

####### SELECT & RECODE RECS DATA #####

df <- RECS%>%
  select(TYPEHUQ, KOWNRENT, NOHEATDAYS, NOACDAYS, MONEYPY, HOUSEHOLDER_RACE, NHSLDMEM,
         NUMCHILD, NUMADULT2, ATHOME)%>%
  mutate(buildingType = case_when(
    TYPEHUQ == "1" ~ "Mobile home",
    TYPEHUQ == "2" ~ "Single-family detached",
    TYPEHUQ == "3" ~ "Single-family attached",
    TYPEHUQ == "4" ~ "Multi-family small",
    TYPEHUQ == "5" ~ "Multi-family large"
  ))

####### CALCULATE PROPORTION OF DWELLINGS #####
####### DO I NEED TO USE WEIGHTING HERE?

result <- df%>%
  group_by(buildingType)%>%
  summarise(Percentage = n() / nrow(RECS) * 100)

print(result)

######### RUN TOBIT ########

est.tobit <- tobit(NOHEATDAYS ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE, left = 0, right = 366, data = df)

ivtobit <- 

########## JUNKYARD #########



##compute first-stage regression, fraction of explained variation

## R1 <- summary(mlogit(df2$buildingType ~ df2$HOUSEHOLDER_RACE))$r.squared
## R2 <- summary(lm(df$TYPEHUQ ~ df$SCALEB2))$r.squared
## R3 <- summary(lm(df$KOWNRENT ~ df$SCALEB2))$r.squared

## estimate IV regression of log(scaleg) on ??
## in progress -- quite confused

## scaleg_mod_iv1 <- ivreg(log(df$SCALEG) ~ df$TYPEHUQ | df$HOUSEHOLDER_RACE)

## scaleg_mod_iv2 <- ivreg(log(SCALEG) ~ )
