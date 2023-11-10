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
library(glmmML)
library(ivprobit)

#################### LOAD DATA ####################

RECS <- read_sas("/Users/willacperlman/recs2020_public_v5.sas7bdat")
OR_ACS <- read_csv("/Users/willacperlman/psam_h41.csv")
### WA_ACS <- read_csv("/Users/willacperlman/psam_h53.csv")

########## ENERGY INSECURITY VARIABLES OF INTEREST #########

####### SELECT & RECODE RECS DATA #####

df <- RECS%>%
  filter(NOHEATDAYS != -2)%>%
  filter(NOACDAYS != -2)%>%
  filter(KOWNRENT != 3)%>%
  select(TYPEHUQ, KOWNRENT, NOHEATDAYS, NOACDAYS, MONEYPY, HOUSEHOLDER_RACE, NHSLDMEM,
         NUMCHILD, NUMADULT2, ATHOME, COLDMA, HOTMA, ENERGYASST20, ENERGYASST19, ENERGYASST18,
         ENERGYASST17, ENERGYASST16, NOACEL, NOACBROKE, NOACHELP, DOEID)%>%
  mutate(buildingType = case_when(
    TYPEHUQ == "1" ~ "Mobile home",
    TYPEHUQ == "2" ~ "Single-family detached",
    TYPEHUQ == "3" ~ "Single-family attached",
    TYPEHUQ == "4" ~ "Multi-family small",
    TYPEHUQ == "5" ~ "Multi-family large"
  ))

####### CALCULATE PROPORTION OF DWELLINGS #####

result <- df%>%
  group_by(buildingType)%>%
  summarise(Percentage = n() / nrow(RECS) * 100)

print(result)

######### CALCULATE PROPORTION OF DWELLINGS BY INCOME AND RACE #####
## in progress 

## resultInc <- df%>%
  
##  group_by(buildingType)%>%

######### CONVERT CATEGORICAL VARIABLES TO FACTORS ######

df$KOWNRENT <- as.factor(df$KOWNRENT)
df$TYPEHUQ <- as.factor(df$TYPEHUQ)
df$HOUSEHOLDER_RACE <- as.factor(df$HOUSEHOLDER_RACE)
df$NUMCHILD <- as.factor(df$NUMCHILD)
df$NUMADULT2 <- as.factor(df$NUMADULT2)


######### RUN TOBIT ########

#### attempt 1 with censReg #####

tobitModel <- censReg(NOHEATDAYS ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM,
                   left = 0, right = 366, data = df)

summary(tobitModel)

#### attempt 2 with censReg ####

tobitModel2 <- censReg(NOACDAYS ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM,
                      left = 0, right = 366, data = df)

summary(tobitModel2)

#### attempt 3 with censReg ####

tobitModel3 <- censReg(HOTMA ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM,
                       left = 0, right = 366, data = df)

summary(tobitModel3)

#### attempt with tobit ####

tobitModel <- tobit(HOTMA ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM,
                        left = 0, right = 366, data = df)

summary(tobitModel)

#### attempt with glm #####

glmModel <- glm(HOTMA ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM, family = binomial(link = "probit"), data = df)

summary(glmModel)

mlogitModel <- mlogit(HOTMA ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM, data = df)

#### attempt with ivprobit #####

ivprobModel <- ivprobit(HOTMA ~ TYPEHUQ + KOWNRENT + MONEYPY + HOUSEHOLDER_RACE + ATHOME + NHSLDMEM, data = df)

summary(ivprobModel)


########## JUNKYARD #########



##compute first-stage regression, fraction of explained variation

## R1 <- summary(mlogit(df2$buildingType ~ df2$HOUSEHOLDER_RACE))$r.squared
## R2 <- summary(lm(df$TYPEHUQ ~ df$SCALEB2))$r.squared
## R3 <- summary(lm(df$KOWNRENT ~ df$SCALEB2))$r.squared

## estimate IV regression of log(scaleg) on ??
## in progress -- quite confused

## scaleg_mod_iv1 <- ivreg(log(df$SCALEG) ~ df$TYPEHUQ | df$HOUSEHOLDER_RACE)

## scaleg_mod_iv2 <- ivreg(log(SCALEG) ~ )
