#################### FILE HISTORY ##################
##### Original file created by Willa Perlman 10/20/23
##### For use in PSU energy economics course term paper

################### LOAD PACKAGES ##################

library(tidyverse)
library(dplyr)
library(writexl)
library(readxl)
library(readr)
library(AER)
library(ggplot2)

#################### LOAD DATA ####################

RECS <- read_csv("/Users/willacperlman/recs2020_public_v5.csv")
OR_ACS <- read_csv("/Users/willacperlman/psam_h41.csv")
### WA_ACS <- read_csv("/Users/willacperlman/psam_h53.csv")

########## RECODE ENERGY SECURITY VARIABLES OF INTEREST #########

df <- as_tibble(RECS)%>%
  mutate(SCALEB2 = case_when(
    SCALEB == 1 ~ 3,
    SCALEB == 3 ~ 1,
    .default = as.numeric(SCALEB)
  ))%>%
  mutate(SCALEG2 = case_when(
    SCALEG == 1 ~ 3,
    SCALEG == 3 ~ 1,
    .default = as.numeric(SCALEG)
))%>%
  mutate(SCALEE2 = case_when(
    SCALEE == 1 ~ 3,
    SCALEE == 3 ~ 1,
    .default = as.numeric(SCALEE)
  ))

####### COMPUTE ACS METRICS OF INTEREST #####
####### DOING SOMETHING WRONG HERE ##########

df2 <- OR_ACS

df2%>%
  select(BLD,TYPEHUGQ,WGTP)%>%
  mutate(buildingType = case_when(
    BLD == "01" ~ "Mobile home or trailer",
    BLD == "02" ~ "One-family house detached",
    BLD == "03" ~ "One family house attached",
    BLD == "04" ~ "2 Apartments",
    BLD == "05" ~ "3-4 Apartments",
    BLD == "06" ~ "5-9 Apartments",
    BLD == "07" ~ "10-19 Apartments",
    BLD == "08" ~ "20-49 Apartments",
    BLD == "09" ~ "50+ Apartments",
    BLD == "10" ~ "Boat, RV, van, etc."
  ))

result <- df2%>%
  group_by(buildingType)%>%
  summarise(Percentage = n() / nrow(OR_ACS) * 100)

print(result)

########## COMPUTE CORRELATIONS #########

C1 <- cor(df$TYPEHUQ,df$SCALEB2)
C2 <- cor(df$TYPEHUQ, df$SCALEG2)
C3 <- cor(df$TYPEHUQ, df$SCALEE2)
C4 <- cor(df$TYPEHUQ, df$MONEYPY)
C5 <- cor(df$TYPEHUQ,df$HOUSEHOLDER_RACE)
C6 <- cor(df$MONEYPY,df$HOUSEHOLDER_RACE)
C7 <- cor(df$KOWNRENT, df$SCALEG2)
C8 <- cor(df$KOWNRENT, df$SCALEB2)
C9 <- cor(df$TYPEHUQ, df$SCALEE2)

##compute first-stage regression, fraction of explained variation

R1 <- summary(lm(df$TYPEHUQ ~ df$HOUSEHOLDER_RACE))$r.squared
R2 <- summary(lm(df$TYPEHUQ ~ df$SCALEB2))$r.squared
R3 <- summary(lm(df$KOWNRENT ~ df$SCALEB2))$r.squared

## estimate IV regression of log(scaleg) on ??
## in progress -- quite confused

scaleg_mod_iv1 <- ivreg(log(df$SCALEG) ~ df$TYPEHUQ | df$HOUSEHOLDER_RACE)

scaleg_mod_iv2 <- ivreg(log(SCALEG) ~ )
