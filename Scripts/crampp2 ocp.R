#crampp2 ocp use - prelim data for renewal grant
#Written by Sarah Darnell, began 3.16.26

library(readxl)
library(dplyr)
library(tableone)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/EH17-338/EH17-338")

#import crampp2 ocp data
crampp2 <- read_excel("Raw Data/crampp2_ocp.xlsx")

#clean ocp var
crampp2 <- crampp2 %>%
  mutate(mh_ocps = case_when(
    is.na(mh_ocps) ~ 0, 
    .default = mh_ocps
  )) %>%
  mutate(mh_ocps = as.factor(mh_ocps))

#filter out ages above 25
crampp2 <- crampp2 %>%
  filter(Age < 26)

#compute avg_NMPP
crampp2 <- crampp2 %>%
  mutate(avg_NMPP = (mcgill_1 + mcgill_2 + mcgill_3)/3
  )

#table of nmpp, mp, age of pain, stratified by ocp use

#ensure factor vars are factors
crampp2 <- crampp2 %>%
  mutate(ibs_bl = as.factor(ibs_bl)) %>%
  mutate(mh18_painfulperiodsyn = as.factor(mh18_painfulperiodsyn)) %>%
  mutate(mh19 = as.factor(mh19)) 

#define vars for table
vars <- c("Age",
          "ibs_bl", 
          "max_pain_bl",
          "mh18_painfulperiodsyn",
          "mh19",
          "mh19a",
          "mh23",
          "avg_NMPP",
          "mcgill_1",
          "mcgill_2",
          "mcgill_3"
          )

#define factor vars for table
factor_vars <- c("ibs_bl", 
          "mh18_painfulperiodsyn",
          "mh19"
)

crampp2_ocps <- CreateTableOne(vars, data = crampp2, factorVars = factor_vars, 
                               strata = "mh_ocps")

crampp2_ocps_df <- as.data.frame(print(crampp2_ocps, 
                                       nonnormal = c("max_pain_bl", 
                                                     "mh19a", 
                                                     "mh23", 
                                                     "avg_NMPP", 
                                                     "Age", 
                                                     "mcgill_1", 
                                                     "mcgill_2", 
                                                     "mcgill_3"),
                                    printToggle = FALSE,
                                    quote = FALSE,
                                    noSpaces = TRUE,
                                    showAllLevels = TRUE))

#Creating table with comparisons for never OCP use vs current OCP use
crampp2_filtered <- crampp2 %>%
  mutate(mh_ocps = as.numeric(levels(mh_ocps))[mh_ocps]) %>%
  filter(mh_ocps != 2) %>%
  mutate(mh_ocps = as.factor(mh_ocps))


comp <- CreateTableOne(vars, data = crampp2_filtered, factorVars = factor_vars, 
                       strata = "mh_ocps")

comp_df <- as.data.frame(print(comp, 
                               nonnormal = c("max_pain_bl", 
                                             "mh19a", 
                                             "mh23", 
                                             "avg_NMPP", 
                                             "Age", 
                                             "mcgill_1", 
                                             "mcgill_2", 
                                             "mcgill_3"),
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE)) %>%
  dplyr::select("p")

#merge summary and comparison tables
crampp2_ocps_df_comb <- cbind(crampp2_ocps_df, p_0vs1 = comp_df$p)

sink("Logs/crampp2_ocp_log_3.16.26.txt")
print(crampp2_ocps_df_comb)
sink()


