#Composite variables for empathy dataset#
#Written by Sarah Darnell, last updated 7.14.25

library(readr)
library(dplyr)
library(readr)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/EH17-338/EH17-338")

#import wide dataset from See Wan, collected 7.10.25
empathy_wide <- read_csv("Empathy_Actigraphy_Diaries_Wide_Values__SeeWan7.10.25.csv")

#Add new variables which hold sum of all promis ped short from v1.0
#psychological stress experiences for t0, t1, and t2

empathy_wide <- empathy_wide %>%
  mutate(psych_stress_sum_t0 = 
           promis_eos_p_011r1_c1b667t0 + promis_eos_p_064r1_a4c868t0 + 
           promis_eos_p_067r1_9222a6t0 + promis_eos_p_112r1_b3deadt0 +
           promis_eos_p_048r1_38a89at0 + promis_eos_p_063r1_3dce53t0 +
           promis_eos_p_105r1_7be006t0 + promis_eos_p_118r1_d63498t0) %>%
  mutate(psych_stress_sum_t1 = 
           promis_eos_p_011r1_c1b667t1 + promis_eos_p_064r1_a4c868t1 + 
           promis_eos_p_067r1_9222a6t1 + promis_eos_p_112r1_b3deadt1 +
           promis_eos_p_048r1_38a89at1 + promis_eos_p_063r1_3dce53t1 +
           promis_eos_p_105r1_7be006t1 + promis_eos_p_118r1_d63498t1) %>%
  mutate(psych_stress_sum_t2 = 
           promis_eos_p_011r1_c1b667t2 + promis_eos_p_064r1_a4c868t2 + 
           promis_eos_p_067r1_9222a6t2 + promis_eos_p_112r1_b3deadt2 +
           promis_eos_p_048r1_38a89at2 + promis_eos_p_063r1_3dce53t2 +
           promis_eos_p_105r1_7be006t2 + promis_eos_p_118r1_d63498t2)



#create lookup table of t-scores and SE from promis manual
psych_stress_lookup <- tibble(
  summed_score = 8:40,
  t_score = c(37.0, 42.6, 45.2, 47.2, 48.9, 50.3, 51.6, 52.8, 54.0, 55.0, 56.1,
              57.1, 58.1, 59.1, 60.1, 61.0, 62.0, 63.0, 63.9, 64.9, 65.9, 66.8,
              67.8, 68.8, 69.8, 70.8, 71.9, 73.0, 74.3, 75.6, 77.2, 79.1, 81.8),
  se = c(5.7, 4.0, 3.6, 3.2, 2.9, 2.8, 2.6, 2.6, 2.5, 2.5, 2.5,
         2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4,
         2.4, 2.4, 2.4, 2.4, 2.4, 2.5, 2.6, 2.7, 2.9, 3.1, 3.5)
)

#create new variables with t scores and SE for T0, T1, and T2 via joins
empathy_wide <- empathy_wide %>%
  # t0
  left_join(psych_stress_lookup, by = c("psych_stress_sum_t0" = "summed_score")) %>%
  rename(
    psych_stress_tscore_t0 = t_score,
    psych_stress_se_t0 = se
  ) %>%
  # t1
  left_join(psych_stress_lookup, by = c("psych_stress_sum_t1" = "summed_score")) %>%
  rename(
    psych_stress_tscore_t1 = t_score,
    psych_stress_se_t1 = se
  ) %>%
  # t2
  left_join(psych_stress_lookup, by = c("psych_stress_sum_t2" = "summed_score")) %>%
  rename(
    psych_stress_tscore_t2 = t_score,
    psych_stress_se_t2 = se
  )

#saving file
write_csv(empathy_wide, "C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/EH17-338/EH17-338/Empathy_Actigraphy_Diaries_Wide_Values__SeeWan7.10.25_COMPOSITES.csv")           

         
    


