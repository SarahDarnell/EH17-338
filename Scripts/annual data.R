#Annual Data - years 1-3
#Written by Sarah Darnell

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tableone)

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/EH17-338/EH17-338")

#import annual data
annuals <- read_csv("Raw Data/EH17338EMPATHY-AnnualData_DATA_2026-02-10_1113.csv")

#change redcap event names to be more readable
annuals <- annuals %>%
  mutate(redcap_event_name = case_when(
    redcap_event_name == "annual_year_1_arm_1" ~ "year1", 
    redcap_event_name == "annual_year_2_arm_1" ~ "year2",
    redcap_event_name == "annual_year_3_arm_1" ~ "year3"
  ))

#create dummy variable for ocp usage
annuals <- annuals %>%
  mutate(ocps = case_when (
    str_detect(tolower(annual_q3), "iud|testosterone|plan") ~ 0, 
    is.na(annual_q3) ~ 0, 
    TRUE ~ 1
  )) 

#Ensure pain killer usage and ocps are factor vars
annuals <- annuals %>%
  mutate(annual_q7 = as.factor(annual_q7)) %>%
  mutate(ocps = as.factor(ocps))


#filter out nsaid, pain, and ocp vars per year
annuals_y1_subset <- annuals %>%
  filter(redcap_event_name == "year1") %>%
  filter(annual_followup_questionnaire_child_complete == 2) %>%
  select(record_id, annual_q1, annual_q6, annual_q7, ocps)
annuals_y2_subset <- annuals %>%
  filter(redcap_event_name == "year2") %>%
  filter(annual_followup_questionnaire_child_complete == 2) %>%
  select(record_id, annual_q1, annual_q6, annual_q7, ocps)
annuals_y3_subset <- annuals %>%
  filter(redcap_event_name == "year3") %>%
  filter(annual_followup_questionnaire_child_complete == 2) %>%
  select(record_id, annual_q1, annual_q6, annual_q7, ocps)

#table menstrual pain based on ocp usage at y1
mp_y1_ocps <- annuals_y1_subset %>%
  select(annual_q6, ocps) %>%
  pivot_longer(cols = -ocps, names_to = "Item", values_to = "Value") %>% 
  group_by(ocps, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = ocps, values_from = `Median [IQR]`) 

#table menstrual pain based on ocp usage at y2
mp_y2_ocps <- annuals_y2_subset %>%
  select(annual_q6, ocps) %>%
  pivot_longer(cols = -ocps, names_to = "Item", values_to = "Value") %>% 
  group_by(ocps, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = ocps, values_from = `Median [IQR]`) 


#table menstrual pain based on ocp usage at y3
mp_y3_ocps <- annuals_y3_subset %>%
  select(annual_q6, ocps) %>%
  pivot_longer(cols = -ocps, names_to = "Item", values_to = "Value") %>% 
  group_by(ocps, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = ocps, values_from = `Median [IQR]`) 

#merge tables together, add variable for annual year
mp_ocps <- bind_rows(
  mp_y1_ocps %>% mutate(annual_year = 1),
  mp_y2_ocps %>% mutate(annual_year = 2),
  mp_y3_ocps %>% mutate(annual_year = 3)
)

ggplot(annuals, aes(x = ocps, y = annual_q6, group = redcap_event_name, color = ocps)) +
  geom_boxplot(
    aes(group = ocps),
    width = 0.4,
    alpha = 0.6,
    outlier.shape = NA  
  ) +
  labs(
    title = "Menstrual Pain and OCP usage",
    x = "",
    y = "Menstrual Pain, avg last 3 months"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ redcap_event_name, strip.position = "bottom")

#table pain killer usage based on ocp usage at y1
pk_y1_ocps <- CreateTableOne("annual_q7", data = annuals_y1_subset, factorVars = "annual_q7", 
                       strata = "ocps")

pk_y1_ocps <- as.data.frame(print(pk_y1_ocps, 
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))

#table pain killer usage based on ocp usage at y2
pk_y2_ocps <- CreateTableOne("annual_q7", data = annuals_y2_subset, factorVars = "annual_q7", 
                       strata = "ocps")

pk_y2_ocps <- as.data.frame(print(pk_y2_ocps, 
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))

#table pain killer usage based on ocp usage at y1
pk_y3_ocps <- CreateTableOne("annual_q7", data = annuals_y3_subset, factorVars = "annual_q7", 
                       strata = "ocps")

pk_y3_ocps <- as.data.frame(print(pk_y3_ocps, 
                               printToggle = FALSE,
                               quote = FALSE,
                               noSpaces = TRUE,
                               showAllLevels = TRUE))


#merge tables together, add variable for annual year
pk_ocps <- bind_rows(
  pk_y1_ocps %>% mutate(annual_year = 1),
  pk_y2_ocps %>% mutate(annual_year = 2),
  pk_y3_ocps %>% mutate(annual_year = 3)
)
