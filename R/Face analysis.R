

library(ggplot2)
library(ggpubr)
library(Matrix)
library(languageR)
library(lsr)
library(reshape2)
library(rcompanion)
library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)



rm(list = ls(all = TRUE))


data_raw <- utils::read.csv("data_project_1011770_2021_07_28.csv", header = TRUE, sep = ";")

data_clean <-
  data_raw %>%
  select(-c(1:6, 11, 100:150)) %>% 
  rename(attention_check = v_169) %>% 
  filter(condition != "-77",
         use_data == 2,
         seriousness > 4,
         consent == 1,
         cyberball_played == 2,
         attention_check == 2,
         !((condition == 1 & received_tosses <= 15) | (condition == 2 & received_tosses >= 15))) %>% 
  mutate(condition = case_when(condition == 1 ~ "included",
                               condition == 2 ~ "excluded")) %>% 
  relocate(condition) %>%
  rowid_to_column(var = "id") %>%
  arrange(id)

data_clean %<>% as_tibble()



# Analysis



data_prep <- data_clean %>% 
  mutate(nt = (nt1 + nt2 + nt3 + nt4) / 4,
         b5_e1 = 6 - b5_e1_r,
         b5_c1 = 6 - b5_c1_r,
         b5_n1 = 6 - b5_n1_r,
         b5_o1 = 6 - b5_o1_r,
         b5_a2 = 6 - b5_a2_r,
         b5_e = (b5_e1 + b5_e2) / 2,
         b5_c = (b5_c1 + b5_c2) / 2,
         b5_n = (b5_n1 + b5_n2) / 2,
         b5_o = (b5_o1 + b5_o2) / 2,
         b5_a = (b5_a1 + b5_a2) / 2,
         task1_a = 2 - ((task1_1 + task1_6 + task1_11 + task1_16 + task1_21 + task1_26 + task1_35 + task1_36) / 8),
         task1_c = 2 - ((task1_2 + task1_7 + task1_12 + task1_17 + task1_22 + task1_27 + task1_31 + task1_40) / 8),
         task1_e = 2 - ((task1_3 + task1_8 + task1_13 + task1_18 + task1_23 + task1_28 + task1_32 + task1_37) / 8),
         task1_n = 2 - ((task1_4 + task1_9 + task1_14 + task1_19 + task1_24 + task1_29 + task1_33 + task1_38) / 8),
         task1_o = 2 - ((task1_5 + task1_10 + task1_15 + task1_20 + task1_25 + task1_30 + task1_34 + task1_39) / 8),
         task2_e = (task2_1 + task2_2 + (8 - task2_11) + (8 - task2_13)) / 4,
         task2_n = (task2_3 + task2_4 + (8 - task2_12) + (8 - task2_14)) / 4,
         task2_a = (task2_5 + task2_6 + (8 - task2_15) + (8 - task2_19)) / 4,
         task2_c = (task2_7 + task2_8 + (8 - task2_16) + (8 - task2_17)) / 4,
         task2_o = (task2_9 + task2_10 + (8 - task2_18) + (8 - task2_20)) / 4) %>% 
  select(-c(b5_e1_r, b5_c1_r, b5_n1_r, b5_o1_r, b5_a2_r)) %>% 
  relocate(nt, .before = nt1) %>% 
  relocate(b5_e, b5_c, b5_n, b5_o, b5_a, b5_e1, b5_c1, b5_n1, b5_o1, b5_a2, .before = b5_a1) %>% 
  relocate(task1_a, task1_c, task1_e, task1_n, task1_o, task2_a, task2_c, task2_e, task2_n, task2_o, .before = task1_1)


shapiro.test(data_prep$nt)

ggplot(data_prep, aes(fill = condition, x = nt)) +
  geom_bar(position = "stack")
  
  
data_prep %<>% mutate(nt_z = scale(nt)) %>% 
  relocate(nt_z, .before = nt)

# Influence of cyberball on need threats
t.test(nt_z ~ condition, data = data_prep)
# p-value < 2.2e-16


shapiro.test(data_prep$task1_a)

shapiro.test(data_prep$task1_c)

shapiro.test(data_prep$task1_e)

shapiro.test(data_prep$task1_n)

shapiro.test(data_prep$task1_o)


wilcox.test(task1_a ~ condition, data = data_prep)

wilcox.test(task1_c ~ condition, data = data_prep)

wilcox.test(task1_e ~ condition, data = data_prep)

wilcox.test(task1_n ~ condition, data = data_prep)

wilcox.test(task1_o ~ condition, data = data_prep)


neugier <- data_prep %>% mutate(task1_a_z = scale(task1_a))

t.test(task1_a_z ~ condition, data = neugier)


shapiro.test(data_prep$task2_a)

shapiro.test(data_prep$task2_c)

shapiro.test(data_prep$task2_e)

shapiro.test(data_prep$task2_n)

shapiro.test(data_prep$task2_o)


t.test(task2_a ~ condition, data = data_prep)

t.test(task2_c ~ condition, data = data_prep)

t.test(task2_e ~ condition, data = data_prep)

t.test(task2_n ~ condition, data = data_prep)

t.test(task2_o ~ condition, data = data_prep)



fitmod_a <- lm(task2_a ~ condition + b5_a + condition:b5_a, data = data_prep)
summary(fitmod_a)

fitmod_c <- lm(task2_c ~ condition + b5_c + condition:b5_c, data = data_prep)
summary(fitmod_c)

fitmod_e <- lm(task2_e ~ condition + b5_e + condition:b5_e, data = data_prep)
summary(fitmod_e)

fitmod_n <- lm(task2_n ~ condition + b5_n + condition:b5_n, data = data_prep)
summary(fitmod_n)

fitmod_o <- lm(task2_o ~ condition + b5_o + condition:b5_o, data = data_prep)
summary(fitmod_o)


