
# load packages
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
library(lmerTest)
library(lme4)
library(rstatix)
library(effectsize)
library(psych)


# clean environment
rm(list = ls(all = TRUE))


# read raw data
data_raw <- utils::read.csv("data_project_1011770_2021_07_28.csv", header = TRUE, sep = ";")


# clean data
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
         !((condition == 1 & received_tosses <= 15) | (condition == 2 & received_tosses > 15))) %>% 
  mutate(condition = case_when(condition == 1 ~ "included",
                               condition == 2 ~ "excluded"),
         sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female",
                         sex == 3 ~ "other"),
         age = as.numeric(age)) %>% 
  relocate(condition) %>%
  rowid_to_column(var = "id") %>%
  arrange(id)

data_clean %<>% as_tibble()



# Analysis ----


# prepare data for analysis
data_prep <- data_clean %>% 
  mutate(# create one variable with the average of all need threats
    nt = (nt1 + nt2 + nt3 + nt4) / 4, 
    # recode all reversed scored items of the big five questionnaire
    b5_e1 = 6 - b5_e1_r,
    b5_c1 = 6 - b5_c1_r,
    b5_n1 = 6 - b5_n1_r,
    b5_o1 = 6 - b5_o1_r,
    b5_a2 = 6 - b5_a2_r,
    # calculate average score for each big five trait
    b5_e = (b5_e1 + b5_e2) / 2,
    b5_c = (b5_c1 + b5_c2) / 2,
    b5_n = (b5_n1 + b5_n2) / 2,
    b5_o = (b5_o1 + b5_o2) / 2,
    b5_a = (b5_a1 + b5_a2) / 2,
    # calculate average score for each trait in task 1 with values between 0 and 1
    task1_a = 2 - ((task1_1 + task1_6 + task1_11 + task1_16 + task1_21 + task1_26 + task1_35 + task1_36) / 8),
    task1_c = 2 - ((task1_2 + task1_7 + task1_12 + task1_17 + task1_22 + task1_27 + task1_31 + task1_40) / 8),
    task1_e = 2 - ((task1_3 + task1_8 + task1_13 + task1_18 + task1_23 + task1_28 + task1_32 + task1_37) / 8),
    task1_n = 2 - ((task1_4 + task1_9 + task1_14 + task1_19 + task1_24 + task1_29 + task1_33 + task1_38) / 8),
    task1_o = 2 - ((task1_5 + task1_10 + task1_15 + task1_20 + task1_25 + task1_30 + task1_34 + task1_39) / 8),
    # calculate average score for each trait in task 2
    task2_e = (task2_1 + task2_2 + (8 - task2_11) + (8 - task2_13)) / 4,
    task2_n = (task2_3 + task2_4 + (8 - task2_12) + (8 - task2_14)) / 4,
    task2_a = (task2_5 + task2_6 + (8 - task2_15) + (8 - task2_19)) / 4,
    task2_c = (task2_7 + task2_8 + (8 - task2_16) + (8 - task2_17)) / 4,
    task2_o = (task2_9 + task2_10 + (8 - task2_18) + (8 - task2_20)) / 4) %>% 
  # throw out reverse scored items
  select(-c(b5_e1_r, b5_c1_r, b5_n1_r, b5_o1_r, b5_a2_r, b5_a1, b5_e2, b5_c2, b5_n2, b5_o2, b5_e1, b5_c1, b5_n1, b5_o1, b5_a2, native, no_native, use_data, use_data_reason, comments, consent, seriousness, occupation, nt1, nt2, nt3, nt4, received_tosses, attention_check)) %>% 
  # order the columns
  relocate(excluded, ignored, sex, age, nt, 
           .before = mood) %>% 
  relocate(b5_e, b5_c, b5_n, b5_o, b5_a, task1_a, task1_c, task1_e, task1_n, task1_o, task2_a, task2_c, task2_e, task2_n, task2_o, 
           .before = task1_1)


# demographics
describe(data_prep)
table(data_prep$sex)

# test need threat variable for normal distribution
# normal distribution is given
shapiro.test(data_prep$nt)

ggplot(data_prep, aes(fill = condition, x = nt)) +
  geom_bar(position = "stack")


# influence of cyberball on need threats
tt_nt <- t.test(nt ~ condition, data = data_prep) # p-value < 2.2e-16
tt_nt
cohens_d(nt ~ condition, data = data_prep)

# descriptive statistics of need threats
data_prep %>%
  group_by(condition) %>%
  get_summary_stats(nt, type = "mean_sd")



## Task 1 ----


# test task 1 variables for normal distribution
# normal distribution is given in all cases
shapiro.test(data_prep$task1_a)

shapiro.test(data_prep$task1_c)

shapiro.test(data_prep$task1_e)

shapiro.test(data_prep$task1_n)

shapiro.test(data_prep$task1_o)


# descriptive statistics of task 1
data_prep %>%
  group_by(condition) %>%
  get_summary_stats(task1_a, task1_c, task1_e, task1_n, task1_o, type = "mean_sd") %>% 
  arrange(variable)


# calculate t-test for every trait - influence of condition on each trait preference and adjusting the p-value with the bonferroni-method

# agreeableness
tt1_a <- t.test(task1_a ~ condition, data = data_prep)
tt1_a # p-value = 0.5405
# bonferroni method
tt1_a_p <- p.adjust(tt1_a$p.value, method = "bonferroni", n = length(tt1_a$p.value))
tt1_a_p # 0.5404828
# effect size
tt1_es_a <- cohens_d(task1_a ~ condition, data = data_prep)
tt1_es_a

# conscientiousness
tt1_c <- t.test(task1_c ~ condition, data = data_prep)
tt1_c # p-value = 0.5775
# bonferroni method
tt1_c_p <- p.adjust(tt1_c$p.value, method = "bonferroni", n = length(tt1_c$p.value))
tt1_c_p # 0.5774825
# effect size
tt1_es_c <- cohens_d(task1_c ~ condition, data = data_prep)
tt1_es_c

# extraversion
tt1_e <- t.test(task1_e ~ condition, data = data_prep)
tt1_e # p-value = 0.4087
# bonferroni method
tt1_e_p <- p.adjust(tt1_e$p.value, method = "bonferroni", n = length(tt1_e$p.value))
tt1_e_p # 0.4086965
# effect size
tt1_es_e <- cohens_d(task1_e ~ condition, data = data_prep)
tt1_es_e

# neuroticism
tt1_n <- t.test(task1_n ~ condition, data = data_prep)
tt1_n # p-value = 0.3328
# bonferroni method
tt1_n_p <- p.adjust(tt1_n$p.value, method = "bonferroni", n = length(tt1_n$p.value))
tt1_n_p # 0.3328037
# effect size
tt1_es_n <- cohens_d(task1_n ~ condition, data = data_prep)
tt1_es_n

# openness
tt1_o <- t.test(task1_o ~ condition, data = data_prep)
tt1_o # p-value = 0.8599
# bonferroni method
tt1_o_p <- p.adjust(tt1_o$p.value, method = "bonferroni", n = length(tt1_o$p.value))
tt1_o_p # 0.8599382
# effect size
tt1_es_o <- cohens_d(task1_o ~ condition, data = data_prep)
tt1_es_o


# create long format
data_long <- data_prep %>% 
  pivot_longer(cols = c(task1_a, task1_c, task1_e, task1_n, task1_o), 
               names_to = "task1", 
               values_to = "task1_value") %>% 
  relocate(task1, task1_value, .before = task2_a)
  
merging <- data_prep %>% 
  pivot_longer(cols = c(task2_a, task2_c, task2_e, task2_n, task2_o), names_to = "task2", values_to = "task2_value", ) %>% 
  select(task2, task2_value)

data_long %<>% bind_cols(merging) %>% 
  relocate(task2, task2_value, .after = task1_value)



# ANOVA!!!!!!!!!!!
# overall_anova1 <- lmer(task1_value ~ condition * task1 + (task1 | id), data = data_long)
# anova(overall_anova1)
# eta_squared(overall_anova1, partial = FALSE)

overall_anova1 <- data_long %>%
  anova_test(dv = task1_value, 
             wid = id, 
             within = task1, 
             between = condition,
             effect.size = "ges") %>%
  get_anova_table()
overall_anova1

# pwc1 <- data_long %>%
#   pairwise_t_test(task1_value ~ task1, p.adjust.method = "bonferroni")
# pwc1




## Task 2 ----


# test task 2 variables for normal distribution
# normal distribution is given in all cases
shapiro.test(data_prep$task2_a)

shapiro.test(data_prep$task2_c)

shapiro.test(data_prep$task2_e)

shapiro.test(data_prep$task2_n)

shapiro.test(data_prep$task2_o)


# descriptive statistics of task 2
data_prep %>%
  group_by(condition) %>%
  get_summary_stats(task2_a, task2_c, task2_e, task2_n, task2_o, type = "mean_sd") %>% 
  arrange(variable)


# calculate t-test for every trait - influence of condition on each trait rating

# agreeableness
tt2_a <- t.test(task2_a ~ condition, data = data_prep)
tt2_a # p-value = 0.7803
tt2_es_a <- cohens_d(task2_a ~ condition, data = data_prep)
tt2_es_a

# conscientiousness
tt2_c <- t.test(task2_c ~ condition, data = data_prep)
tt2_c # p-value = 0.5174
tt2_es_c <- cohens_d(task2_c ~ condition, data = data_prep)
tt2_es_c

# extraversion
tt2_e <- t.test(task2_e ~ condition, data = data_prep)
tt2_e # p-value = 0.2256 
tt2_es_e <- cohens_d(task2_e ~ condition, data = data_prep)
tt2_es_e

# neuroticism
tt2_n <- t.test(task2_n ~ condition, data = data_prep)
tt2_n # p-value = 0.3778
tt2_es_n <- cohens_d(task2_n ~ condition, data = data_prep)
tt2_es_n

# openness
tt2_o <- t.test(task2_o ~ condition, data = data_prep)
tt2_o # p-value = 0.4409
tt2_es_o <- cohens_d(task2_o ~ condition, data = data_prep)
tt2_es_o


# calculate average score for each trait and direction in task 2

# agreeableness
data_prep_a <- data_prep %>% 
  mutate(task2_a_e = (task2_5 + task2_6) / 2,
         task2_a_r = ((8 - task2_15) + (8 - task2_19)) / 2) %>% 
  pivot_longer(cols = c(task2_a_e, task2_a_r), 
               names_to = "task2_a_type", 
               values_to = "task2_a_value")

# conscientiousness
data_prep_c <- data_prep %>%
  mutate(task2_c_e = (task2_7 + task2_8) / 2,
         task2_c_r = ((8 - task2_16) + (8 - task2_17)) / 2) %>% 
  pivot_longer(cols = c(task2_c_e, task2_c_r), 
               names_to = "task2_c_type", 
               values_to = "task2_c_value")

# extraversion
data_prep_e <- data_prep %>%
  mutate(task2_e_e = (task2_1 + task2_2) / 2,
         task2_e_r = ((8 - task2_11) + (8 - task2_13)) / 2) %>% 
  pivot_longer(cols = c(task2_e_e, task2_e_r), 
               names_to = "task2_e_type", 
               values_to = "task2_e_value")

# neuroticism
data_prep_n <- data_prep %>%
  mutate(task2_n_e = (task2_3 + task2_4) / 2,
         task2_n_r = ((8 - task2_12) + (8 - task2_14)) / 2) %>% 
  pivot_longer(cols = c(task2_n_e, task2_n_r), 
               names_to = "task2_n_type", 
               values_to = "task2_n_value")

# openness
data_prep_o <- data_prep %>%
  mutate(task2_o_e = (task2_9 + task2_10) / 2,
         task2_o_r = ((8 - task2_18) + (8 - task2_20)) / 2) %>% 
  pivot_longer(cols = c(task2_o_e, task2_o_r), 
               names_to = "task2_o_type", 
               values_to = "task2_o_value")


# ANOVA with direction of trait manipulation as a factor

# agreeableness
t2_anova_a <- lmer(task2_a_value ~ condition * task2_a_type + (task2_a_type | id), data = data_prep_a)
anova(t2_anova_a)
eta_squared(t2_anova_a, partial = FALSE)

# agreeableness
t2_anova_a <- lmer(task2_a_value ~ condition * task2_a_type + (1 | id), data = data_prep_a)
anova(t2_anova_a)
eta_squared(t2_anova_a, partial = FALSE)

# conscientiousness
t2_anova_c <- lmer(task2_c_value ~ condition * task2_c_type + (1 | id), data = data_prep_c)
anova(t2_anova_c)
eta_squared(t2_anova_c, partial = FALSE)

# extraversion
t2_anova_e <- lmer(task2_e_value ~ condition * task2_e_type + (1 | id), data = data_prep_e)
anova(t2_anova_e)
eta_squared(t2_anova_e, partial = FALSE)

# neuroticism
t2_anova_n <- lmer(task2_n_value ~ condition * task2_n_type + (1 | id), data = data_prep_n)
anova(t2_anova_n)
eta_squared(t2_anova_n, partial = FALSE)

# openness
t2_anova_o <- lmer(task2_o_value ~ condition * task2_o_type + (1 | id), data = data_prep_o)
anova(t2_anova_o)
eta_squared(t2_anova_o, partial = FALSE)


# ANOVA!!!!!!!!!
overall_anova2 <- lmer(task2_value ~ condition * task2 + (1 | id), data = data_long)
anova(overall_anova2)
eta_squared(overall_anova2, partial = FALSE)

overall_anova2 <- data_long %>%
  anova_test(dv = task2_value, 
             wid = id, 
             within = task2, 
             between = condition,
             effect.size = "ges") %>%
  get_anova_table()
overall_anova2



## Moderator variable ----

# calculate linear model for every trait - influence of each big five trait (moderator) on the effect of condition on the big five ratings

# agreeableness
fitmod_a <- lm(task1_a ~ condition + b5_a + condition:b5_a, data = data_prep)
summary(fitmod_a) # 0.407

# conscientiousness
fitmod_c <- lm(task1_c ~ condition + b5_c + condition:b5_c, data = data_prep)
summary(fitmod_c) # 0.774

# extraversion
fitmod_e <- lm(task1_e ~ condition + b5_e + condition:b5_e, data = data_prep)
summary(fitmod_e) # 0.347

# neuroticism
fitmod_n <- lm(task1_n ~ condition + b5_n + condition:b5_n, data = data_prep)
summary(fitmod_n) # 0.579

# openness
fitmod_o <- lm(task1_o ~ condition + b5_o + condition:b5_o, data = data_prep)
summary(fitmod_o) # 0.0438




# data preperation for t-test on rating accuracy
data_e_r <- data_prep %>% 
  mutate(task2_enhanced = (task2_5 + task2_6 + task2_7 + task2_8 + task2_1 + task2_2 + task2_3 + task2_4 + task2_9 + task2_10) / 10,
         task2_reduced = (task2_15 + task2_19 + task2_16 + task2_17 + task2_11 + task2_13 + task2_12 + task2_14 + task2_18 + task2_20) / 10)


tt_enhanced <- t.test(data_e_r$task2_enhanced, mu = 4, alternative = "greater")
tt_enhanced
tt_enhanced_es <- cohens_d(tt_enhanced)
tt_enhanced_es
sd(data_e_r$task2_enhanced)

tt_reduced <- t.test(data_e_r$task2_reduced, mu = 4, alternative = "less")
tt_reduced
tt_reduced_es <- cohens_d(tt_reduced)
tt_reduced_es
sd(data_e_r$task2_reduced)



data_individual <- data_prep %>% 
  mutate(task2_a_e = (task2_5 + task2_6) / 2,
         task2_a_r = (task2_15 + task2_19) / 2,
         task2_c_e = (task2_7 + task2_8) / 2,
         task2_c_r = (task2_16 + task2_17) / 2,
         task2_e_e = (task2_1 + task2_2) / 2,
         task2_e_r = (task2_11 + task2_13) / 2,
         task2_n_e = (task2_3 + task2_4) / 2,
         task2_n_r = (task2_12 + task2_14) / 2,
         task2_o_e = (task2_9 + task2_10) / 2,
         task2_o_r = (task2_18 + task2_20) / 2)


tt_enhanced_a <- t.test(data_individual$task2_a_e, mu = 4, alternative = "greater")
tt_enhanced_a
tt_enhanced_a_es <- cohens_d(tt_enhanced_a)
tt_enhanced_a_es
sd(data_individual$task2_a_e)

tt_enhanced_c <- t.test(data_individual$task2_c_e, mu = 4, alternative = "greater")
tt_enhanced_c
tt_enhanced_c_es <- cohens_d(tt_enhanced_c)
tt_enhanced_c_es
sd(data_individual$task2_c_e)

tt_enhanced_e <- t.test(data_individual$task2_e_e, mu = 4, alternative = "greater")
tt_enhanced_e
tt_enhanced_e_es <- cohens_d(tt_enhanced_e)
tt_enhanced_e_es
sd(data_individual$task2_e_e)

tt_enhanced_n <- t.test(data_individual$task2_n_e, mu = 4, alternative = "greater")
tt_enhanced_n
tt_enhanced_n_es <- cohens_d(tt_enhanced_n)
tt_enhanced_n_es
sd(data_individual$task2_n_e)

tt_enhanced_o <- t.test(data_individual$task2_o_e, mu = 4, alternative = "greater")
tt_enhanced_o
tt_enhanced_o_es <- cohens_d(tt_enhanced_o)
tt_enhanced_o_es
sd(data_individual$task2_o_e)


tt_reduced_a <- t.test(data_individual$task2_a_r, mu = 4, alternative = "less")
tt_reduced_a
tt_reduced_a_es <- cohens_d(tt_reduced_a)
tt_reduced_a_es
sd(data_individual$task2_a_r)

tt_reduced_c <- t.test(data_individual$task2_c_r, mu = 4, alternative = "less")
tt_reduced_c
tt_reduced_c_es <- cohens_d(tt_reduced_c)
tt_reduced_c_es
sd(data_individual$task2_c_r)

tt_reduced_e <- t.test(data_individual$task2_e_r, mu = 4, alternative = "less")
tt_reduced_e
tt_reduced_e_es <- cohens_d(tt_reduced_e)
tt_reduced_e_es
sd(data_individual$task2_e_r)

tt_reduced_n <- t.test(data_individual$task2_n_r, mu = 4, alternative = "less")
tt_reduced_n
tt_reduced_n_es <- cohens_d(tt_reduced_n)
tt_reduced_n_es
sd(data_individual$task2_n_r)

tt_reduced_o <- t.test(data_individual$task2_o_r, mu = 4, alternative = "less")
tt_reduced_o
tt_reduced_o_es <- cohens_d(tt_reduced_o)
tt_reduced_o_es
sd(data_individual$task2_o_r)





