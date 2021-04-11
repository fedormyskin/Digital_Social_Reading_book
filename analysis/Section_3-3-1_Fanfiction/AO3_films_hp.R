library(tidyverse)
library(lubridate)
library(scales)
library(Hmisc)
library(tidymodels)
library(lme4)
library(broom.mixed)

# load files for characters' screen time and clean them ---------------------

stone_screen_time <- read_csv("data/HP/1_stone_screen_time.csv", col_types = "ccdc")
chamber_screen_time <- read_csv("data/HP/2_chamber_screen_time.csv", col_types = "ccdc")
azkaban_screen_time <- read_csv("data/HP/3_azkaban_screen_time.csv", col_types = "ccdc")
goblet_screen_time <- read_csv("data/HP/4_goblet_screen_time.csv", col_types = "ccdc")
phoenix_screen_time <- read_csv("data/HP/5_phoenix_screen_time.csv", col_types = "ccdc")
half_blood_screen_time <- read_csv("data/HP/6_half-blood_screen_time.csv", col_types = "ccdc")
hallows1_screen_time <- read_csv("data/HP/7_hallows1_screen_time.csv", col_types = "ccdc")
hallows2_screen_time <- read_csv("data/HP/8_hallows2_screen_time.csv", col_types = "ccdc")

all_screen_time <- bind_rows(stone_screen_time, chamber_screen_time, azkaban_screen_time,
                         goblet_screen_time, phoenix_screen_time, half_blood_screen_time,
                         hallows1_screen_time, hallows2_screen_time)

screen_time_df <- all_screen_time %>%
  rename(canonical_tag = character) %>% 
  group_by(canonical_tag) %>% 
  mutate(index = cur_group_id()) %>% 
  relocate(index) %>% 
  mutate(screen_time = as.duration(ms(screen_time))) %>% 
  group_by(index) %>% 
  # calculate number of occurrences for the whole series
  mutate(grand_tot_films = sum(screen_time)) %>% 
  ungroup()

# total length of each film
# source: https://www.quora.com/How-long-are-each-of-the-Harry-Potter-films
movies_total_time <- as.numeric(dseconds(dminutes(c(152+161+142+157+138+153+146+130))))

# load AO3 Harry Potter data ----------------------------------------------

# load data (285 MB CSV file)
harry <- read_csv("https://osf.io/r97nu/download")

harry <- harry %>% 
  filter(language == "English") %>% # because I only have list of film characters in English
  filter(between(date_year, 2010, 2019))  # because AO3 was made public in November 2009

# Aggregate synonym tags ------------------------------------------------------
# see this paper for details about AO3 tag system and how the data has been obtained:
# https://github.com/fedormyskin/Linked-Potter/blob/master/Linked-potter_DHJapan2020.pdf

characters_syn <- read_csv("https://raw.githubusercontent.com/fedormyskin/Linked-Potter/master/queries/Character/characters_synonyms.csv")
freeforms_syn <- read_csv("https://raw.githubusercontent.com/fedormyskin/Linked-Potter/master/queries/Freeform/freeforms_synonyms.csv")
common_freeform_syn <- read_csv("https://raw.githubusercontent.com/fedormyskin/Linked-Potter/master/queries/Freeform/common_freeforms_synonyms.csv")

merge_freeforms_syn <- bind_rows(freeforms_syn, common_freeform_syn)

char_dictionary <- characters_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)
free_dictionary <- merge_freeforms_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)

harry_char <- harry %>%
  filter(tag_type == "characters") %>%
  # aggregate synonyms
  left_join(char_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))

harry_free <- harry %>%
  filter(tag_type=="freeforms") %>%
  left_join(free_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))

# build dataset including aggregated synonyms (includes only character and freeform tags)
harry_aggr <- bind_rows(harry_char, harry_free) 

# load list of characters with assigned race and gender (manually created)
checked_full_chars_list <- read_csv("data/HP/checked_full_chars_list.csv") %>% 
  select(-character) %>% 
  distinct(canonical_tag, .keep_all = TRUE)

# list characters to remove because they don't appear in the 8 HP films
fantastic_beast_new_chars <- c("Newt Scamander", "Original Percival Graves",
                               "Tina Goldstein", "Queenie Goldstein",
                               "Mary Lou Barebone", "Gellert Grindelwald")

# put all information together
harry_char <- harry_char %>% 
  # keep only stories with HP characters, no Alternate Universe settings or similar
  filter(canonical_tag %in% checked_full_chars_list$canonical_tag) %>% 
  # add info about gender and race
  left_join(checked_full_chars_list, by = "canonical_tag") %>% 
  filter(!canonical_tag %in% fantastic_beast_new_chars)

# calculate some descriptive statistics
AO3_total_stories <-  n_distinct(harry_char$index)

AO3_totals_intersection <- harry_char %>%
  group_by(gender, race) %>% 
  summarise(tot_stories = n_distinct(index),
            prop_stories = tot_stories / AO3_total_stories * 100)

total_AO3_stories_by_gender <-  harry_char %>%
  group_by(gender) %>% 
  summarise(tot_stories = n_distinct(index),
            prop_stories = tot_stories / AO3_total_stories * 100)

total_AO3_stories_by_race <-  harry_char %>%
  group_by(race) %>% 
  summarise(tot_stories = n_distinct(index),
            prop_stories = tot_stories / AO3_total_stories * 100)

total_AO3_stories_by <- total_AO3_stories_by_gender %>% 
  full_join(total_AO3_stories_by_race) %>% 
  relocate(gender, race)


# compare screen time and AO3 occurrence ----------------------------------

# count characters occurrences in AO3
count_chars_AO3 <- harry_char %>% 
  group_by(canonical_tag) %>% 
  summarise(n_AO3 = n()) 

# put together screen time and AO3 occurrences
full_occurrences_df <- screen_time_df %>% 
  select(-index) %>%
  full_join(count_chars_AO3) %>% 
  left_join(checked_full_chars_list, by = "canonical_tag") %>%
  arrange(desc(n_AO3), release_date)

# remove subtotals about single films
test_data <- full_occurrences_df %>%
  select(canonical_tag, gender, race, grand_tot_films, n_AO3) %>% 
  group_by(canonical_tag) %>% 
  slice_head() %>% 
  replace_na(list(grand_tot_films = 0, n_AO3 = 0)) %>%
  ungroup() %>% 
  # remove characters only appearing in the film Fantastic Beasts...
  filter(!canonical_tag %in% fantastic_beast_new_chars)

# exclude collective characters (e.g. Slytherin students) and unknown gender
test_data_fm <- test_data %>% 
  filter(gender == "F" | gender == "M")

# calculate total stories in which film characters appear
film_chars_stories <- harry_char %>% 
  # exclude characters not featured in films and Grindelwald because his popularity was  
  # boosted by the film Fantastic Beasts and we don't have screen time that for that
  filter(canonical_tag %in% test_data_fm$canonical_tag[test_data_fm$grand_tot_films != 0]) %>% 
  filter(canonical_tag != "Gellert Grindelwald")

total_film_chars_stories <- n_distinct(film_chars_stories$index)
# 121 film characters, one character is not featured in any story: Bogrod (Harry Potter)

# calculate statistics for each tag
exclude_no_screen <- test_data_fm  %>%
  filter(grand_tot_films != 0) %>% 
  filter(canonical_tag != "Gellert Grindelwald") %>% 
  # calculate statistics
  mutate(prop_screen_time = grand_tot_films / movies_total_time * 100,
         prop_AO3_occurrences = n_AO3 / total_film_chars_stories * 100,
         # I will calculate the variation as increase or decrease with respect to screen time
         diff_presence = (prop_AO3_occurrences - prop_screen_time) / prop_screen_time * 100)

# test relation between screen time and AO3 occurrence
r <- rcorr(exclude_no_screen$grand_tot_films, exclude_no_screen$n_AO3, type = "pearson")
round(r$r[2,1] ,2) # r = .82
round(sqrt(r$r[2,1]), 2) # R2 = .91

# gender gap in films and AO3 ---------------------------------------------

# calculate statistics aggregated by gender
aggr_diff_gender_ <- exclude_no_screen %>%
  mutate(tot_chars = length(canonical_tag)) %>% 
  group_by(gender) %>%
  summarise(n_gender_chars = length(canonical_tag),
            prop_gender_chars = n_gender_chars / tot_chars * 100,
            # calculate cumulative proportion of screen time and AO3 occurrences
            prop_gender_screen_time = sum(prop_screen_time),
            prop_gender_AO3_occurrences = sum(prop_AO3_occurrences),
            # use individual percentages to calculate mean proportion, instead of 
            # aggregating all screen times and AO3 occurrences and then calculate the mean
            mean_screen_time = mean(prop_screen_time),
            mean_AO3_occ = mean(prop_AO3_occurrences),
            mean_diff_presence = mean(diff_presence),
            # since there's a lot of fanfic about the main male characters, let's also look at the median
            median_screen_time = median(prop_screen_time),
            median_AO3_occ = median(prop_AO3_occurrences),
            median_diff_presence = median(diff_presence)) %>% 
  slice_head() %>% 
  ungroup() %>% 
  # calculate gender gap within medium and its variation between media
  # (% increase or decrease with respect to gender gap for screen time)
  mutate(chars_gender_gap = (prop_gender_chars - lag(prop_gender_chars)) / lag(prop_gender_chars) * 100,
         screen_time_gender_gap = (prop_gender_screen_time - lag(prop_gender_screen_time)) /
                                  lag(prop_gender_screen_time) * 100,
         AO3_occ_gender_gap = (prop_gender_AO3_occurrences - lag(prop_gender_AO3_occurrences)) /
                              lag(prop_gender_AO3_occurrences) * 100,
         film_to_AO3_gap_var = (AO3_occ_gender_gap - screen_time_gender_gap) / 
                               screen_time_gender_gap * 100) %>% 
  # calculate variation of gender gap in media representation with respect to characters starting gender gap
  mutate(chars_to_film_gap = (screen_time_gender_gap - chars_gender_gap) / chars_gender_gap * 100,
         chars_to_AO3_gap = (AO3_occ_gender_gap - chars_gender_gap) / chars_gender_gap * 100,
         chars_to_medium_gap_var = (chars_to_AO3_gap - chars_to_film_gap) / chars_to_film_gap * 100) %>%
  # check also variation of mean and median
  mutate(mean_screen_time_gap = (mean_screen_time - lag(mean_screen_time)) / lag(mean_screen_time) * 100,
         mean_AO3_occ_gap = (mean_AO3_occ - lag(mean_AO3_occ)) / lag(mean_AO3_occ) * 100,
         # mean_film_to_AO3_gap_var = (mean_AO3_occ_gap - mean_screen_time_gap) / mean_screen_time_gap * 100,
         median_screen_time_gap = (median_screen_time - lag(median_screen_time)) / lag(median_screen_time) * 100,
         median_AO3_occ_gap = (median_AO3_occ - lag(median_AO3_occ)) / lag(median_AO3_occ) * 100,
         median_film_to_AO3_gap_var = (median_AO3_occ_gap - median_screen_time_gap) / median_screen_time_gap * 100)
  # Alternative using ratio instead of % variation
  # mutate(chars_gender_gap = prop_gender_chars / lag(prop_gender_chars),
  #      screen_time_gender_gap = prop_gender_screen_time / lag(prop_gender_screen_time),
  #      AO3_occ_gender_gap = prop_gender_AO3_occurrences / lag(prop_gender_AO3_occurrences),
  #      film_to_AO3_gap_var = (AO3_occ_gender_gap - screen_time_gender_gap) / screen_time_gender_gap * 100,
  #      chars_to_film_gap = chars_gender_gap - screen_time_gender_gap,
  #      chars_to_AO3_gap = chars_gender_gap - AO3_occ_gender_gap,
  #      chars_to_medium_gap_var = (chars_to_AO3_gap - chars_to_film_gap) / chars_to_film_gap * 100,
  #      mean_screen_time_gap = mean_screen_time / lag(mean_screen_time),
  #      mean_AO3_occ_gap = mean_AO3_occ / lag(mean_AO3_occ),
  #      median_screen_time_gap = median_screen_time / lag(median_screen_time),
  #      median_AO3_occ_gap = median_AO3_occ / lag(median_AO3_occ))


# test if there is a significant difference in representation of gender ------
exclude_no_screen_long <- exclude_no_screen %>%
  rename(film = prop_screen_time, AO3 = prop_AO3_occurrences) %>% 
  pivot_longer(film:AO3, names_to = "medium", values_to = "prop_representation") %>% 
  mutate(medium = factor(medium, levels = c("film", "AO3")))

# Multilevel mixed effect model for repeated measures (lmer) with bootstrap resampling
set.seed(123)
boots <- bootstraps(exclude_no_screen_long, times = 2000, apparent = TRUE)

boot_models <- boots %>%
  mutate(model = map(splits,
                     ~lmer(prop_representation ~ medium * gender + (1 | canonical_tag), data = analysis(.))),
         coef_info = map(model, tidy))

boot_models %>%
  unnest(coef_info)

int_pctl(boot_models, coef_info)
# Significant variation in representation between films and AO3 (b = 1.48 [CI 0.48,2.70]),
# but NO significant effect of gender*medium on it (b = 0.401 [CI -1.31, 2.17])

# maybe outliers are hiding some effect
Q <- quantile(exclude_no_screen$prop_screen_time, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(exclude_no_screen$prop_screen_time)
up <- Q[2]+1.5*iqr # Upper Range
low <- Q[1]-1.5*iqr # Lower Range
no_outliers1 <- exclude_no_screen %>% filter(between(prop_screen_time, low, up))
Q <- quantile(exclude_no_screen$prop_AO3_occurrences, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(exclude_no_screen$prop_AO3_occurrences)
up <- Q[2]+1.5*iqr # Upper Range
low <- Q[1]-1.5*iqr # Lower Range
no_outliers1 <- no_outliers1 %>% filter(between(prop_AO3_occurrences, low, up))

no_outliers1_log <- no_outliers1 %>%
  mutate(across(c(prop_screen_time, prop_AO3_occurrences), .fns = ~log1p(.x)))

no_outliers1_long <- no_outliers1 %>%
  rename(film = prop_screen_time, AO3 = prop_AO3_occurrences) %>% 
  pivot_longer(film:AO3, names_to = "medium", values_to = "prop_representation") %>% 
  mutate(medium = factor(medium, levels = c("film", "AO3")),
         gender = factor(gender, levels = c("M", "F")))

set.seed(123)
boots <- bootstraps(no_outliers1_long, times = 2000, apparent = TRUE)
boot_model1 <- boots %>%
  mutate(model = map(splits,
                     ~lmer(prop_representation ~ medium + (1 | canonical_tag), data = analysis(.))),
         coef_info = map(model, tidy))
boot_model1 %>%
  unnest(coef_info)
CI_model1 <- int_pctl(boot_model1, coef_info)

boot_model2 <- boots %>%
  mutate(model = map(splits,
                     ~lmer(prop_representation ~ medium * gender + (1 | canonical_tag), data = analysis(.))),
         coef_info = map(model, tidy))
boot_model2 %>%
  unnest(coef_info)
CI_model2 <- int_pctl(boot_model2, coef_info)

anova(boot_model1$model, boot_model2$model)
# confirmed, NO significant effect of gender*medium (-0.0209  [CI -0.372, 0.417])

# double-check removing outliers based on diff_presence
Q <- quantile(exclude_no_screen$diff_presence, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(exclude_no_screen$diff_presence)
up <- Q[2]+1.5*iqr # Upper Range
low <- Q[1]-1.5*iqr # Lower Range
no_outliers_diff <- exclude_no_screen %>% filter(between(diff_presence, low, up))
no_outliers_diff_long <- no_outliers_diff %>%
  rename(film = prop_screen_time, AO3 = prop_AO3_occurrences) %>% 
  pivot_longer(film:AO3, names_to = "medium", values_to = "prop_representation") %>% 
  mutate(medium = factor(medium, levels = c("film", "AO3")))

set.seed(123)
boots <- bootstraps(no_outliers_diff_long, times = 2000, apparent = TRUE)

boot_models <- boots %>%
  mutate(model = map(splits,
                     ~lmer(prop_representation ~ medium * gender + (1 | canonical_tag), data = analysis(.))),
         coef_info = map(model, tidy))

boot_models %>%
  unnest(coef_info)

int_pctl(boot_models, coef_info)

# calculate aggregated statistics for no_outliers
aggr_diff_gender_no_outliers <- no_outliers1 %>%
  mutate(tot_chars = length(canonical_tag)) %>% 
  group_by(gender) %>%
  summarise(n_gender_chars = length(canonical_tag),
            prop_gender_chars = n_gender_chars / tot_chars * 100,
            # calculate cumulative proportion of screen time and AO3 occurrences
            prop_gender_screen_time = sum(prop_screen_time),
            prop_gender_AO3_occurrences = sum(prop_AO3_occurrences),
            # use individual percentages to calculate mean proportion, instead of 
            # aggregating all screen times and AO3 occurrences and then calculate the mean
            mean_screen_time = mean(prop_screen_time),
            mean_AO3_occ = mean(prop_AO3_occurrences),
            mean_diff_presence = mean(diff_presence),  
            # since there's a lot of fanfic about the main male characters, let's also look at the median
            median_screen_time = median(prop_screen_time),
            median_AO3_occ = median(prop_AO3_occurrences),
            median_diff_presence = median(diff_presence)) %>% # !!! this is increase for F and decrease for M
  slice_head() %>% 
  ungroup() %>% 
  # calculate gender gap within medium and its variation between media
  # (% increase or decrease with respect to gender gap for screen time)
  mutate(chars_gender_gap = (prop_gender_chars - lag(prop_gender_chars)) / lag(prop_gender_chars) * 100,
         screen_time_gender_gap = (prop_gender_screen_time - lag(prop_gender_screen_time)) /
           lag(prop_gender_screen_time) * 100,
         AO3_occ_gender_gap = (prop_gender_AO3_occurrences - lag(prop_gender_AO3_occurrences)) /
           lag(prop_gender_AO3_occurrences) * 100,
         film_to_AO3_gap_var = (AO3_occ_gender_gap - screen_time_gender_gap) / 
           screen_time_gender_gap * 100) %>% 
  # calculate variation of gender gap in media representation with respect to characters starting gender gap
  mutate(chars_to_film_gap = (screen_time_gender_gap - chars_gender_gap) / chars_gender_gap * 100,
         chars_to_AO3_gap = (AO3_occ_gender_gap - chars_gender_gap) / chars_gender_gap * 100,
         chars_to_medium_gap_var = (chars_to_AO3_gap - chars_to_film_gap) / chars_to_film_gap * 100) %>%
  # check also variation of mean and median
  mutate(mean_screen_time_gap = (mean_screen_time - lag(mean_screen_time)) / lag(mean_screen_time) * 100,
         mean_AO3_occ_gap = (mean_AO3_occ - lag(mean_AO3_occ)) / lag(mean_AO3_occ) * 100,
         # mean_film_to_AO3_gap_var = (mean_AO3_occ_gap - mean_screen_time_gap) / mean_screen_time_gap * 100,
         median_screen_time_gap = (median_screen_time - lag(median_screen_time)) / lag(median_screen_time) * 100,
         median_AO3_occ_gap = (median_AO3_occ - lag(median_AO3_occ)) / lag(median_AO3_occ) * 100,
         median_film_to_AO3_gap_var = (median_AO3_occ_gap - median_screen_time_gap) / median_screen_time_gap * 100)

# let's plot the differences ---------------------------------------------

no_outliers1 %>%
  ggplot(aes(x = gender, y = diff_presence, color = gender)) +
  geom_boxplot(show.legend = FALSE) +
  # geom_point(position = position_jitter(0.2), alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .5, alpha = .8) +
  geom_hline(yintercept = 0, alpha = 0.7, linetype="dashed") +
  scale_color_manual(values = c("#58A051", "#18679A")) +
  scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
  theme_minimal() +
  geom_text(aes(label = paste(canonical_tag, round(diff_presence,0), "%", sep = " ")), size = 3,
            hjust = 0,  nudge_x = 0.02, check_overlap = TRUE , show.legend = FALSE) +
  labs(x = "Gender", y = "Variation between screen time and AO3 stories occurrences") +
  ggsave("plots/3_AO3_film_gender_boxplot.png", width = 9, height = 8)

exclude_no_screen %>%
  ggplot(aes(x = prop_screen_time, y = prop_AO3_occurrences, color = gender)) +
  geom_point() +
  #geom_line(stat="smooth",method = "lm", se = FALSE, fullrange=TRUE, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.7, linetype="dashed") +
  scale_color_manual(values = c("#58A051", "#18679A")) +
  # scale_x_continuous(labels = label_percent(accuracy = 0.1, scale = 1), limits = c(0,1.6)) +
  # scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1), limits = c(0,5.2)) +
  scale_x_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
  theme_minimal() +
  geom_text(aes(label = paste(canonical_tag, round(diff_presence,0), "%", sep = " ")), size = 2.5, vjust = 1,
            nudge_y = -0.05, check_overlap = TRUE , show.legend = FALSE) +
  labs(x = "Screen time", y = "AO3 stories", color = "Gender") +
  ggsave("plots/3_AO3_film_gender_full.png", width = 9, height = 8)
