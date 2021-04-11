library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer)
library(plotly)

# original data are in "../data/Full_HP_AO3_tags.csv". We use here the tidy version (saved as RData object to save space)
# created with the script tidy_up_corpus.R. In the script tidy_up_corpus.R the function read_and_clean() also exclude stories 
# with less than 10 words, and stories published in AO3 in the year 2020
load("/Users/fedor/GitHub/cumulative_AO3/data/Harry_tidy.RData")

# check languages - 90% is English
# harry %>%
#   group_by(language) %>%
#   summarise(n = n_distinct(index)) %>%
#   mutate(prop = prop.table(n)) %>% 
#   arrange(desc(n))

# we exclude stories that are not in English, obtaining a total of N=196726 stories
# harry <- harry %>% 
#  filter(language == "English")

# Backdated stories --------------------------------------------------------------

# column "year_fix" will assign to backdated stories the backdated year, while not backdated ones will keep the year assigned
# based on "link". This is useful for queries on themes within the HP fandom beyond AO3, since stories were published outside
# AO3 before being imported and having articulated tags added to them.
# New reliable date range starting from 2000
harry <- harry %>%
  mutate(year_fix = year(date)) %>% 
  mutate(backdated = ifelse(date_year >= (year_fix + 1), TRUE, FALSE)) %>% 
  mutate(year_fix = ifelse(backdated==FALSE, date_year, year_fix))

# Aggregate synonyms ------------------------------------------------------
# remember this is a quite conservative estimate of accumulation, if we do not consider synonyms it would be higher.

# load the synonyms files
characters_syn <- read_csv("/Users/fedor/GitHub/cumulative_AO3/data/characters_synonyms.csv")
relationships_syn <- read_csv("/Users/fedor/GitHub/cumulative_AO3/data/relationships_synonyms.csv")
freeforms_syn <- read_csv("/Users/fedor/GitHub/cumulative_AO3/data/freeforms_synonyms.csv")
common_freeform_syn <- read_csv("/Users/fedor/GitHub/cumulative_AO3/data/common_freeforms_synonyms.csv")

merge_freeforms_syn <- bind_rows(freeforms_syn, common_freeform_syn)


# transform the synonyms files in a tidy format
char_dictionary <- characters_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)
ship_dictionary <- relationships_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)
free_dictionary <- merge_freeforms_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)


# aggregate synonyms
harry_char <- harry %>%
  filter(tag_type=="characters") %>%
  left_join(char_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))

harry_ships <- harry %>%
  filter(tag_type=="relationships") %>%
  left_join(ship_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))

harry_free <- harry %>%
  filter(tag_type=="freeforms") %>%
  left_join(free_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))


# build dataset including aggregated synonyms and exclude crossover stories
harry_aggr <- bind_rows(harry_char, harry_ships, harry_free) 

harry_full <- harry %>% 
  left_join(harry_aggr) %>% 
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag)) %>% 
  group_by(index) %>% 
  filter(!any(canonical_tag == "Crossover")) %>% 
  ungroup()


# Accumulation analysis ---------------------------------------------------

# harry_full <- harry_full %>%
#   filter(between(year_fix, 2002, 2019)) # start from 2002 because 2000 and 2001 have only 21 and 55 stories

# total number of stories by year:
harry_full %>%
  group_by(year_fix) %>%
  summarise(count = n_distinct(index)) %>%
  ggplot(aes(x = year_fix, y = count)) +
  geom_col() +
  scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x = "Year", y = "Number of stories") +
  theme_minimal() +
  ggsave("plots/1_1_number_of_stories.pdf", width = 5, height = 5)

# total number of different tags by year
harry_full %>% 
  group_by(year_fix, tag_type) %>%
  summarise(count = n_distinct(canonical_tag)) %>%
  ggplot(aes(x = year_fix, y = count)) +
  geom_point() +
  geom_line() +
  scale_color_jco() +
  scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
  facet_wrap(vars(tag_type), scales = "free") +
  labs(x = "Year", y = "Count") +
  theme_minimal() +
  ggsave("plots/1_2_accumulation_totals.pdf", width = 12, height = 5)


# Diversity analysis ------------------------------------------------------

total_stories <- n_distinct(harry_full$index)

# count queer pairings
count_sex <- harry_full %>% 
  filter(tag_type == "category") %>% 
  filter(canonical_tag == "M/M" | canonical_tag == "F/F" | canonical_tag == "Multi" | canonical_tag == "Other") %>%
  distinct(index, .keep_all = TRUE) %>% 
  count(canonical_tag) %>%
  mutate(cumul_n = cumsum(n),
         prop = n / total_stories * 100,
         cumul_prop = cumsum(prop))

# load list of characters with assigned race and gender
characters_race <- read_csv("data/HP/checked_full_chars_list.csv") %>% 
  select(-character) %>% 
  distinct(canonical_tag, .keep_all = TRUE)

harry_full <- harry_full %>% 
  left_join(characters_race)

# count stories with at least on person of color
count_race <- harry_full %>% 
  filter(tag_type == "characters") %>% 
  filter(race == "black" | race == "asian") %>% 
  summarise(n = n_distinct(index),
            prop = n / total_stories * 100)

# count stories featuring only people of color
count_color_only <- harry_full %>% 
  filter(tag_type == "characters") %>% 
  group_by(index) %>% 
  mutate(color_only = ifelse(any(race == "white" | race == "various" | race == "beast" | race == "other"), FALSE, TRUE)) %>% 
  filter(color_only == TRUE) %>% 
  ungroup() %>% 
  summarise(n = n_distinct(index),
            prop = n / total_stories * 100)


# SPECIFIC TAGS ANALYSIS  -------------------------------------------------

# For each character, or group of characters, we can show that the number of freeforms and relationships tags increase, ans there is 
# more diversity in stories. Here an example for Harry and freeforms tags.

# Harry:
tagOf_harry <- read_csv("/Users/fedor/GitHub/cumulative_AO3/data/tagOf_harry.csv", quote="")

tagOf_harry <- tagOf_harry %>%
  mutate(tagOf = "Harry Potter") %>%
  filter(tagOf_first_row != "Harry Potter") %>%
  rename(isTaggedAs = tagOf_first_row)

harry_aggr_tagOf <- harry_full %>%
  filter(tag %in% tagOf_harry$isTaggedAs)

freeform_groups <- read_csv("/Users/fedor/GitHub/cumulative_AO3/data/freeform_groups_harry.csv")

syn_count_tagOf <- harry_aggr_tagOf %>%
  left_join(freeform_groups, by = "canonical_tag") %>%
  group_by(year_fix) %>%
  mutate(n_stories_y = n_distinct(index),
         canonical_tag = ifelse(canonical_tag == "Dark Harry", "Dark Harry Potter", canonical_tag)) %>%
  count(year_fix, tag, tag_type, canonical_tag, freeform_group, n_stories_y, n_syn) %>%
  group_by(year_fix, canonical_tag, freeform_group, n_stories_y) %>%
  summarise(tot = sum(n))

# generate a colour palette for each category
n_colors <- length(unique(syn_count_tagOf$canonical_tag[syn_count_tagOf$freeform_group == "Ethics"]))
n_colors2 <- length(unique(syn_count_tagOf$canonical_tag[syn_count_tagOf$freeform_group == "Family"]))
n_colors3 <- length(unique(syn_count_tagOf$canonical_tag[syn_count_tagOf$freeform_group == "Job"]))
n_colors4 <- length(unique(syn_count_tagOf$canonical_tag[syn_count_tagOf$freeform_group == "Other"]))
n_colors5 <- length(unique(syn_count_tagOf$canonical_tag[syn_count_tagOf$freeform_group == "Relationship"]))
n_colors6 <- length(unique(syn_count_tagOf$canonical_tag[syn_count_tagOf$freeform_group == "Skills"]))
mypal <- colorRampPalette(brewer.pal(7, "Blues")[1:7])(n_colors)[order(sample(1:n_colors, n_colors))] # for legibility, shuffle palette order
mypal2 <- brewer.pal(9, "Oranges")[c(4,9,5,7)]
mypal3 <- brewer.pal(9, "Greys")[c(4,9,6)]
mypal4 <- brewer.pal(9, "YlOrRd")[c(2,3)]
mypal5 <- colorRampPalette(brewer.pal(7, "Greens")[1:7])(n_colors5)[order(sample(1:n_colors5, n_colors5))]
mypal6 <- colorRampPalette(brewer.pal(7, "Purples")[1:7])(n_colors6)[order(sample(1:n_colors6, n_colors6))]

release_dates <- c("Chamber of Secrets (m) - Nov 2002", "Order of the Phoenix (b) - Jul 2003",
                   "Prisoner of Azkaban (m) - Nov 2004", "Half Blood Prince (b) - Jul 2005\nGoblet of Fire (m) - Nov 2005",
                   "2006", "Order of the Phoenix (m) - Jul 2007\nDeathly Hallows (b) - Jul 2007",
                   "2008", "Half Blood Prince (m) - Jul 2009", "Deathly Hallows pt1 (m) - Nov 2010",
                   "Deathly Hallows pt2 (m) - Nov 2011", "2012", "2013","2014","2015",
                   "Fantastic Beasts (m) -  Nov 2016", "2017", "Grindelwald (m) - Nov 2018", "2019")

syn_count_tagOf %>%
  ggplot(aes(x = year_fix, y = tot, fill = interaction(as.factor(canonical_tag), freeform_group, sep = " — "))) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c(mypal, mypal2, mypal3, mypal4, mypal5, mypal6),
                    name = "Tag — group") +
  labs(x = "Canon release dates", y = "Proportion of stories") +
  scale_x_reverse(breaks= c(2002:2019), labels = release_dates) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 9)) +
  ggsave("plots/1_accumulation_relationships_growth.png", width = 14, height = 8)

#wrong!!!
fig <- plot_ly(syn_count_tagOf, x = ~year_fix, y = ~tot, type = "bar",
               marker = list(color = c(mypal, mypal2, mypal3, mypal4, mypal5, mypal6)),
               color = interaction(as.factor(syn_count_tagOf$canonical_tag), syn_count_tagOf$freeform_group, sep = " — ")) %>% 
  layout(yaxis = list(title = "Proportion of stories"), barmode = "stack", barnorm = "fraction")


## -----------------------------------------------------------------------

# Calculate number of tags for each story

harry_full <- harry_full %>%
  group_by(index, hits, kudos, bookmarks, year_fix) %>% # remember to always group by index and not by title (which can be the same for different stories)
  summarise(characters_n = n_distinct(canonical_tag[tag_type == "characters"]), # get the number of canonical "character" tags for the story
            relationships_n = n_distinct(canonical_tag[tag_type == "relationships"]),
            freeforms_n = n_distinct(canonical_tag[tag_type == "freeforms"]))

# Optional: check how many stories don't use tags for each category -- for some years it can reach 30-50%

# harry_aggr %>%
#   group_by(year_fix) %>% 
#   summarise(zero_char = n_distinct(index[characters_n == 0]), prop_char = zero_char / n_distinct(index),
#             zero_ship = n_distinct(index[relationships_n == 0]), prop_ship = zero_ship / n_distinct(index),
#             zero_free = n_distinct(index[freeforms_n == 0]), prop_free = zero_free / n_distinct(index),) 


# Calculate percentiles

harry_full <- harry_full %>%  
  group_by(year_fix) %>% # calculate per year to be sure to have stories from all years for each percentile
  mutate(hits_p = ntile(hits, 100)) %>%
  mutate(kudos_p = ntile(kudos, 100)) %>%
  mutate(bookmarks_p = ntile(bookmarks, 100))

# Optional: check the number of NAs per year for each metric -- Number of NAs is 1.7~10% for hits, 1~19% for kudos, 16~54% for bookmarks

# check_na <- harry %>%
#   group_by(year_fix) %>%
#   distinct(index, .keep_all = TRUE) %>%
#   summarise(unique_stories = n(),
#             NA_hits = sum(is.na(hits)), prop_NA_hits = NA_hits / unique_stories,
#             NA_kudos = sum(is.na(kudos)), prop_NA_kudos = NA_kudos / unique_stories,
#             NA_bookmarks = sum(is.na(bookmarks)), prop_NA_bookmarks = NA_bookmarks / unique_stories)


## Count unique tags per percentile interval
# Specify the metric to be used in group_by (e.g. here kudos) and percentiles intervals of same length for all clusters (e.g. here 10)

# KUDOS

#

lower_percentile <- 1 
higher_percentile <- 10

harry_low <- harry_full %>%
  filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
  group_by(year_fix, kudos_p) %>%
  mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
         n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
         n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
  group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
  summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
            min(kudos), max(kudos)) %>%
  mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
         Freeforms = free_tags_count/n_stories_y_p_free) %>%
  pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")

#
lower_percentile <- 46
higher_percentile <- 55

harry_mid <- harry_full %>%
  filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
  group_by(year_fix, kudos_p) %>%
  mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
         n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
         n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
  group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
  summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
            min(kudos), max(kudos)) %>%
  mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
         Freeforms = free_tags_count/n_stories_y_p_free) %>%
  pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")

#
lower_percentile <- 91
higher_percentile <- 100

harry_high <- harry_full %>%
  filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
  group_by(year_fix, kudos_p) %>%
  mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
         n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
         n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
  group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
  summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
            min(kudos), max(kudos)) %>%
  mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
         Freeforms = free_tags_count/n_stories_y_p_free) %>%
  pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")

# PROPER STAT MODEL HERE:
# Compare linear and exponential fitting.
cumulative <- bind_rows(harry_high, harry_mid, harry_low) %>%
  add_column(Rank = as_factor(rep(c("High kudos (91-100th percentile)", "Mid kudos (46-55th percentile)", "Low kudos (1-10th percentile)"), each = 540)))

# replace below High/Mid/Low kudos:
trend_to_fit <- cumulative %>%
  filter(Rank == "Low kudos (1-10th percentile)", tag_type == "Freeforms") 
y <- trend_to_fit$count
x <- trend_to_fit$year_fix
AIC(lm(y ~ x))
AIC(lm(y ~ x + I(x^2)))

trend_to_fit <- cumulative %>%
  filter(Rank == "Low kudos (1-10th percentile)", tag_type == "Characters") 
y <- trend_to_fit$count
x <- trend_to_fit$year_fix
AIC(lm(y ~ x))
AIC(lm(y ~ x + I(x^2)))


trend_to_fit <- cumulative %>%
  filter(Rank == "Low kudos (1-10th percentile)", tag_type == "Relationships") 
y <- trend_to_fit$count
x <- trend_to_fit$year_fix
AIC(lm(y ~ x))
AIC(lm(y ~ x + I(x^2)))

# compare slopes of percentiles
cumulative %>%
  ggplot(aes(x = year_fix, y = count, colour = Rank)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + # according to the test, it is better an exponential fit.
  scale_color_jco() +
  scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
  facet_wrap(vars(tag_type), scales = "free") +
  labs(x = "Year", y = "Average count") +
  theme_bw() +
  ggsave("plots/1_4_accumulation_percentiles.pdf", width = 15, height = 5)

