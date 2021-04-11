library(tidyverse)
library(jsonlite)
library(scales)

# Goodreads
# dataset downloaded from https://sites.google.com/eng.ucsd.edu/ucsdbookgraph/books?authuser=0#h.p_1epVjtoBCBGF
goodreads_books <- stream_in(file("../data/goodreads_book_works.json"))

goodreads_tibble <- as_tibble(goodreads_books)

goodreads_count <- goodreads_tibble %>% 
  select(best_book_id, original_publication_year, original_title, ratings_count) %>%
  mutate(best_book_id = as.double(best_book_id),
         original_publication_year = as.double(original_publication_year),
         decade = (original_publication_year - original_publication_year %% 10),
         fifty = (original_publication_year - original_publication_year %% 50),
         century = (original_publication_year - original_publication_year %% 100),
         ratings_count = as.double(ratings_count)) %>% 
  arrange(desc(ratings_count)) %>% 
  rowid_to_column() %>% 
  group_by(century) %>%
  mutate(grouped_rowid = 1:n())

top3_goodreads <- goodreads_count %>% 
  filter(between(century, 1500,2010)) %>%
  group_by(century) %>%
  filter(grouped_rowid <= 3)

top100_goodreads <- goodreads_count %>% 
  ungroup() %>%
  select(-c(decade, fifty, grouped_rowid)) %>%
  slice_head(n = 100) %>% 
  mutate(original_title = ifelse(best_book_id == 227443, "Bridget Jones's Diary", original_title))

top100_goodreads %>% 
  write_csv("data/top100_goodreads.csv")

table_top20_goodreads <- top20_goodreads %>% 
  select(-c(decade, fifty)) %>% 
  rename(century_rank = grouped_rowid)

top20_centuries_goodreads <- goodreads_count %>% 
  filter(between(century, 1500,2010)) %>%
  group_by(century) %>%
  filter(grouped_rowid <= 20) %>% 
  arrange(century)

# fill missing titles by manually checking Goodreads
missing_titles <- tibble(best_book_id = c(1923820, 13006, 24096, 72978),
                         original_title = c("Holy Bible: King James Version",
                                            "Julius Caesar",
                                            "Leonardo's Notebooks",
                                            "Titus Andronicus"))

table_top20_centuries_goodreads <- top20_centuries_goodreads %>% 
  select(-c(decade, fifty)) %>% 
  rename(century_rank = grouped_rowid)

temp <- table_top20_centuries_goodreads %>% 
  filter(best_book_id %in% missing_titles$best_book_id) %>%
  select(-original_title) %>% 
  left_join(missing_titles)

table_top20_centuries_goodreads <- table_top20_centuries_goodreads %>% 
  filter(!best_book_id %in% temp$best_book_id) %>% 
  bind_rows(temp) %>% 
  arrange(desc(ratings_count))

table_top20_centuries_goodreads %>% 
  write_csv("data/top20_centuries_goodreads.csv")
  
summary_count <- goodreads_count %>% 
  filter(between(century, 1500,2010)) %>%
  group_by(century) %>% 
  summarise(tot_books = n())

centuries_labels <- c("1500" = "16th century", "1600" = "17th century", "1700" = "18th century",
                      "1800" = "19th century", "1900" = "20th century", "2000" = "21st century")

goodreads_count %>% 
  filter(between(century, 1500,2010)) %>% 
  slice_head(n = 5000) %>% 
  ggplot(aes(x = grouped_rowid, y = ratings_count)) +
  geom_point(size = .5) +
  # geom_text(data = top3, aes(label = original_title), size = 3, hjust = "left", nudge_x = 0.5) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(vars(century), scales = "free", labeller = as_labeller(centuries_labels)) +
  theme_minimal() +
  theme(strip.text.x = element_text(face = "bold.italic"),
        strip.text.y = element_text(face = "bold.italic")) +
  labs(x = "Books ordered by popularity", y = "Number of ratings") +
  ggsave("plots/5_Goodreads_popularity.png", width = 12, height = 8)


# libraries of Roma (Italy)

# load files and create dataframe
filenames_roma <- list.files(path = "data/roma")

final_df_roma <- data.frame()

for(i in filenames_roma){
  filepath <- file.path("data/roma", i)
  prob_df <- read_csv2(filepath)
  final_df_roma <- rbind(final_df_roma, prob_df)
}


filenames_roma_tab <- list.files(path = "data/roma/tab")

final_df_roma_tab <- data.frame()

for(i in filenames_roma_tab){
  filepath <- file.path("data/roma/tab", i)
  prob_df <- read_tsv(filepath)
  final_df_roma_tab <- rbind(final_df_roma_tab, prob_df)
}


roma_df <- rbind(final_df_roma, final_df_roma_tab) %>% 
  filter(`Cod. tipo materiale` == "M")

roma_count <- roma_df %>% 
  select(`Identificativo Titolo`, Titolo, `Genere 1`) %>%
  add_count(`Identificativo Titolo`, sort = TRUE) %>%
  distinct(`Identificativo Titolo`, .keep_all = TRUE) %>% 
  filter(`Identificativo Titolo` != "182464") %>% # remove Topolino because it's a collection
  filter(`Identificativo Titolo` != "265272") %>% # remove Paperino
  rowid_to_column()

roma_count %>% 
  slice_head(n = 5000) %>%
  ggplot(aes(x = rowid, y = n)) +
  geom_point(size = .5) +
  theme_minimal() +
  labs(x = "Books ordered by popularity", y = "Number of times borrowed") +
  ggsave("plots/5_Roma_popularity.png", width = 12, height = 8)

#repeat divided by decade of publication 
roma_count_pub_decades <- roma_df %>% 
  select(`Identificativo Titolo`, Titolo, `Anno di pubblicazione`) %>%
  mutate(pub_date = as.double(`Anno di pubblicazione`),
         pub_decade = (pub_date - pub_date %% 10)) %>% 
  filter(`Identificativo Titolo` != "182464") %>% # remove Topolino because it's a collection
  filter(`Identificativo Titolo` != "265272") %>% # remove Paperino
  filter(`Identificativo Titolo` != "241650") %>% # remove Internazionale magazine
  add_count(`Identificativo Titolo`, sort = TRUE) %>%
  distinct(`Identificativo Titolo`, .keep_all = TRUE) %>% 
  rowid_to_column() %>% 
  group_by(pub_decade) %>%
  mutate(grouped_rowid = 1:n())

top3_pub <- roma_count_pub_decades %>% 
  filter(between(pub_decade, 1910, 2010)) %>%
  filter(grouped_rowid <= 3)

summary_count_roma_pub <- roma_count_pub_decades %>% 
  filter(between(pub_decade, 1910, 2010)) %>%
  group_by(pub_decade) %>% 
  summarise(tot_books = n())

roma_count_pub_decades %>% 
  filter(between(pub_decade, 1910, 2010)) %>%
  slice_head(n = 5000) %>% 
  ggplot(aes(x = grouped_rowid, y = n)) +
  geom_point(size = .5) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  facet_wrap(vars(pub_decade), scales = "free") +
  theme_minimal() +
  theme(strip.text.x = element_text(face = "bold.italic"),
        strip.text.y = element_text(face = "bold.italic")) +
  labs(x = "Books ordered by popularity", y = "Number of times borrowed")

#repeat divided by age group
roma_count_age_decades <- roma_df %>% 
  select(`Identificativo Titolo`, Titolo, Natura, `Genere 1`, `Eta' utente-movimento`) %>%
  mutate(age = as.double(`Eta' utente-movimento`),
         age_decade = (age - age %% 10)) %>% 
  filter(Natura != "Periodico") %>%
  group_by(age_decade) %>%
  add_count(`Identificativo Titolo`, sort = TRUE) %>%
  distinct(`Identificativo Titolo`, .keep_all = TRUE) %>% 
  select(-c(`Eta' utente-movimento`, age)) %>% 
  mutate(grouped_rowid = 1:n())

top3_roma <- roma_count_age_decades %>% 
  filter(between(age_decade, 10, 90)) %>%
  group_by(age_decade) %>%
  filter(grouped_rowid <= 3)

top20_roma <- roma_count_age_decades %>% 
  filter(between(age_decade, 10, 90)) %>%
  group_by(age_decade) %>%
  filter(grouped_rowid <= 20) %>% 
  arrange(age_decade)

top20_roma %>% 
  write_csv("data/top20_roma.csv")

summary_count_roma <- roma_count_age_decades %>% 
  filter(between(age_decade, 10, 90)) %>%
  group_by(age_decade) %>% 
  summarise(tot_books = n())

age_labels <- c("10" = "10-19 years old", "20" = "20-29 years old", "30" = "30-39 years old",
                "40" = "40-49 years old", "50" = "50-59 years old", "60" = "60-69 years old",
                "70" = "70-79 years old", "80" = "80-89 years old", "90" = "90-99 years old")

roma_count_age_decades %>% 
  filter(between(age_decade, 10, 90)) %>%
  slice_head(n = 5000) %>% 
  ggplot(aes(x = grouped_rowid, y = n)) +
  geom_point(size = .5) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  facet_wrap(vars(age_decade), scales = "free", labeller = as_labeller(age_labels)) +
  theme_minimal() +
  theme(strip.text.x = element_text(face = "bold.italic"),
        strip.text.y = element_text(face = "bold.italic")) +
  labs(x = "Books ordered by popularity", y = "Number of times borrowed") +
  ggsave("plots/5_Roma_popularity_age.png", width = 12, height = 8)


roma_df %>% 
  count(`Tipo utente movimento`, sort = TRUE)
