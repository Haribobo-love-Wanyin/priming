library(tidyverse)
install.packages("readxl")
library(readxl)

en_priming <- read_csv("en_answered_prime_summary_no2.5.csv")
es_priming <- read_csv("es_answered_prime_summary_no2.5.csv")
translation <- read_excel("es_translated_final.xlsx", sheet ="es_translate")%>%
  mutate(concept_id = row_number()) %>%
  relocate(concept_id, .before = everything())

select_en_priming <- en_priming %>%
  transmute(
    en_target = target_word_unique,
    en_avgRT_prime = avgRT_prime,
    en_avgZ_prime = avgZ_prime,
    en_status = case_when(
      en_avgRT_prime >= 40 ~ "strong",
      en_avgRT_prime >= 15 ~ "weak",
      TRUE ~ "no"
    )
  )


select_es_priming <- es_priming %>%
  transmute(
    es_target = target_word_unique,
    es_avgRT_prime = avgRT_prime,
    es_avgZ_prime = avgZ_prime,
    es_status = case_when(
      es_avgRT_prime >= 40 ~ "strong",
      es_avgRT_prime >= 15 ~ "weak",
      TRUE ~ "no"
    )
  )

matched_items <- translation %>%
  left_join(select_en_priming, by = "en_target") %>%
  left_join(select_es_priming, by = "es_target") %>%
  mutate(
    condition = paste0("English_", en_status, "_Spanish_", es_status)
  )

matched_items %>%
  group_by(condition) %>%
  summarise(count = n()) %>%
  print(n = Inf)

select_condtions <- matched_items %>%
  filter(condition %in% c("English_strong_Spanish_strong", 
  "English_no_Spanish_no", 
  "English_strong_Spanish_no", 
  "English_no_Spanish_strong"
  )) %>%
  relocate(concept_id, en_target, es_target, condition, en_avgRT_prime, es_avgRT_prime)

select_condtions %>%
  group_by(condition) %>%
  summarise(
    count = n(),
    en_mean = mean(en_avgRT_prime, na.rm = TRUE),
    en_sd = sd(en_avgRT_prime, na.rm = TRUE),
    en_min = min(en_avgRT_prime, na.rm = TRUE),
    en_max = max(en_avgRT_prime, na.rm = TRUE),
    es_mean = mean(es_avgRT_prime, na.rm = TRUE),
    es_sd = sd(es_avgRT_prime, na.rm = TRUE),
    es_min = min(es_avgRT_prime, na.rm = TRUE),
    es_max = max(es_avgRT_prime, na.rm = TRUE),
    .groups = "drop"
  )

select_condtions %>%
  group_by(condition) %>%
  summarise(
    count = n())

#English priming effect close to 0 or negative
#Spanish priming effect as large as possible
#the bigger the difference between Spanish and English priming effect, the better
English_no_Spanish_strong <- select_condtions %>%
  filter(condition == "English_no_Spanish_strong") %>%
  mutate(
    difference_score = es_avgRT_prime - en_avgRT_prime
  ) %>%
  arrange(desc(difference_score)) %>%
  slice_head(n = 56) %>%
  select(-difference_score)

#both effects to be as close to zero as possible.
English_no_Spanish_no <- select_condtions %>%
  filter(condition == "English_no_Spanish_no") %>%
  mutate(
    closeness_to_zero = abs(en_avgRT_prime) + abs(es_avgRT_prime)
  ) %>%
  arrange(closeness_to_zero) %>%
  slice_head(n = 56) %>%
  select(-closeness_to_zero)

English_strong_Spanish_no <- select_condtions %>%
  filter(condition == "English_strong_Spanish_no")

English_strong_Spanish_strong <- select_condtions %>%
  filter(condition == "English_strong_Spanish_strong") %>%
  mutate(combined_strength = en_avgRT_prime + es_avgRT_prime
  ) %>%
  arrange(desc(combined_strength)) %>%
  slice_head(n = 56) %>%
  select(-combined_strength)



full_selected_items <- bind_rows(
  English_no_Spanish_strong,
  English_no_Spanish_no,
  English_strong_Spanish_no,
  English_strong_Spanish_strong
)

full_selected_items %>%
  group_by(condition) %>%
  summarise(count = n()) %>%
  print(n = Inf)


full_selected_items %>%
  group_by(condition) %>%
  summarise(
    count = n(),
    en_mean = mean(en_avgRT_prime, na.rm = TRUE),
    en_sd = sd(en_avgRT_prime, na.rm = TRUE),
    es_mean = mean(es_avgRT_prime, na.rm = TRUE),
    es_sd = sd(es_avgRT_prime, na.rm = TRUE),
    .groups = "drop"
  )



write_csv(full_selected_items, "selected_stimuli.csv")
