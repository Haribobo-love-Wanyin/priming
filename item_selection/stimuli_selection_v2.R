library(tidyverse)
library(readxl)

#english data
en_priming <- read_csv("en_answered_prime_summary_no2.5.csv")
#spanish
es_priming <- read_csv("es_answered_prime_summary_no2.5.csv")

translation <- read_excel("es_translated_final.xlsx", sheet ="es_translate")%>%
  mutate(concept_id = row_number()) %>%
  relocate(concept_id, .before = everything())

# ---- Reliability of each item's priming effect ----------------------------
# avgRT_prime = avgRT_unrelated - avgRT_related (difference of two means)
#   SE_prime = sqrt(seRT_related^2 + seRT_unrelated^2)
#   z        = avgRT_prime / SE_prime
#   95% CI   = avgRT_prime +/- 1.96 * SE_prime
add_reliability <- function(df, z_crit = 1.96) {
  df %>%
    mutate(
      se_prime = sqrt(seRT_related^2 + seRT_unrelated^2),
      z_prime  = avgRT_prime / se_prime,
      ci_low   = avgRT_prime - z_crit * se_prime,
      ci_high  = avgRT_prime + z_crit * se_prime,
      reliability = case_when(
        ci_low  > 0 ~ "reliable_facilitation",
        ci_high < 0 ~ "reliable_inhibition",
        TRUE        ~ "null_or_noise"
      ),
      status = case_when(
        reliability == "reliable_facilitation" & avgRT_prime >= 40 ~ "strong",
        reliability == "reliable_facilitation" & avgRT_prime >= 18 ~ "weak",
        reliability == "reliable_inhibition"                       ~ "inhibitory",
        abs(avgRT_prime) < 18 & reliability == "null_or_noise"     ~ "no",
        TRUE                                                       ~ "unclear"
      )
    )
}


select_en_priming <- add_reliability(en_priming) %>%
  transmute(en_target = target_word_unique,
            en_avgRT_prime = avgRT_prime, en_se = se_prime,
            en_z = z_prime, en_status = status)


select_es_priming <- add_reliability(es_priming) %>%
  transmute(es_target = target_word_unique,
            es_avgRT_prime = avgRT_prime, es_se = se_prime,
            es_z = z_prime, es_status = status)



matched_items <- translation %>%
  left_join(select_en_priming, by = "en_target") %>%
  left_join(select_es_priming, by = "es_target") %>%
  mutate(
    condition = paste0("En_", en_status, "_Span_", es_status)
  )

matched_items %>%
  group_by(condition) %>%
  summarise(count = n()) %>%
  print(n = Inf)

select_conditions <- matched_items %>%
  filter(condition %in% c("En_strong_Span_strong", 
  "En_no_Span_no", 
  "En_strong_Span_no", 
  "En_no_Span_strong"
  )) %>%
  relocate(concept_id, en_target, es_target, condition, en_avgRT_prime, es_avgRT_prime)

select_conditions %>% count(condition) %>% print(n = Inf)

full_summary <- select_conditions %>%
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

select_conditions %>%
  group_by(condition) %>%
  summarise(
    count = n())

#English priming effect close to 0 or negative
#Spanish priming effect as large as possible
#the bigger the difference between Spanish and English priming effect, the better
English_no_Spanish_strong <- select_conditions %>%
  filter(condition == "En_no_Span_strong") %>%
  mutate(
    difference_score = es_avgRT_prime - en_avgRT_prime
  ) %>%
  arrange(desc(difference_score)) %>%
  slice_head(n = 43) %>%
  select(-difference_score)

#both effects to be as close to zero as possible.
English_no_Spanish_no <- select_conditions %>%
  filter(condition == "En_no_Span_no") %>%
  mutate(
    closeness_to_zero = abs(en_avgRT_prime) + abs(es_avgRT_prime)
  ) %>%
  arrange(closeness_to_zero) %>%
  slice_head(n = 43) %>%
  select(-closeness_to_zero)

English_strong_Spanish_no <- select_conditions %>%
  filter(condition == "En_strong_Span_no")

English_strong_Spanish_strong <- select_conditions %>%
  filter(condition == "En_strong_Span_strong") %>%
  mutate(combined_strength = en_avgRT_prime + es_avgRT_prime
  ) %>%
  arrange(desc(combined_strength)) %>%
  slice_head(n = 43) %>%
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
    en_min = min(en_avgRT_prime, na.rm = TRUE),
    en_max = max(en_avgRT_prime, na.rm = TRUE),
    es_mean = mean(es_avgRT_prime, na.rm = TRUE),
    es_sd = sd(es_avgRT_prime, na.rm = TRUE),
    es_min = min(es_avgRT_prime, na.rm = TRUE),
    es_max = max(es_avgRT_prime, na.rm = TRUE),
    .groups = "drop"
  )



write_csv(full_selected_items, "selected_stimuli.csv")
