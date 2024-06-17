library(tidyverse)

readxl::read_xlsx("data/data_words-and-sentences.xlsx") |> 
  filter(str_detect(potentially_middle, "^y_")) |> 
  write_rds("data/data_words-and-sentences.rds")

df <- read_rds("data/data_words-and-sentences.rds")

# df1 <- df |> 
#   select(word, eno_word_gloss_en, potentially_middle) |> 
#   mutate(eno_word_gloss_en = str_replace(eno_word_gloss_en,
#                                          "^already ", ""),
#          eno_word_gloss_en = str_replace(eno_word_gloss_en,
#                                          "go past", "go.past"),
#          eno_word_gloss_en = str_replace(eno_word_gloss_en,
#                                          "tell story", "tell.story"),
#          eno_word_gloss_en = str_replace(eno_word_gloss_en,
#                                          "wear shirt", "wear.shirt")) |> 
#   distinct()

# df1 |> 
#   count(eno_word_gloss_en) |> 
#   arrange(eno_word_gloss_en) |> 
#   as.data.frame() |> 
#   write_tsv("data/word-glosses.txt")

unified_gloss <- readxl::read_xlsx("data/word-glosses.xlsx") |> 
  select(-n)

df <- df |>
  mutate(eno_word_gloss_en = str_replace(eno_word_gloss_en,
                                         "^already ", ""),
         eno_word_gloss_en = str_replace(eno_word_gloss_en,
                                         "go past", "go.past"),
         eno_word_gloss_en = str_replace(eno_word_gloss_en,
                                         "tell story", "tell.story"),
         eno_word_gloss_en = str_replace(eno_word_gloss_en,
                                         "wear shirt", "wear.shirt"),
         morph_gloss_en = str_replace_all(morph_gloss_en, "^if(?=_)", "IF"),
         morph_gloss_en = str_replace_all(morph_gloss_en, "^when(?=_)", "WHEN"),
         morph_gloss_en = str_replace_all(morph_gloss_en, "want(?=___BU_)", "WANT"),
         morph_gloss_en = str_replace_all(morph_gloss_en, "\\s(?=_)", ""),
         morph_gloss_en = str_replace_all(morph_gloss_en, "(?<=[a-z])\\s(?=[a-z])", "."),
         morph_gloss_en = str_replace_all(morph_gloss_en, "[;]\\s", "/"),
         morph_gloss_en = str_replace_all(morph_gloss_en, "[,]\\s", "/"))

df1 <- df |> 
  left_join(unified_gloss)
stems <- df1 |> 
  pull(morph_gloss_en) |> 
  str_extract_all("[a-z./]{2,}") |> 
  map(function(x) paste(x, collapse = ".")) |> 
  unlist()
df1$stem <- stems

# Distribution of middle semantics with PA- =========
pa_morph <- df1 |> 
  mutate(TEXT_TYPE = replace(TEXT_TYPE, text_title == "Kahler 1955 Retelling", "naturalistic")) |> 
  # filter(TEXT_TYPE == "naturalistic") |> 
  filter(PA_MORPH) |> # select the word with p-/pa- in its morpheme gloss
  filter(str_detect(morph_gloss_en, "^NM", negate = TRUE)) |> # exclude word with nominal marker e-
  filter(word != "kapakahai'") |> # a causative according to Charlotte
  filter(str_detect(potentially_middle, "^y_")) |> # potentially middle data
  filter(str_detect(potentially_middle, "_direct\\-reflexive", negate = TRUE)) |> # exclude direct reflexive
  mutate(middle_sem = if_else(str_detect(potentially_middle, "reciprocal"),
                              "reciprocal", potentially_middle),
         middle_sem = if_else(str_detect(potentially_middle, "collective"),
                              "collective", middle_sem),
         middle_sem = if_else(str_detect(potentially_middle, "spontaneous"),
                              "spontaneous-events", middle_sem),
         middle_sem = if_else(str_detect(potentially_middle, "speech"),
                              "speech-actions", middle_sem),
         # middle_sem = if_else(str_detect(potentially_middle, "(direct\\-reflex|indirect\\-middle)"),
                              # "reflexive", middle_sem),
         middle_sem = if_else(str_detect(potentially_middle, "antipassive"),
                              "antipassive", middle_sem),
         middle_sem = str_replace_all(middle_sem, "(^y_|\\s+\\([^\\)]+\\))", ""))

## interim storage of the pa_morph data for ICAL 2024
pa_morph |> 
  write_tsv("data/pa_morph_ical2024.tsv")
pa_morph |> 
  write_rds("data/pa_morph_ical2024.rds")
pa_morph <- read_rds("data/pa_morph_ical2024.rds")

## type freq of the middle semantics by word form
pa_middle_word_productivity <- pa_morph |> 
  group_by(middle_sem) |> 
  summarise(typefreq = n_distinct(word)) |> 
  arrange(desc(typefreq))
pa_middle_word_productivity

## type freq of the middle semantics by stem form
pa_middle_stem_productivity <- pa_morph |> 
  group_by(middle_sem) |> 
  summarise(typefreq = n_distinct(stem)) |> 
  arrange(desc(typefreq))
pa_middle_stem_productivity

pa_middle <- pa_middle_word_productivity |> 
  mutate(forms = "word forms") |> 
  bind_rows(pa_middle_stem_productivity |> 
              mutate(forms = "root forms")) |> 
  mutate(middle_sem = factor(middle_sem,
                             levels = pa_middle_word_productivity$middle_sem),
         forms = factor(forms, levels = c("word forms", "root forms")))

pa_middle |> 
  ggplot(aes(x = middle_sem, y = typefreq, fill = forms)) + 
  geom_col(position = position_dodge(.9)) + 
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  labs(x = "Middle Semantics", y = "Type frequency",
       fill = "Count based on")
ggsave("figures/02-productivity-middle-ical2024.png", 
       dpi = 600, units = "in", width = 6, height = 3)

# OLD: type frequency analysis of the situation types =====
## grouped by potentially_middle column and summarised by word form
# type_freq_01_by_word <- df1 |> 
#   group_by(potentially_middle) |> 
#   summarise(n_types = n_distinct(word)) |> 
#   arrange(desc(n_types))
# type_freq_01_by_word
## grouped by potentially_middle column and summarised by morpheme gloss
### this one is better because the allomorphs are grouped under the same form/morpheme
# type_freq_02_by_morph <- df1 |> 
#   group_by(potentially_middle) |> 
#   summarise(n_types = n_distinct(morph_gloss_en)) |> 
#   arrange(desc(n_types))
# type_freq_02_by_morph
### OLD: visualisation =====
# type_freq_02_plot_df <- type_freq_02_by_morph |> 
#   slice_max(order_by = n_types, n = 10) |> 
#   rename(situation_types = potentially_middle, type_freq = n_types) |> 
#   mutate(situation_types = str_replace_all(situation_types, "^y_", ""),
#          situation_types = str_replace_all(situation_types, "\\s\\(.+?\\)", ""),
#          situation_types = str_replace_all(situation_types, "^spontaneous", "spont."))

# type_freq_02_plot_df |> 
#   mutate(situation_types = factor(situation_types, 
#                                   levels = type_freq_02_plot_df$situation_types) #, 
#          # situation_types = fct_rev(situation_types)
#          ) |> 
#   ggplot(aes(x = situation_types, y = type_freq)) + 
#   geom_col() + 
#   scale_x_discrete(guide = guide_axis(angle = 50)) +
#   labs(y = "Type frequency", x = "Situation types")
# ggsave("figures/01-productivity-middle.png", dpi = 600, units = "in", width = 6, height = 5)
  # coord_flip()

# type frequency analysis of the situation types with the unified English gloss =====
## grouped by potentially_middle and unified_gloss columns and summarised by word form
# type_freq_03_by_word_and_gloss <- df1 |> 
#   group_by(potentially_middle, unified_gloss) |> 
#   summarise(n_types = n_distinct(word)) |> 
#   arrange(desc(potentially_middle), desc(n_types))
# type_freq_03_by_word_and_gloss
## grouped by potentially_middle and unified_gloss columns and summarised by morpheme gloss
### this one is better because the allomorphs are grouped under the same form/morpheme
# type_freq_04_by_morph_and_gloss <- df1 |> 
#   group_by(potentially_middle, unified_gloss) |> 
#   summarise(n_types = n_distinct(morph_gloss_en)) |> 
#   arrange(desc(n_types))
# type_freq_04_by_morph_and_gloss

##





# df |> filter(potentially_middle == "y_translational-motion (11)") |> 
#   select(word, eno_word_gloss_en, morph_gloss_en, pa_morph) |> 
#   distinct() |> 
#   count(pa_morph)
# 
# df |> filter(potentially_middle == "y_translational-motion (11)") |> 
#   select(word, eno_word_gloss_en, morph_gloss_en, RECIP) |> 
#   distinct() |> 
#   count(RECIP)
# 
# df |> filter(potentially_middle == "y_translational-motion (11)") |> 
#   select(word, eno_word_gloss_en, morph_gloss_en, BU) |> 
#   distinct() |> 
#   count(BU) |> 
#   mutate(perc = n/sum(n) * 100)
# 
# df |> filter(potentially_middle == "y_translational-motion (11)") |> 
#   select(word, eno_word_gloss_en, morph_gloss_en, KI) |> 
#   distinct() |> 
#   count(KI) |> 
#   mutate(perc = n/sum(n) * 100)
