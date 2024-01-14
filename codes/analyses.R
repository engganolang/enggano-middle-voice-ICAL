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



# OLD: type frequency analysis of the situation types =====
## grouped by potentially_middle column and summarised by word form
type_freq_01_by_word <- df1 |> 
  group_by(potentially_middle) |> 
  summarise(n_types = n_distinct(word)) |> 
  arrange(desc(n_types))
# type_freq_01_by_word
## grouped by potentially_middle column and summarised by morpheme gloss
### this one is better because the allomorphs are grouped under the same form/morpheme
type_freq_02_by_morph <- df1 |> 
  group_by(potentially_middle) |> 
  summarise(n_types = n_distinct(morph_gloss_en)) |> 
  arrange(desc(n_types))
# type_freq_02_by_morph
### OLD: visualisation =====
type_freq_02_plot_df <- type_freq_02_by_morph |> 
  slice_max(order_by = n_types, n = 10) |> 
  rename(situation_types = potentially_middle, type_freq = n_types) |> 
  mutate(situation_types = str_replace_all(situation_types, "^y_", ""),
         situation_types = str_replace_all(situation_types, "\\s\\(.+?\\)", ""),
         situation_types = str_replace_all(situation_types, "^spontaneous", "spont."))

type_freq_02_plot_df |> 
  mutate(situation_types = factor(situation_types, 
                                  levels = type_freq_02_plot_df$situation_types) #, 
         # situation_types = fct_rev(situation_types)
         ) |> 
  ggplot(aes(x = situation_types, y = type_freq)) + 
  geom_col() + 
  scale_x_discrete(guide = guide_axis(angle = 50)) +
  labs(y = "Type frequency", x = "Situation types")
# ggsave("figures/01-productivity-middle.png", dpi = 600, units = "in", width = 6, height = 5)
  # coord_flip()

# type frequency analysis of the situation types with the unified English gloss =====
## grouped by potentially_middle and unified_gloss columns and summarised by word form
type_freq_03_by_word_and_gloss <- df1 |> 
  group_by(potentially_middle, unified_gloss) |> 
  summarise(n_types = n_distinct(word)) |> 
  arrange(desc(potentially_middle), desc(n_types))
# type_freq_03_by_word_and_gloss
## grouped by potentially_middle and unified_gloss columns and summarised by morpheme gloss
### this one is better because the allomorphs are grouped under the same form/morpheme
type_freq_04_by_morph_and_gloss <- df1 |> 
  group_by(potentially_middle, unified_gloss) |> 
  summarise(n_types = n_distinct(morph_gloss_en)) |> 
  arrange(desc(n_types))
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
