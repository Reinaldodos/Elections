pacman::p_load(tidyverse, data.table, rio, readxl, magrittr)

input =
  "~/Downloads/2020-03-16-resultats-par-bv.xlsx" %>% read_excel()

input %<>%
  count(`Libellé de la commune`, `Libellé du département`, `Code B.Vote`) %>%
  filter(n > 1) %>%
  anti_join(x = input)

Donnees_globales =
  input %>%
  select(contains("Code"), Abstentions, Blancs, Nuls) %>%
  unique()

Bureaux =
  input %>%
  select(-contains("...")) %>%
  select(contains("Code"), contains("Libellé")) %>%
  unique()

data =
  input %>%
  select(-contains("%")) %>%
  select(contains("Code"), contains("Libellé"), contains("...")) %>%
  unique() %>%
  mutate_all(as.character) %>%
  gather(key = Type,
         value = VAL,
         contains("..."),
         na.rm = T) %>%
  mutate(VAL = str_trim(VAL))

data %>%
  count(`Libellé de la commune`, `Libellé du département`,`Code B.Vote`, Type) %>%
  filter(n > 1)

data %<>%
  tidyr::separate(col = Type,
                  into = c("Type", "Num"),
                  sep = "\\.\\.\\.") %>%
  mutate(Num =
           str_remove_all(string = Num,
                          pattern = "[^[:digit:]]") %>%
           as.numeric()) %>%
  group_by(`Code du département`, `Code de la commune`, `Code B.Vote`, Type) %>%
  mutate(Num = dense_rank(Num)) %>% ungroup

data %<>%
  spread(key = Type, value = VAL) %>%
  mutate(Voix = as.numeric(Voix))

data %>% filter(is.na(Voix)) %>% mutate_all(type.convert) %>%
  semi_join(x = input) %>%
  select(-contains("%")) %>%
  select(contains("Code"), contains("Libellé"), contains("...")) %>%
  unique() %>%
  mutate_all(as.character) %>%
  gather(key = Type,
         value = VAL,
         contains("..."),
         na.rm = T) %>%
  filter(str_detect(string = Type, pattern = "Voix")) %>%
  pull(VAL) %>% as.numeric()
