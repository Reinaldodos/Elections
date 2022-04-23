pacman::p_load(tidyverse, rio, magrittr, data.table)
input_T2 = "https://www.data.gouv.fr/fr/datasets/r/2e3e44de-e584-4aa2-8148-670daf5617e1"
input_T2 %<>% import(encoding = "UTF-8") %>% rowid_to_column()
input_T2 %<>% janitor::clean_names()

input_T2 %<>%
  select(
    rowid,
    abstentions,
    blancs,
    nuls,
    EM = nom,
    Voix_EM = voix,
    MLP = v31,
    Voix_MLP = v33
  )

Exprimes =
  list(
    input_T2 %>% select(rowid, EM, Voix_EM) %>% spread(key = EM, value = Voix_EM),
    input_T2 %>% select(rowid, MLP, Voix_MLP) %>% spread(key = MLP, value = Voix_MLP)
  ) %>% reduce(.f = inner_join, by = "rowid")

output_T2 =
  input_T2 %>%
  select(rowid, abstentions, blancs, nuls) %>%
  inner_join(x = Exprimes) %>%
  gather(key = candidat,
         value = voix,
         -rowid,
         na.rm = T)
