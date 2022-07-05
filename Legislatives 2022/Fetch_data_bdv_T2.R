pacman::p_load(tidyverse, magrittr, data.table, janitor, readxl)

input =
  "Legislatives 2022/resultats-par-niveau-burvot-t2-france-entiere.xlsx" %>%
  readxl::read_excel()

input %<>% janitor::clean_names()
input %<>% rowid_to_column()


CLINNE <- function(data) {
  names(data) = c("rowid", "candidat", "voix")
  return(data)
}
Scores =
  map(.x = 8*0:2, .f = ~input[,c(1, 27 + ., 28 + .)]) %>%
  map(.f = CLINNE) %>%
  rbindlist() %>% drop_na()
Bureaux = input[, 1:8]

Abstention =
  input %>% select(rowid, abstentions, blancs, nuls) %>%
  pivot_longer(cols = -rowid,
               names_to = "candidat",
               values_to = "voix")

output_T2 =
  bind_rows(Scores, Abstention) %>%
  inner_join(x = Bureaux, by = "rowid") %>%
  drop_na(candidat, voix)


output_T2 %<>%
  group_by(rowid) %>%
  mutate(score = voix/sum(voix)) %>%
  ungroup()

rm(input, Scores, Abstention)
gc()
