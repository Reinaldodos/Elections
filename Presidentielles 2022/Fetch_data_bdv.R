pacman::p_load(tidyverse, magrittr, data.table, janitor, readxl)

input =
  "~/Téléchargements/resultats-par-niveau-burvot-t1-france-entiere.xlsx" %>%
  readxl::read_excel()

input %<>% janitor::clean_names()
input %<>% rowid_to_column()


CLINNE <- function(data) {
  names(data) = c("rowid", "candidat", "voix")
  return(data)
}
Scores =
  map(.x = 7*0:11, .f = ~input[,c(1, 25 + ., 27 + .)]) %>%
  map(.f = CLINNE) %>%
  rbindlist()
Bureaux = input[, 1:8]

Abstention =
  input %>% select(rowid, abstentions, blancs, nuls) %>%
  pivot_longer(cols = -rowid,
               names_to = "candidat",
               values_to = "voix")

output_T1 =
  bind_rows(Scores, Abstention) %>%
  inner_join(x = Bureaux, by = "rowid")

output_T1 %<>%
  group_by(rowid) %>%
  mutate(score = voix/sum(voix)) %>%
  ungroup()

rm(input, Scores, Abstention)
gc()
