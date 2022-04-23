require(tidyverse)
require(magrittr)
require(data.table)

input =
  "~/Téléchargements/resultats-par-niveau-burvot-t1-france-entiere.xlsx" %>%
  readxl::read_excel()

input %<>% janitor::clean_names()
input %<>% rowid_to_column()

input %>% sample_n(1) %>% as.list()

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

output =
  bind_rows(Scores, Abstention) %>%
  inner_join(x = Bureaux, by = "rowid")

output %<>%
  group_by(rowid) %>%
  mutate(score = voix/sum(voix)) %>%
  ungroup()

