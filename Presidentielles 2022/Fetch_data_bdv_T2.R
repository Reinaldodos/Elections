require(tidyverse)
require(magrittr)
require(data.table)

input =
  "Presidentielles 2022/resultats-par-niveau-burvot-t2-france-entiere.xlsx" %>%
  readxl::read_excel()

input %<>% janitor::clean_names()
input %<>% rowid_to_column()


CLINNE <- function(data) {
  names(data) = c("rowid", "candidat", "voix")
  return(data)
}
input %>% sample_n(1) %>% as.list()
Scores =
  map(.x = 7*0:1, .f = ~input[,c(1, 25 + ., 27 + .)]) %>%
  map(.f = CLINNE) %>%
  rbindlist()
Bureaux = input[, 1:8]

Abstention =
  input %>% select(rowid, abstentions, blancs, nuls) %>%
  pivot_longer(cols = -rowid,
               names_to = "candidat",
               values_to = "voix")

output_T2 =
  bind_rows(Scores, Abstention) %>%
  inner_join(x = Bureaux, by = "rowid")

output_T2 %<>%
  group_by(rowid) %>%
  mutate(score = voix/sum(voix)) %>%
  ungroup()

