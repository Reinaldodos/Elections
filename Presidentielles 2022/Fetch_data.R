require(tidyverse)
require(magrittr)
require(data.table)

input =
  "~/Téléchargements/resultats-par-niveau-subcom-t1-france-entiere.xlsx" %>%
  # "~/Téléchargements/resultats-par-niveau-burvot-t1-france-entiere.xlsx" %>%
  readxl::read_excel()

input %<>% janitor::clean_names()
input %<>% rowid_to_column()

input %>% sample_n(1) %>% as.list()

CLINNE <- function(data) {
  names(data) = c("rowid", "candidat", "voix")
  return(data)
}
Scores =
  map(.x = 7*0:11, .f = ~input[,c(1, 23 + ., 25 + .)]) %>%
  map(.f = CLINNE) %>%
  rbindlist()
Bureaux = input[, 1:5]

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

DATA =
  output %>%
  mutate(code_de_la_commune =
           str_c("00000", code_de_la_commune) %>%
           str_sub(start = -3)) %>%
  mutate(insee = str_c(code_du_departement, code_de_la_commune)) %>%
  select(insee, contains("departement"), candidat, score, voix)

input_2017 =
  "https://www.data.gouv.fr/fr/datasets/r/f4c23dab-46ff-4799-b217-1ab29db7938b" %>%
  rio::import(format = "csv") %>%
  rowid_to_column()

output_2017 =
  input_2017 %>%
  mutate_at(.vars = vars(ends_with("_ins")),
            .funs = ~.*Inscrits/100) %>%
  select(rowid, ends_with("_ins")) %>%
  select(-Votants_ins, -Exprimés_ins) %>%
  gather(key = candidat, value = score, -rowid) %>%
  left_join(x = input_2017 %>% select(rowid, insee = CodeInsee),
            by = "rowid") %>%
  group_by(insee, candidat) %>%
  summarise(voix = sum(score)) %>%
  mutate(score = voix /sum(voix)) %>% ungroup() %>%
  mutate_at(.vars = "candidat",
            .funs = str_remove_all,
            pattern = "_ins$")


output_2017 %<>% mutate_at(.vars = "candidat", .funs = str_to_title)
DATA %<>% mutate_at(.vars = "candidat", .funs = str_to_title)

FINAL =
  list(
    "2017" = output_2017,
    "2022" = DATA) %>%
  bind_rows(.id = "annee")


