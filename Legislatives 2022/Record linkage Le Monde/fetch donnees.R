pacman::p_load(rio, magrittr, tidyverse,
               data.table)
LeMonde =
  "Legislatives 2022/candidats_legislatives_lemonde.csv" %>%
  rio::import() %>%
  janitor::clean_names()

Candidats =
  "Legislatives 2022/resultats-par-niveau-burvot-t1-france-entiere.xlsx" %>%
  readxl::read_excel(col_types = c("text"))

Candidats %<>% janitor::clean_names()
Candidats %<>% rowid_to_column()

Candidats %>% map(class)

CLINNE <- function(data) {
  names(data) = c("rowid", "nom", "prenom", "sexe", "num_panneau", "voix")
  return(data)
}


Scores =
  map(.x = 8 * 0:21,
      .f = ~ Candidats[, c(1, 25 + ., 26 + ., 24 + ., 23 + ., 28 + .)]) %>%
  map(.f = CLINNE) %>%
  rbindlist()

Bureaux =
  Candidats %>%
  select(rowid, starts_with("code"), starts_with("libel")) %>%
  distinct()

Candidats =
  Candidats %>%
  distinct(rowid, code_du_departement, code_de_la_circonscription) %>%
  inner_join(y = Scores, by = "rowid") %>%
  distinct(code_du_departement, code_de_la_circonscription,
           nom, prenom, sexe, num_panneau)

save.image(file = "Legislatives 2022/Record linkage Le Monde/donnees.Rdata")
