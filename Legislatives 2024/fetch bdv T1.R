require(tidyverse)

input =
  "https://www.data.gouv.fr/fr/datasets/r/6813fb28-7ec0-42ff-a528-2bc3d82d7dcd" %>%
  rio::import(format = "csv", header = TRUE) %>%
  janitor::clean_names() %>%
  rowid_to_column(var = "rowid")

Bureaux =
  input %>%
  distinct(rowid,
           code_departement,
           libelle_departement,
           code_commune,
           libelle_commune,
           code_bv)  %>%
  transmute(
    rowid,
    code_du_departement =
      str_c("0", code_departement) %>%
      str_sub(start = -2),
    libelle_du_departement = libelle_departement,
    code_de_la_commune =
      code_commune %>%
      str_sub(start = -3),
    libelle_de_la_commune = libelle_commune,
    code_du_b_vote =
      str_c("0000", code_bv) %>%
      str_sub(start = -4),
  )

Non_exprimes =
  input %>%
  select(rowid, abstentions, nuls, blancs) %>%
  pivot_longer(cols = -rowid,
               names_to = "candidat",
               values_to = "voix")
Listes =
  input %>%
  select(rowid,
         starts_with("nuance_")) %>%
  pivot_longer(cols = -rowid,
               names_to = "TOTO",
               values_to = "candidat") %>%
  filter(nchar(candidat) > 0) %>%
  mutate(TOTO = TOTO %>% str_remove_all(pattern = "[^[0-9]]"))

Noms =
  input %>%
  select(rowid,
         starts_with("nom")) %>%
  pivot_longer(cols = -rowid,
               names_to = "TOTO",
               values_to = "nom") %>%
  filter(nchar(nom) > 0) %>%
  mutate(TOTO = TOTO %>% str_remove_all(pattern = "[^[0-9]]"))

Prenoms =
  input %>%
  select(rowid,
         starts_with("nom")) %>%
  pivot_longer(cols = -rowid,
               names_to = "TOTO",
               values_to = "prenom") %>%
  filter(nchar(prenom) > 0) %>%
  mutate(TOTO = TOTO %>% str_remove_all(pattern = "[^[0-9]]"))

Voix =
  input %>%
  select(rowid,
         starts_with("voix_")) %>%
  pivot_longer(cols = -rowid,
               names_to = "TOTO",
               values_to = "voix") %>%
  drop_na() %>%
  mutate(TOTO = TOTO %>% str_remove_all(pattern = "[^[0-9]]"))

Resultats =
  list(Listes,Noms, Prenoms, Voix) %>%
  reduce(.f = inner_join,
             by = join_by(rowid, TOTO)) %>%
  select(-TOTO) %>%
  bind_rows(Non_exprimes) %>%
  filter(voix > 0) %>%
  inner_join(x = Bureaux,
             by= join_by(rowid))

Resultats %>%
  select(-rowid) %>%
  saveRDS(file = "Legislatives 2024/resultats bdv T1.rds")
