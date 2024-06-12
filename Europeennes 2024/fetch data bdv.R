
europeennes =
  "https://www.data.gouv.fr/fr/datasets/r/937bb638-a487-40cd-9a0b-610d539a4207" %>%
  rio::import(format = "csv", header = TRUE, skip = 1) %>%
  janitor::clean_names() %>%
  rowid_to_column(var = "rowid")


Bureaux =
  europeennes %>%
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
  europeennes %>%
  select(rowid, abstentions, nuls, blancs) %>%
  pivot_longer(cols = -rowid,
               names_to = "candidat",
               values_to = "voix")
Listes =
  europeennes %>%
  select(rowid,
         starts_with("libelle_abrege_de_liste")) %>%
  pivot_longer(cols = -rowid,
               names_to = "TOTO",
               values_to = "candidat") %>%
  mutate(TOTO = TOTO %>% str_remove_all(pattern = "[^[0-9]]"))

Voix =
  europeennes %>%
  select(rowid,
         starts_with("voix_")) %>%
  pivot_longer(cols = -rowid,
               names_to = "TOTO",
               values_to = "voix") %>%
  mutate(TOTO = TOTO %>% str_remove_all(pattern = "[^[0-9]]"))
)

Resultats =
  inner_join(x = Listes,
           y = Voix,
           by = join_by(rowid, TOTO)) %>%
  select(-TOTO) %>%
  bind_rows(Non_exprimes) %>%
  filter(voix > 0) %>%
  inner_join(x = Bureaux,
             by= join_by(rowid))

Resultats %>%
  select(-rowid) %>%
  saveRDS(file = "Europeennes 2024/resultats.rds")


