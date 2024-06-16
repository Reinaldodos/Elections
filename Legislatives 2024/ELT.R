source(file = "Legislatives 2024/Fonctions.R",
       encoding = "UTF-8")


# ELT données -------------------------------------------------------------

Donnees =
  list(PDT = "Presidentielles 2022/resultats.rds",
       LGS = "Legislatives 2022/resultats_raw.rds",
       EURO = "Europeennes 2024/output/resultats.rds") %>%
  map(.f = read_rds)

Communes_circo =
  "Legislatives 2024/bureaux-de-vote-circonscriptions.csv"  %>%
  rio::import() %>%
  transmute(
    code_du_departement = codeDepartement %>%
      str_replace_all(pattern = "99", replacement = "ZZ"),
    code_de_la_circonscription = codeCirconscription %>%
      str_sub(start = -2),
    code_de_la_commune = codeCommune %>%
      str_sub(start = -3),
    code_du_b_vote = codeBureauVote %>%
      str_sub(start = -4)
  )

Donnees$EURO =
  Communes_circo %>%
  distinct() %>%
  inner_join(
    x = Donnees$EURO,
    by = join_by(code_du_departement, code_de_la_commune, code_du_b_vote)
  ) %>%
  mutate(Tour = "T1")

input =
  Donnees %>%
  bind_rows(.id = "scrutin") %>%
  select(-score, -rowid)

input %>%
  arrow::write_parquet(sink = "Legislatives 2024/data/donnees.parquet")
