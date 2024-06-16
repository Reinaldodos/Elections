
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
  )

input =
  Donnees %>%
  bind_rows(.id = "scrutin") %>%
  select(-score, -rowid)


# choix de la circo -------------------------------------------------------

circo = "5904"

input_circo =
  input %>%
  filter(
    code_du_departement == str_sub(string = circo, end = 2),
    code_de_la_circonscription == str_sub(string = circo, start = 3)
  )

data =
  input_circo %>%
  tidyr::unite(col = bulletin, scrutin, Tour, candidat) %>%
  tidyr::unite(col = bureau, code_de_la_commune, code_du_b_vote) %>%
  group_by(bulletin, bureau) %>%
  summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop")


# CAH ---------------------------------------------------------------------

hc_KOR =
  data %>%
  get_CAH()

# Clustering --------------------------------------------------------------

Groupes =
  hc_KOR %>%
  get_clusters(Nb_clusters = 4)


clusters_to_JSON(Groupes = Groupes,
                 input_circo = input_circo,
                 path = "Legislatives 2024/clusters 5904.json")

# Scoring des clusters ----------------------------------------------------

data_score = get_data_score(data = data, Groupes = Groupes)

get_plot_groupes(data_score = data_score)
