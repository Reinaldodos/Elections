
source(file = "Legislatives 2024/Fonctions.R",
       encoding = "UTF-8")

# choix de la circo -------------------------------------------------------

circo = "5904"

input =
  "Legislatives 2024/data/donnees.parquet" %>%
  arrow::open_dataset()

input_circo =
  input %>%
  distinct(code_du_departement,
           code_de_la_circonscription) %>%
  collect() %>%
  filter(
    code_du_departement == str_sub(string = circo, end = 2),
    code_de_la_circonscription == str_sub(string = circo, start = 3)
  ) %>%
  semi_join(x = input,
            by = join_by(code_du_departement, code_de_la_circonscription))

data =
  input_circo %>%
  collect() %>%
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

data %>%
  get_data_score(Groupes = Groupes) %>%
  get_plot_groupes()
