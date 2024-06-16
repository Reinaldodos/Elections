
source(file = "Legislatives 2024/Fonctions.R",
       encoding = "UTF-8")

# choix de la circo -------------------------------------------------------

input =
  "Legislatives 2024/data/donnees.parquet" %>%
  arrow::open_dataset()

input_circo =
  get_data_circo(input = input,
                 circo = "4101")

data =
  input_circo %>%
  unite_data()

# CAH ---------------------------------------------------------------------

hc_KOR =
  data %>%
  get_CAH()

# Clustering --------------------------------------------------------------

Groupes =
  hc_KOR %>%
  get_clusters(Nb_clusters = 4)

get_clusters_libelles(Groupes = Groupes,
                      input_circo = input_circo) %>%
  clusters_to_JSON(path = "Legislatives 2024/clusters 4101.json")

# Scoring des clusters ----------------------------------------------------

data %>%
  get_data_score(Groupes = Groupes) %>%
  get_plot_groupes()
