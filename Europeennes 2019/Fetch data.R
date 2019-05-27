pacman::p_load(rvest, tidyverse, data.table)
url = "https://elections.interieur.gouv.fr/europeennes-2019/"

FETCH <- function(ville) {
  input = ville %>% read_html()
  data = input %>% html_table()

  nom = input %>% html_nodes(css = ".pub-fil-ariane a") %>% html_text()
  nom = paste(nom, collapse = "--")

  Scores =
    data[[2]] %>%
    select(Listes, Voix) %>%
    mutate_all(type.convert)

  Participation =
    data[[3]] %>% data.table() %>%
    select(Listes = V1, Voix = Nombre) %>%
    filter(Listes %in% c("Abstentions", "Blancs", "Nuls")) %>%
    mutate(Voix = str_remove_all(string = Voix, pattern = "[^[0-9]]")) %>%
    mutate_all(type.convert)

  require(jsonlite)
  list(Score = Scores, Participation = Participation) %>%
    bind_rows() %>%
    jsonlite::write_json(path = str_c("Europeennes 2019/Soiree_electorale/data/",
                                      nom,
                                      ".json"))

  DEJA_FAIT <<- append(x = DEJA_FAIT, values = ville)
  print(nom)
}



LETTRE <- function(lettre_ville) {
  print(lettre_ville)
  lettre_ville %>%
    read_html() %>% html_nodes(css = ".tableau-communes a") %>%
    html_attr(name = "href") %>% str_remove_all(pattern = "\\.\\./") %>%
    str_c(url %>% str_remove_all(pattern = "index.html"), .) %>%
    setdiff(DEJA_FAIT) %>%
    return()
}

safeFETCH = safely(FETCH)

ALL_IN <- function(lettre_ville) {
  lettre_ville %>% LETTRE() %>% walk(safeFETCH)
}

safeIN = safely(ALL_IN)

Lettres_villes = "Europeennes 2019/sources.rds" %>% readRDS()
DEJA_FAIT = NULL

repeat {
  Lettres_villes %>% sample(1) %>% walk(safeIN)
}
