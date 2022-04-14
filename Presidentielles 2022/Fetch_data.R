require(rvest)
require(magrittr)
require(tidyverse)

url =
  "https://www.resultats-elections.interieur.gouv.fr/presidentielle-2022/"

Get_url_departements <- function(url) {
  Departements =
    url %>% read_html() %>%
    html_elements(css = "#listeDpt") %>%
    html_children() %>%
    html_attr(name = "value")

  Departements =
    url %>%
    str_remove_all(pattern = "index.html") %>%
    str_c(., Departements)

  return(Departements)
}

Departements = Get_url_departements(url = url)

get_liste_communes <- function(departement) {
  Liste_Communes =
    departement %>%
    read_html() %>%
    html_elements(css = ".span8 a") %>%
    html_attr(name = "href")

  Liste_Communes =
    departement %>%
    str_remove_all(pattern = "index.html") %>%
    str_c(., Liste_Communes)

  return(Liste_Communes)
}

Liste_Communes = map(.x = Departements, .f = get_liste_communes)
Liste_Communes %<>% flatten_chr()

get_noms_communes <- function(nom_commune, departement) {
  Noms_communes =
    nom_commune %>%
    read_html() %>%
    html_elements(css = ".tableau-communes a") %>%
    html_attr(name = "href")

  if (length(Noms_communes) == 0)
    return(nom_commune)

  toto = nom_commune %>% str_split(pattern = "/") %>% flatten_chr()

  Noms_communes =
    nom_commune %>%
    str_remove_all(pattern = toto[length(toto)])  %>%
    str_c(., Noms_communes)

  return(Noms_communes)
}

Communes = map(.x = Liste_Communes, .f = get_noms_communes)
Communes %<>% flatten_chr()

Communes %<>% str_subset(pattern = "#", negate = T)

FICHIERS =
  list.files(path = "Presidentielles 2022/data/",
             pattern = "json")

Stash = Communes

TOTO = Stash %>% janitor::make_clean_names() %>% str_c(., ".json")

Stash = Stash[!TOTO %in% FICHIERS]

while (length(Stash) > 0) {
  cat("Still to go:", length(Stash), "/", length(Communes), "\n")
  commune = Stash %>% sample(1)

  resultats =
    commune %>%
    read_html() %>%
    html_table()

  if (length(resultats) == 3) {
    names(resultats) = c("TITRE",
                         "Scores T1",
                         "Global T1")

    nom_fichier = commune %>% janitor::make_clean_names()

    resultats %>% jsonlite::toJSON(pretty = T) %>%
      write(file = str_c("Presidentielles 2022/data/",
                         nom_fichier,
                         ".json"))

  }
  toto = commune %>% str_split(pattern = "/") %>% flatten_chr()
  Stash = str_subset(string = Stash,
                     pattern = toto[length(toto)],
                     negate = T)
}
