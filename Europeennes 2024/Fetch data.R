pacman::p_load(tidyverse, rvest)

url_resultats =
  "https://www.resultats-elections.interieur.gouv.fr/europeennes2024/index.html"

departements =
  url_resultats %>%
  get_departements()

All_lettres =
  departements %>%
  map(.f = html_get_lettres,
      .progress = TRUE) %>%
  flatten_chr()

All_communes =
  All_lettres %>%
  map(.f = get_communes,
      .progress = TRUE) %>%
  flatten_chr()

All_tables =
  All_communes %>%
  purrr::set_names() %>%
  map(.f = safely(get_tables_bled),
      .progress = TRUE)



