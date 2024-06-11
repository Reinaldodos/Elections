output =
  All_tables %>%
  purrr::transpose() %>%
  map(.f = purrr::compact)

Done =
  output$result %>%
  names()

Donnees =
  output$result %>%
  bind_rows(.id = "url")

Done =
  Donnees %>%
  filter(is.na(Voix)) %>%
  distinct(url) %>%
  setdiff(Done, .)

Donnees =
  Donnees %>%
  filter(url %in% Done)


# Deuxième passage --------------------------------------------------------
output =
  setdiff(All_communes, Done) %>%
  purrr::set_names() %>%
  map(.f = safely(get_tables_bled), .progress = TRUE) %>%
  purrr::transpose() %>%
  map(.f = purrr::compact)

Batch =
  output$result %>%
  bind_rows(.id = "url")

Batch =
  Batch %>%
  mutate(
    `Liste des candidatures par suffrages exprimés` =
      if_else(
        condition = is.na(`Liste des candidatures par suffrages exprimés...1`),
        true = `Liste des candidatures par suffrages exprimés...3`,
        false = `Liste des candidatures par suffrages exprimés...1`
      )
  ) %>%
  select(-`Liste des candidatures par suffrages exprimés...1`,
         -`Liste des candidatures par suffrages exprimés...3`)

Donnees =
  list(Batch, Donnees) %>%
  data.table::rbindlist(use.names = TRUE)


# Troisième jeu -----------------------------------------------------------
output =
  setdiff(All_communes, Donnees$url) %>%
  str_replace_all(pattern = "geographique", replacement = "geographique/") %>%
  purrr::set_names() %>%
  map(.f = safely(get_tables_bled), .progress = TRUE) %>%
  purrr::transpose() %>%
  map(.f = purrr::compact)


Batch =
  output$result %>%
  bind_rows(.id = "url") %>%
  mutate(
    `Liste des candidatures par suffrages exprimés` =
      if_else(
        condition = is.na(`Liste des candidatures par suffrages exprimés...1`),
        true = `Liste des candidatures par suffrages exprimés...3`,
        false = `Liste des candidatures par suffrages exprimés...1`
      )
  ) %>%
  select(
    -`Liste des candidatures par suffrages exprimés...1`,
    -`Liste des candidatures par suffrages exprimés...3`
  )

Donnees =
  list(Batch, Donnees) %>%
  data.table::rbindlist(use.names = TRUE)

# Passage 4 : Paris ---------------------------------------------------------------
Paris =
  str_c(
  "https://www.resultats-elections.interieur.gouv.fr/europeennes2024/ensemble_geographique/11/75/751",
  sprintf("%02d/index.html", 1:20)
) %>%
  purrr::set_names() %>%
  map(.f = get_tables_bled, .progress = TRUE)

Batch =
  Paris %>%
  bind_rows(.id = "url") %>%
  mutate(
    `Liste des candidatures par suffrages exprimés` =
      if_else(
        condition = is.na(`Liste des candidatures par suffrages exprimés...1`),
        true = `Liste des candidatures par suffrages exprimés...3`,
        false = `Liste des candidatures par suffrages exprimés...1`
      )
  ) %>%
  select(
    -`Liste des candidatures par suffrages exprimés...1`,
    -`Liste des candidatures par suffrages exprimés...3`
  )

Donnees =
  list(Batch, Donnees) %>%
  data.table::rbindlist(use.names = TRUE)

arrow::write_parquet(x = Donnees,
                     sink = "Europeennes 2024/resultats communes.parquet")

