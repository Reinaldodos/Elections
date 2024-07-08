require(tidyverse)

BASE =
  "Legislatives 2024/Soirée électorale/Stock/" %>%
  arrow::open_dataset(format = "arrow")

input =
  BASE %>%
  collect() %>%
  mutate(across(
    .cols = starts_with("percent"),
    .fns = \(x)
    str_replace_all(
      string = x,
      pattern = ",",
      replacement = "\\."
    ) %>%
      as.numeric()
  ))

input$voix =
  input$voix %>%
  str_remove_all(pattern = "[^[0-9]]") %>%
  as.numeric()

input %>%
  group_nest(Departement, circo, url) %>%
  jsonlite::write_json(path = "Legislatives 2024/Soirée électorale/bilan.json")

Circos =
  BASE %>%
  select(Departement, circo) %>%
  distinct() %>%
  collect()

Elus =
  input %>%
  filter(elu_e == "OUI")

Qualifs =
  input %>%
  filter(elu_e == "QUALIF T2")

Elus_partiels =
  bind_rows(Elus, Qualifs) %>%
  anti_join(x = input, by = join_by(Departement, circo)) %>%
  filter(percent_exprimes > 50, percent_inscrits > 25) %>%
  mutate(elu_e = "NON")

Ballotage_partiel =
  bind_rows(Elus, Qualifs, Elus_partiels) %>%
  anti_join(x = input,
            by = join_by(Departement, circo)) %>%
  group_by(Departement, circo) %>%
  mutate(Rank = dense_rank(-voix)) %>%
  ungroup() %>%
  filter(percent_inscrits >= 12.5 | Rank <= 2)

Bilan =
  list(Elus, Qualifs, Ballotage_partiel, Elus_partiels) %>%
  data.table::rbindlist(fill = TRUE) %>%
  arrange(Departement, circo, nuance) %>%
  summarise(
    Situation = paste(nuance, collapse = " vs "),
    .by = c(Departement, circo, elu_e)
  ) %>%
  mutate(Duels = case_when(
    Situation %>%
      str_count(pattern = " vs ") == 0 ~ "ELU",
    Situation %>%
      str_count(pattern = " vs ") == 1 ~ "Duel",
    Situation %>%
      str_count(pattern = " vs ") == 2 ~ "Triangulaires",
    Situation %>%
      str_count(pattern = " vs ") == 3 ~ "Quadrangulaires"
  )) %>%
  inner_join(x = Circos,
             by = join_by(Departement, circo))


