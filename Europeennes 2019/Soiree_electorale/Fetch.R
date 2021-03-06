pacman::p_load(tidyverse, data.table, jsonlite)

Results =
  "./data/" %>% list.files(pattern = "json", full.names = T)  %>%
  purrr::set_names() %>%
  map(.f = fromJSON) %>%
  bind_rows(.id = "file") %>%
  tidyr::extract(
    col = file,
    into = c("Region", "Departement", "Ville"),
    regex = "(.*)--(.*)--(.*).json"
  ) %>%
  mutate(Region = str_remove_all(string = Region, pattern = "./data/Accueil Européennes 2019")) %>%
  mutate(Region = str_remove_all(string = Region, pattern = "--"))

Results =
  Results %>%
  mutate_all(type.convert) %>%
  data.table()

Results =
  Results %>%
  distinct(Departement) %>%
  extract(
    col = Departement,
    into = c("Dep", "DOM"),
    regex = "(.*) \\((.*)\\).*",
    remove = F
  ) %>%
  mutate(DOM = as.numeric(DOM)) %>%
  mutate(DOM = DOM > 100 & !is.na(DOM)) %>%
  left_join(x = Results, by = "Departement")

Order =
  Results %>%
  group_by(Listes = as.character(Listes)) %>% summarise(TOT = sum(Voix)) %>%
  arrange(desc(TOT)) %>% pull(Listes)

Results$Listes = factor(x = Results$Listes, levels = Order)
source(file = "Graphs.R")
