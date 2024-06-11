require(tidyverse)
conn = DBI::dbConnect(drv = duckdb::duckdb())

Donnees =
  conn %>%
  dplyr::tbl("read_parquet('Europeennes 2024/*.parquet')") %>%
  collect()

input =
  Donnees %>%
  mutate(Voix = Voix %>%
           str_remove_all(pattern = "[^[0-9]]") %>%
           as.numeric()) %>%
  group_by(Commune) %>%
  mutate(Score = Voix / sum(Voix, na.rm = TRUE)) %>%
  ungroup()


Abstention =
  Donnees %>%
  type.convert(dec=",", ) %>%
  transmute(Commune, Abstention = 1 - `% Inscrits` / `% Exprimés`) %>%
  distinct() %>%
  drop_na() %>%
  group_by(Commune) %>%
  summarise(Abstentions = mean(Abstention, na.rm = TRUE))

input =
  input %>%
  filter(`Liste des candidatures par suffrages exprimés` != "Abstentions") %>%
  group_by(Commune) %>%
  summarise(Exprimés = sum(Voix, na.rm = TRUE)) %>%
  inner_join(y = Abstention, by = join_by(Commune)) %>%
  mutate(
    Inscrits = Exprimés / (1 - Abstentions),
    Abstentions = (Inscrits - Exprimés) %>% round()
  ) %>%
  select(Commune, Abstentions) %>%
  pivot_longer(cols = Abstentions,
               names_to = "Liste des candidatures par suffrages exprimés",
               values_to = "Voix") %>%
  bind_rows(input %>%
              filter(`Liste des candidatures par suffrages exprimés` != "Abstentions")) %>%
  discard(.p = ~ any(is.na(.)))%>%
  group_by(Commune) %>%
  mutate(Score = Voix / sum(Voix, na.rm = TRUE)) %>%
  ungroup()


La_Gauche =
  c(
    "RÉVEILLER L'EUROPE",
    "LA FRANCE INSOUMISE - UNION POPULAIRE",
    "EUROPE ÉCOLOGIE",
    "GAUCHE UNIE POUR LE MONDE DU TRAVAIL SOUTENUE PAR FABIEN ROUSSEL"
  )


input_NUPES =
  input %>%
  filter(`Liste des candidatures par suffrages exprimés` %in% La_Gauche) %>%
  group_by(Commune) %>%
  summarise(Voix = sum(Voix, na.rm = TRUE)) %>%
  mutate(`Liste des candidatures par suffrages exprimés` = "GAUCHE") %>%
  bind_rows(input %>%
              filter(!`Liste des candidatures par suffrages exprimés` %in% La_Gauche))


