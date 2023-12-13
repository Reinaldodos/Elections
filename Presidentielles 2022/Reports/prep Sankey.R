source(file = "Presidentielles 2022/Fetch_data_bdv.R")
source(file = "Presidentielles 2022/Fetch_data_bdv_T2.R")
# Tripoter l'abstention (a bit) -------------------------------------------
Electorat_stable =
  list(
    output_T1 %>%
      group_by(rowid) %>%
      summarise(Tot_T1 = sum(voix)),
    output_T2 %>%
      group_by(rowid) %>%
      summarise(Tot_T2 = sum(voix))
  ) %>% reduce(.f = inner_join)

DELTA =
  Electorat_stable %>%
  filter(Tot_T1 != Tot_T2) %>%
  mutate(DELTA = Tot_T2 - Tot_T1) %>%
  split(f = .$DELTA > 0)

Abstentions_T2 =
  output_T2 %>%
  filter(candidat == "abstentions") %>%
  inner_join(y = DELTA$`FALSE`) %>%
  mutate(voix = voix - DELTA)
Abstentions_T1 =
  output_T1 %>%
  filter(candidat == "abstentions") %>%
  inner_join(y = DELTA$`TRUE`) %>%
  mutate(voix = voix + DELTA)

output_T1 %<>%
  anti_join(y = Abstentions_T1,
            by = c("rowid", "candidat")) %>%
  bind_rows(Abstentions_T1) %>%
  select(names(output_T1))

output_T2 %<>%
  anti_join(y = Abstentions_T2,
            by = c("rowid", "candidat")) %>%
  bind_rows(Abstentions_T2) %>%
  select(names(output_T2))

Electorat_stable =
  list(
    output_T1 %>%
      group_by(rowid) %>%
      summarise(Tot_T1 = sum(voix)),
    output_T2 %>%
      group_by(rowid) %>%
      summarise(Tot_T2 = sum(voix))
  ) %>% reduce(.f = inner_join) %>%
  filter(Tot_T1 == Tot_T2)


# Inférence écologique ----------------------------------------------------
pacman::p_load(eiPack)
input =
  list("T1" = output_T1,
       "T2" = output_T2) %>%
  bind_rows(.id = "Tour")

input_Sankey =
  input %>%
  tidyr::unite(col = candidat, candidat, Tour) %>%
  group_by(rowid, candidat) %>%
  summarise(voix = sum(voix, na.rm = T), .groups = "drop") %>%
  spread(key = candidat, value = voix, fill = 0)

input_Sankey %<>% janitor::clean_names()
# input_Sankey %<>% semi_join(y = Electorat_stable, by = "rowid")

input_Sankey %<>%
  mutate_at(.vars = vars(contains("_t")),
            .funs = as.integer)

input_Sankey %<>%
  gather(key = Liste, value = Score,-rowid) %>%
  group_by(rowid) %>%
  summarise(N = sum(Score)) %>%
  inner_join(x = Bureaux, by = "rowid") %>%
  filter(N > 0) %>%
  semi_join(x = input_Sankey, by = "rowid")

source(file = "Presidentielles 2022/Reports/sankey_fonctions.R")
