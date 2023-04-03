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

new_absT1 =
  Electorat_stable %>%
  filter(Tot_T1!=Tot_T2) %>%
  mutate(DELTA = Tot_T2-Tot_T1) %>%
  inner_join(x = output_T1) %>%
  filter(candidat == "abstentions") %>%
  mutate(voix  = voix + DELTA) %>%
  select(names(output_T1))

output_T1 %<>%
  anti_join(y = new_absT1,
            by = c("rowid", "candidat")) %>%
  bind_rows(new_absT1)

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
  list(
    "T1" = output_T1,
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

source(file = "Presidentielles 2022/Reports/sankey_fonctions.R")

