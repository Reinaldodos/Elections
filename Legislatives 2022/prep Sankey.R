source(file = "Legislatives 2022/Fetch_data_bdv.R")
source(file = "Legislatives 2022/Fetch_data_bdv_T2.R")
# Tripoter l'abstention (a bit) -------------------------------------------
input =
  list(
    "T1" = output_T1,
    "T2" = output_T2) %>%
  bind_rows(.id = "Tour")

Bureaux =
  input %>%
  select(starts_with("code")) %>%
  distinct() %>%
  rowid_to_column()

input %<>%
  select(-rowid) %>%
  inner_join(x = Bureaux)

Electorat_stable =
  input %>%
  group_by(Tour, rowid) %>%
  summarise(voix = sum(voix)) %>%
  pivot_wider(names_from = Tour, values_from = voix, names_prefix = "Tot_")

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
Bureaux =
  input %>%
  select(starts_with("code")) %>%
  distinct() %>%
  rowid_to_column()

input %<>%
  select(-rowid) %>%
  inner_join(x = Bureaux)




source(file = "Legislatives 2022/sankey_fonctions.R")

