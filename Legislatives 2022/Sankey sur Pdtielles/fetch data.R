pacman::p_load(tidyverse, magrittr, data.table, janitor, readxl)
source(file = "Legislatives 2022/Fetch_data_bdv.R", echo = TRUE)

Abstentions_legislatives =
  output_T1 %>%
  filter(str_to_lower(candidat) == candidat)

Legislatives =
  read_rds(
    file = "Legislatives 2022/Record linkage Le Monde/scores_nuances.rds") %>%
  mutate_at(.vars = "voix", .funs = as.integer)

Bureaux =
  Legislatives %>%
  select(rowid, starts_with("code"), starts_with("libell")) %>%
  distinct()

Legislatives %<>%
  select(rowid, num_panneau,
         nom, prenom, sexe,
         nuance_ministere, nuance_agregee_par_le_monde,
         parti_agrege_par_le_monde, voix)

Legislatives =
  Abstentions_legislatives %>%
  select(rowid, candidat, voix) %>%
  mutate(
    nuance_ministere = candidat,
    nuance_agregee_par_le_monde = candidat,
    parti_agrege_par_le_monde = candidat) %>%
  bind_rows(Legislatives, .) %>%
inner_join(x = Bureaux)

source(file = "Presidentielles 2022/Fetch_data_bdv.R", echo = TRUE)

list(
  "Pdt" = output,
  "Leg" = Legislatives
) %>%
  saveRDS(file = "Legislatives 2022/Sankey sur Pdtielles/donnees.rds")
