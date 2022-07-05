source(file = "Legislatives 2022/prep Sankey.R")
Circos =
  Bureaux %>%
  transmute(
    code_du_departement, code_de_la_circonscription,
    Circo = paste(libelle_de_la_circonscription,
                  " de ",
                  libelle_du_departement)
  ) %>% distinct() %>%
  arrange(code_du_departement, code_de_la_circonscription)

Sankey_clus =
  Circos %>%
  inner_join(x = Bureaux) %>%
  select(Circo, rowid) %>%
  inner_join(y = input_Sankey) %>%
  group_nest(Circo)

input_Sankey %>% Get_safe_report(size = .5)
# Sankey_clus %>% pluck("data", 1) %>% Get_reports()

require(furrr)
plan(strategy = multisession,
     workers = future::availableCores())

Sankey_clus %<>%
  mutate(Sankey = future_map(
    .x = data,
    .f = Get_safe_report, size = .5))

plan(strategy = sequential)

Sankey_clus %<>%
  mutate(Sankey_plot = map(.x = Sankey,
                           .f = Get_Sankey))

