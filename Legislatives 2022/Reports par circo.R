source(file = "Legislatives 2022/prep Sankey.R")

DATA =
  input %>%
  group_nest(libelle_du_departement,
             libelle_de_la_circonscription)

require(furrr)
plan(strategy = multisession,
     workers = future::availableCores())

DATA %<>%
  mutate(input_sankey = future_map(.x = data,
                                   .f = get_input_sankey))

DATA %<>%
  mutate(Rapports =
           future_map(.x = input_sankey,
                      .f = safe_Report))

plan(strategy = sequential)

Reports =
  DATA %>%
  mutate(results = map(.x = Rapports, .f = ~ .$result)) %>%
  unnest(cols = "results") %>%
  select(libelle_du_departement,
         libelle_de_la_circonscription,
         T1,
         T2,
         report)

saveRDS(object = Reports,
        file = "Legislatives 2022/Reports_circo.rds")
