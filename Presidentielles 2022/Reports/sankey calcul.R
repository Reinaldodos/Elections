
Circos =
  Bureaux %>%
  transmute(
    code_du_departement, code_de_la_circonscription,
    Circo = paste(libelle_de_la_circonscription,
                  " de ",
                  libelle_du_departement)
  ) %>% distinct() %>%
  arrange(code_du_departement, code_de_la_circonscription)

Scores_circo =
  input %>%
  inner_join(y = Circos) %>%
  group_by(Circo, Tour, candidat) %>%
  summarise(voix = sum(voix, na.rm = TRUE)) %>%
  transmute(candidat, score = voix/sum(voix)) %>%
  ungroup()  %>%
  tidyr::unite(col = candidat, candidat, Tour) %>%
  spread(Circo, score, fill = 0)

CLUST =
  Scores_circo %>%
  select(-candidat) %>%
  cor() %>%
  dist() %>% hclust(method = "ward.D2")
CLUST %>% dendextend::plot_horiz.dendrogram()

Circos_clust =
  CLUST %>% cutree(k = 11) %>%
  data.frame(Groupe = .) %>%
  rownames_to_column(var = "Circo") %>%
  inner_join(x = Circos)


Sankey_clus =
  Circos_clust %>%
  inner_join(x = Bureaux) %>%
  select(Groupe, rowid) %>%
  inner_join(y = input_Sankey) %>%
  group_nest(Groupe) %>%
  inner_join(x = Circos_clust %>%
               group_nest(Groupe,
                          .key = "Bureaux"))

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

Sankey_clus$Sankey_plot[[5]]
Circos_clust %>% split(x = .$Circo, f = .$Groupe)

TEST =
  Sankey_clus %>%
  unnest(cols = "Sankey") %>%
  select(Groupe, T1, T2, VAR = REPORT_voix)

TEST %>%
  group_by(T1, T2) %>%
  summarise(Report = sum(VAR), .groups = "drop") %>%
  spread(T2, Report, fill = 0) %>%
  mutate_if(.predicate = is.numeric, .funs = as.integer) %>%
  janitor::adorn_totals(where = c("row", "col"))

