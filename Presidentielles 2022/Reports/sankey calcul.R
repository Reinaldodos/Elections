pacman::p_load(dendextend)
Circos =
  Bureaux %>%
  transmute(
    code_du_departement,
    code_de_la_circonscription,
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
  transmute(candidat, score = voix / sum(voix)) %>%
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
  CLUST %>% cutree(k = 16) %>%
  data.frame(Groupe = .) %>%
  rownames_to_column(var = "Circo") %>%
  inner_join(x = Circos)

Circos_clust %>% split(x = .$Circo, f = .$Groupe)

Circos_clust %>%
  inner_join(y = input, multiple = "all") %>%
  group_by(Groupe, Tour) %>%
  summarise(Inscrits = sum(voix),
            .groups = "drop") %>%
  pivot_wider(names_from = Tour,
              values_from = Inscrits) %>%
  arrange(-T1)

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
  mutate(Sankey = future_map(.x = data,
                             .f = Get_reports))

plan(strategy = sequential)

Sankey_clus %<>%
  mutate(Sankey_plot = map(.x = Sankey,
                           .f = Get_Sankey))


Reports =
  Sankey_clus %>%
  unnest(cols = "Sankey") %>%
  select(Groupe, T1, T2, report, se, voix) %>%
  mutate(low = report + se * qnorm(p = 2.5 / 100),
         high = report + se * qnorm(p = 97.5 / 100))

Reports %<>%
  distinct(Groupe, T1, voix) %>%
  group_by(Groupe) %>%
  mutate(score = voix / sum(voix)) %>%
  ungroup() %>%
  inner_join(x = Reports)

Reports %>%
  filter(score > 0.05,
         low > 0,
         report > 0.1) %>%
  ggplot(mapping = aes(y = as.factor(Groupe))) +
  geom_errorbarh(mapping = aes(xmin = low, xmax = high)) +
  facet_grid(facets = T1 ~ T2)

Reports %>%
  filter(str_remove_all(string = T1, "t1") ==
           str_remove_all(string = T2, "t2")) %>%
  view()

Reports %>%
  transmute(Groupe, T1, T2,
            VAR = scales::percent(report, accuracy = .1)) %>%
  spread(Groupe, VAR) %>% view()

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
