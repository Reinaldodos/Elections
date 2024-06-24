data =
  input %>%
  filter(circo == "59_01") %>%
  tidyr::unite(col = bureau, circo, bureau) %>%
  pivot_wider(
    names_from = bulletin,
    values_from = voix,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "bureau") %>%
  janitor::clean_names()

data %>%
  cor(use = "pairwise.complete.obs") %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  # get_clusters(Nb_clusters = 7) %>%
  # split(x = .$bureau, f = .$Groupe)
  ggdendro::ggdendrogram(rotate = T) %>%
  print()

NMF::nmf(x = data, rank = 1:8) %>%
  plot()

result = NMF::nmf(x = data,
                  rank = 3)

summary(result)
basismap(result)
coefmap(result)

coef(result) %>%
  as_tibble() %>%
  rowid_to_column() %>%
  pivot_longer(cols = -rowid) %>%
  filter(value > .01) %>%
  pivot_wider(names_from = name,
              values_from = value,
              names_sort = TRUE,
              values_fn = ~ scales::percent(x =., accuracy = .1)
              ) %>%
  view()

Clusters_Bureaux =
  basis(result) %>%
  as.data.frame() %>%
  rownames_to_column(var = "bureau") %>%
  pivot_longer(cols = -bureau) %>%
  group_by(bureau) %>%
  filter(value >= max(value)) %>%
  spread(name, value) %>%
  tidyr::separate(
    col = bureau,
    into = c("code_du_departement",
             "code_de_la_circonscription",
             "code_de_la_commune",
             "code_du_b_vote"),
    sep = "_") %>%
  inner_join(x = Bureaux)

Clusters_Bureaux %>%
  drop_na(V1) %>%
  anti_join(x = Clusters_Bureaux) %>%
  view()


