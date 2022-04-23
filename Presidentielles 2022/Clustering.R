
FINAL %<>%
  select(insee,annee, candidat, score) %>%
  mutate_at(.vars = "candidat", .funs = str_to_title)

KOR =
  FINAL %>%
  filter(annee == "2022") %>%
  tidyr::unite(col = candidat, candidat, annee) %>%
  spread(candidat, score) %>%
  column_to_rownames(var = "insee") %>%
  cor(use = "pairwise.complete.obs")

KOR %>% ggcorrplot::ggcorrplot(hc.order = T)

CLUST = KOR %>% dist() %>% hclust()

CLUST %>% ggdendro::ggdendrogram(rotate = T)

Blocs =
  CLUST %>% cutree(k = 7) %>%
  as.data.table(keep.rownames = T) %>%
  rename(Listes = "rn", Groupe = ".") %>%
  arrange(Groupe) %>%
  mutate(
    Bloc = case_when(
      Groupe == 1 ~ "Abstention",
      Groupe == 2 ~ "Contre",
      Groupe == 3 ~ "Droite",
      Groupe == 4 ~ "Gauche",
      Groupe == 5 ~ "Centre",
      Groupe == 6 ~ 'Lassalle',
      Groupe == 7 ~ 'EXD',
      TRUE ~ Listes
    )) %>% as_tibble()

FINAL %<>%
  tidyr::unite(col = Listes, candidat, annee, remove = F) %>%
  inner_join(y = Blocs, by = "Listes", copy = TRUE)

list(
  "2017" = output_2017,
  "2022" = DATA) %>%
  bind_rows(.id = "annee") %>%
  tidyr::unite(col = Listes, candidat, annee, remove = F) %>%
  inner_join(y = Blocs, by = "Listes", copy = TRUE) %>%
  group_by(candidat, annee) %>%
  summarise(voix = sum(voix, na.rm = T)) %>%
  spread(key = annee, value = voix)
