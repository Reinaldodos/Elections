MAT =
  input %>%
  distinct(Commune, `Liste des candidatures par suffrages exprimés`, Score) %>%
  spread(`Liste des candidatures par suffrages exprimés`, Score) %>%
  janitor::clean_names() %>%
  column_to_rownames(var = "commune")


hc =
  MAT %>%
  t() %>%
  scale() %>%
  dist() %>%
  hclust(method = "ward.D")

pacman::p_load(ggdendro)
hc %>%
  ggdendrogram(rotate = T) %>%
  print()

pacman::p_load(dendextend)

hc %>%
  as.dendrogram() %>%
  set("branches_k_color", k = 6) %>%
  plot_horiz.dendrogram()

KOR =
  MAT %>%
  scale %>%
  cor(use = "pairwise.complete.obs")

pacman::p_load(ggcorrplot)
KOR %>%
  ggcorrplot(hc.order = T)

hc_KOR =
  KOR %>%
  dist() %>%
  hclust(method = "ward.D")

hc_KOR %>%
  ggdendrogram(rotate = T) %>%
  print()

pacman::p_load(psych, GPArotation)

fa.parallel(x = KOR,
            n.obs = nrow(MAT),
            fa = "pc")

Groupes =
  hc %>%
  cutree(k = 6) %>%
  data.table::as.data.table(keep.rownames = T) %>%
  rename(Listes = "rn", Groupe = ".")


Frances =
  Groupes %>%
  split(x = .$Listes, f = .$Groupe)

outpt =
  MAT %>%
  rownames_to_column(var = "bulletin") %>%
  pivot_longer(
    cols = -bulletin,
    names_to = "circo",
    values_to = "voix",
    values_drop_na = TRUE
  ) %>%
  tidyr::separate(col = bulletin,
                  into = c("candidat", "scrutin"),
                  sep = "_") %>%
  left_join(y = Groupes,
            by = join_by(circo == Listes)) %>%
  group_by(scrutin, Groupe, candidat) %>%
  summarise(voix = sum(voix, na.rm = TRUE)) %>%
  mutate(Score = voix/sum(voix)) %>%
  ungroup()

outpt %>%
  filter(Score > 5/100) %>%
  semi_join(x = outpt,
            by = join_by(candidat, scrutin)) %>%
  select(-voix) %>%
  # spread(Groupe, Score) %>%
  ggplot(mapping = aes(y = candidat, x = Score,
                       fill = scrutin))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(facets = ~ Groupe)

Frances$`6` %>%
  enframe(value = "circo") %>%
  tidyr::extract(col = circo,
                  into = c("libelle_circo", "departement"),
                  regex = "x(.*circonscription)_de_(.*)",
                  remove = FALSE) %>%
  view()
