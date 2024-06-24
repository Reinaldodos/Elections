
source(file = "Legislatives 2024/Fonctions.R",
       encoding = "UTF-8")

# choix de la circo -------------------------------------------------------

Donnees =
  "Legislatives 2024/data/donnees.parquet" %>%
  arrow::open_dataset() %>%
  collect()

input =
  Donnees %>%
  tidyr::unite(col = bulletin, scrutin, Tour, candidat) %>%
  tidyr::unite(col = bureau, code_de_la_commune, code_du_b_vote) %>%
  tidyr::unite(col = circo, code_du_departement, code_de_la_circonscription) %>%
  group_by(circo, bureau, bulletin) %>%
  summarise(voix = sum(voix, na.rm = TRUE), .groups = "drop")

MAT =
  input %>%
  filter(bulletin %>%
           str_detect(pattern = "LGS_",
                      negate = TRUE)) %>%
  tidyr::unite(col = bureau, circo, bureau) %>%
  pivot_wider(
    names_from = bulletin,
    values_from = voix,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "bureau") %>%
  janitor::clean_names()

hc =
  MAT %>%
  t() %>%
  scale() %>%
  dist() %>%
  hclust(method = "ward.D")

hc %>%
  ggdendro::ggdendrogram(rotate = T) %>%
  print()

Groupes =
  hc %>%
  get_clusters(Nb_clusters = 4)

Groupes %>%
  split(x = .$bureau, f = .$Groupe)

Vraies_listes =
  Groupes %>%
  filter(Groupe != 2) %>%
  pull(bureau)

KOR =
  MAT %>%
  select(all_of(Vraies_listes)) %>%
  scale %>%
  cor(use = "pairwise.complete.obs")

hc_KOR =
  KOR %>%
  dist() %>%
  hclust(method = "ward.D")

hc_KOR %>%
  ggdendro::ggdendrogram(rotate = T) %>%
  print()

hc_KOR %>%
  as.dendrogram() %>%
  dendextend::set("branches_k_color",
                  k = 7) %>%
  dendextend::plot_horiz.dendrogram()

hc_KOR %>%
  get_clusters(Nb_clusters = 6) %>%
  split(x = .$bureau, f = .$Groupe)

Frances =
  hc_KOR %>%
  get_clusters(Nb_clusters = 6) %>%
  mutate(
    France = case_match(
      .x = Groupe,
      1 ~ "Les bouseux",
      2 ~ "Les centristes",
      3 ~ "Les réacs",
      4 ~ "Les méluches",
      5 ~ "Les fachos",
      6 ~ "Les dépités"
    )
  )

output =
  MAT %>%
  rownames_to_column(var = "bureau") %>%
  pivot_longer(cols = -bureau,
               names_to = "bulletin",
               values_to = "voix") %>%
  filter(voix > 0) %>%
  inner_join(y = Frances,
             by = join_by(bulletin == bureau)) %>%
  tidyr::extract(col = bulletin, into = c("scrutin", "tour", "candidat"),
                 regex = "(.*)_(t[12])_(.*)") %>%
  group_by(scrutin, tour, bureau, France) %>%
  summarise(voix = sum(voix, na.rm = TRUE),
            .groups = "drop")

output %>%
  group_by(scrutin, tour, France) %>%
  summarise(voix = sum(voix, na.rm = TRUE),
            .groups = "drop_last") %>%
  mutate(score = (voix/sum(voix)) %>%
           scales::percent(accuracy = 0.1)) %>%
  ungroup() %>%
  select(-voix) %>%
  tidyr::unite(col = election, scrutin, tour, sep = "_") %>%
  spread(election, score)

output %>%
  tidyr::unite(col = election, scrutin, tour, sep = "_") %>%
  spread(key = election,
         value = voix) %>%
  group_by(bureau) %>%
  mutate(across(.cols = where(is.numeric),
                .fns = function(x) x/sum(x, na.rm = TRUE)
                )
         ) %>%
  view()
