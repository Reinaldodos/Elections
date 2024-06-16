pacman::p_load(tidyverse)


# ELT données -------------------------------------------------------------

Donnees =
  list(PDT = "Presidentielles 2022/resultats.rds",
       LGS = "Legislatives 2022/resultats_raw.rds",
       EURO = "Europeennes 2024/output/resultats.rds") %>%
  map(.f = read_rds)

Communes_circo =
  "Legislatives 2024/bureaux-de-vote-circonscriptions.csv"  %>%
  rio::import() %>%
  transmute(
    code_du_departement = codeDepartement %>%
      str_replace_all(pattern = "99", replacement = "ZZ"),
    code_de_la_circonscription = codeCirconscription %>%
      str_sub(start = -2),
    code_de_la_commune = codeCommune %>%
      str_sub(start = -3),
    code_du_b_vote = codeBureauVote %>%
      str_sub(start = -4)
  )

Donnees$EURO =
  Communes_circo %>%
  distinct() %>%
  inner_join(x = Donnees$EURO,
             by = join_by(code_du_departement,
                          code_de_la_commune,
                          code_du_b_vote))

input =
  Donnees %>%
  bind_rows(.id = "scrutin") %>%
  select(-score, -rowid)


# choix de la circo -------------------------------------------------------

circo = "5904"

input_circo =
  input %>%
  filter(
    code_du_departement == str_sub(string = circo, end = 2),
    code_de_la_circonscription == str_sub(string = circo, start = 3)
    )

data =
  input_circo %>%
  tidyr::unite(col = bulletin, scrutin, Tour, candidat) %>%
  tidyr::unite(col = bureau, code_de_la_commune, code_du_b_vote) %>%
  group_by(bulletin, bureau) %>%
  summarise(voix = sum(voix, na.rm = TRUE),
            .groups = "drop")

MAT =
  data %>%
  spread(key = bureau, value = voix, fill = 0) %>%
  column_to_rownames(var = "bulletin")

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

return(hc_KOR)

hc_KOR %>%
  ggdendro::ggdendrogram(rotate = T) %>%
  print()



Nb_clusters = 5

hc_KOR %>%
  as.dendrogram() %>%
  dendextend::set("branches_k_color",
                  k = Nb_clusters) %>%
  dendextend::plot_horiz.dendrogram()

Groupes =
  hc_KOR %>%
  cutree(k = Nb_clusters) %>%
  data.table::as.data.table(keep.rownames = T) %>%
  rename(bureau = "rn", Groupe = ".")

Groupes %>%
  tidyr::separate(col = bureau,
                  into = c("code_de_la_commune",
                           "code_du_b_vote"),
                  sep = "_") %>%
  inner_join(x = input_circo %>%
               distinct(code_de_la_commune,
                        libelle_de_la_commune, code_du_b_vote)
  ) %>%
  group_nest(Groupe, code_de_la_commune,
             libelle_de_la_commune,
             .key = "bureaux") %>%
  group_nest(Groupe,
             .key = "commune") %>%
  jsonlite::toJSON(pretty = TRUE)


data_groupe =
  data %>%
  left_join(y = Groupes,
            by = join_by(bureau))

data_score =
  data_groupe %>%
  tidyr::separate(col = bulletin,
                  into = c("scrutin", "tour", "candidat"),
                    sep ="_") %>%
  group_by(Groupe, scrutin, tour, candidat) %>%
  summarise(voix = sum(voix), .groups = "drop_last") %>%
  mutate(score = voix / sum(voix, na.rm = TRUE)) %>%
  select(-voix)
  # filter(
  #   (bulletin %>%
  #     str_detect(pattern = "abstent")&
  #   bulletin %>%
  #     str_detect(pattern = "EURO"))
  #   |
  #   bulletin %>%
  #     str_detect(pattern = "LENCHON")
  #   |
  #   bulletin %>%
  #     str_detect(pattern = "NUP")
  #   ) %>%

data_score %>%
  filter(score > .05) %>%
  semi_join(x = data_score,
            by = join_by(scrutin, tour, candidat)) %>%
ggplot(mapping = aes(fill = as_factor(Groupe),
                       x = score,
                       y = candidat)) +
  # facet_wrap(facets = ~Groupe)+
  geom_bar(stat = "identity",
           position = "dodge") +
  theme(legend.position = "bottom")+
  facet_wrap(facets = ~ scrutin + tour,
             scales = "free_y")


