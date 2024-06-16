
require(tidyverse)

get_CAH <- function(data) {
  MAT =
    data %>%
    pivot_wider(
      names_from = bureau,
      values_from = voix,
      values_fill = 0) %>%
    column_to_rownames(var = "bulletin")

  KOR =
    MAT %>%
    scale %>%
    cor(use = "pairwise.complete.obs")

  pacman::p_load(ggcorrplot)
  KOR %>%
    ggcorrplot::ggcorrplot(hc.order = T)

  hc_KOR =
    KOR %>%
    dist() %>%
    hclust(method = "ward.D")

  hc_KOR %>%
    ggdendro::ggdendrogram(rotate = T) %>%
    print()

  return(hc_KOR)
}

get_clusters <- function(hc_KOR, Nb_clusters) {
  hc_KOR %>%
    as.dendrogram() %>%
    dendextend::set("branches_k_color", k = Nb_clusters) %>%
    dendextend::plot_horiz.dendrogram()

  Groupes =
    hc_KOR %>%
    dendextend::cutree(k = Nb_clusters) %>%
    data.table::as.data.table(keep.rownames = T) %>%
    dplyr::rename(bureau = "rn", Groupe = ".")

  return(Groupes)
}

get_clusters_libelles <- function(Groupes, input_circo) {
  Groupes %>%
    tidyr::separate(
      col = bureau,
      into = c("code_de_la_commune", "code_du_b_vote"),
      sep = "_"
    ) %>%
    inner_join(
      x = input_circo %>%
        distinct(code_de_la_commune, libelle_de_la_commune, code_du_b_vote),
      by = join_by(code_de_la_commune, code_du_b_vote)
    )
}

clusters_to_JSON <- function(data, path, ...) {
  data %>%
    group_nest(Groupe, code_de_la_commune, libelle_de_la_commune, .key = "bureaux") %>%
    group_nest(Groupe, .key = "commune") %>%
    jsonlite::write_json(path = path, ..., pretty = TRUE)
}

get_data_score <- function(data, Groupes) {
  data_groupe =
    data %>%
    left_join(y = Groupes, by = join_by(bureau))

  data_score =
    data_groupe %>%
    tidyr::separate(
      col = bulletin,
      into = c("scrutin", "tour", "candidat"),
      sep = "_"
    ) %>%
    group_by(Groupe, scrutin, tour, candidat) %>%
    summarise(voix = sum(voix), .groups = "drop_last") %>%
    mutate(score = voix / sum(voix, na.rm = TRUE)) %>%
    select(-voix)

  return(data_score)
}

get_plot_groupes <- function(data_score) {
  PLOTT =
    data_score %>%
    filter(score > .05) %>%
    semi_join(x = data_score,
              by = join_by(scrutin, tour, candidat)) %>%
    ggplot(mapping = aes(
      fill = as_factor(Groupe),
      x = score,
      y = candidat
    )) +
    labs(title = "Score des listes selon les clusters",
         subtitle = "en fonction des scrutins",
         fill = "Cluster",
         x = "Score (en % des inscrits)")+
    geom_bar(stat = "identity", position = "dodge") +
    theme(legend.position = "bottom") +
    scale_y_discrete(limits = rev) +
    scale_fill_viridis_d(option = "H") +
    facet_wrap(facets = ~ scrutin + tour, scales = "free")

  return(PLOTT)
}
