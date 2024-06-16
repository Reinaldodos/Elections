
require(tidyverse)

get_CAH <- function(data) {
  MAT =
    data %>%
    spread(key = bureau,
           value = voix,
           fill = 0) %>%
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
    cutree(k = Nb_clusters) %>%
    data.table::as.data.table(keep.rownames = T) %>%
    rename(bureau = "rn", Groupe = ".")

  return(Groupes)
}

clusters_to_JSON <- function(Groupes, input_circo, path, ...) {
  Groupes %>%
    tidyr::separate(
      col = bureau,
      into = c("code_de_la_commune", "code_du_b_vote"),
      sep = "_"
    ) %>%
    inner_join(x = input_circo %>%
                 distinct(code_de_la_commune, libelle_de_la_commune, code_du_b_vote)) %>%
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
    semi_join(x = data_score, by = join_by(scrutin, tour, candidat)) %>%
    ggplot(mapping = aes(
      fill = as_factor(Groupe),
      x = score,
      y = candidat
    )) +
    # facet_wrap(facets = ~Groupe)+
    geom_bar(stat = "identity", position = "dodge") +
    theme(legend.position = "bottom") +
    facet_wrap(facets = ~ scrutin + tour, scales = "free_y")

  return(PLOTT)
}
