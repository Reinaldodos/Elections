# Preprocess data
processed_data <- 
  input %>%
  filter(circo == "59_01") %>%
  tidyr::unite(col = bureau_id, circo, bureau) %>%
  pivot_wider(
    names_from = bulletin,
    values_from = voix,
    values_fill = 0
  ) %>%
  column_to_rownames(var = "bureau_id") %>%
  janitor::clean_names()

# Perform hierarchical clustering and plot dendrogram
processed_data %>%
  cor(use = "pairwise.complete.obs") %>%
  dist() %>%
  hclust(method = "ward.D") %>%
  ggdendro::ggdendrogram(rotate = TRUE) %>%
  print()

# Perform NMF for a range of ranks and plot results
NMF::nmf(x = processed_data, rank = 1:8) %>%
  plot()

# Function to perform NMF and return results
perform_nmf <- function(data_matrix, rank_value) {
  nmf_result <- NMF::nmf(x = data_matrix, rank = rank_value)
  summary(nmf_result)
  basismap(nmf_result)
  coefmap(nmf_result)
  return(nmf_result)
}

# Function to process NMF coefficients
process_coefficients <- function(nmf_result) {
  coef(nmf_result) %>%
    as_tibble() %>%
    rowid_to_column(var = "bureau_id") %>%
    pivot_longer(cols = -bureau_id) %>%
    filter(value > 0.01) %>%
    pivot_wider(
      names_from = name,
      values_from = value,
      names_sort = TRUE,
      values_fn = ~ scales::percent(x = ., accuracy = 0.1)
    ) %>%
    view()
}

# Main script
nmf_result <- perform_nmf(processed_data, rank_value = 3)
process_coefficients(nmf_result)

# Process clustering results
clustered_bureaux <- basis(nmf_result) %>%
  as.data.frame() %>%
  rownames_to_column(var = "bureau_id") %>%
  pivot_longer(cols = -bureau_id, names_to = "cluster_id", values_to = "cluster_value") %>%
  group_by(bureau_id) %>%
  filter(cluster_value == max(cluster_value)) %>%
  tidyr::separate(
    col = bureau_id,
    into = c("department_code", "circonscription_code", "commune_code", "vote_bureau_code"),
    sep = "_"
  ) %>%
  inner_join(x = Bureaux, by = "bureau_id")

# Filter and view results
clustered_bureaux %>%
  drop_na(V1) %>%
  anti_join(x = clustered_bureaux) %>%
  view()


