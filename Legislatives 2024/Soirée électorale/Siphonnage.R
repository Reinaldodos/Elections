source(file = "Legislatives 2024/Soirée électorale/Fonctions.R", encoding = "UTF-8")

input =
  "Legislatives 2024/Soirée électorale/liste circos.rds" %>%
  read_rds()

while (nrow(input) > 0) {
  input =
    "Legislatives 2024/Soirée électorale/Stock/" %>%
    arrow::open_dataset(format = "arrow") %>%
    filter(elu_e != "NON") %>%
    anti_join(x = input,
              copy = TRUE,
              by = join_by(Departement, circo, url))

  Resultats =
    input %>%
    mutate(data = url %>%
             map(.f = safely(get_tables), .progress = TRUE)) %>%
    mutate(ERROR = data %>%
             map(.f = ~ !is_null(.$error))) %>%
    unnest(cols = c(ERROR)) %>%
    filter(!ERROR)

  Resultats %>%
    mutate(resultats = data %>%
             map(.f = ~ .$result$Resultats)) %>%
    select(Departement, circo, url, resultats) %>%
    unnest(cols = c(resultats)) %>%
    arrow::write_dataset(
      path = "Legislatives 2024/Soirée électorale/Stock/",
      format = "arrow",
      partitioning = c("Departement", "circo")
    )
}
