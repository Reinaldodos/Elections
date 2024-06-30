source(file = "Generalisation reports/Fonctions.R", encoding = "UTF-8")

# choix de la circo -------------------------------------------------------

Donnees =
  "Donnees parquet/" %>%
  arrow::open_dataset(format = "arrow") %>%
  select(-starts_with("libelle")) %>%
  collect() %>%
  group_nest(annee,
             date,
             scrutin,
             Tour,
             code_du_departement,
             code_de_la_circonscription)

Scrutins =
  Donnees %>%
  distinct(date, scrutin, Tour) %>%
  collect() %>%
  tidyr::unite(col = election, scrutin, Tour, remove = FALSE) %>%
  mutate(across(.cols = election, .fns = janitor::make_clean_names))

Reports =
  tidyr::crossing(Scrutins, Scrutins, .name_repair = "unique") %>%
  janitor::clean_names() %>%
  filter(date_1 < date_5) %>%
  select(source = election_2, target = election_6)


process_reports <- function(source) {
  data_source_name <- paste0("data_", source)

  Reports %>%
    select({{ source }}) %>%
    inner_join(y = Scrutins, by = join_by({{ source }} == election)) %>%
    left_join(
      y = Donnees,
      by = join_by(date, scrutin, Tour),
      relationship = "many-to-many"
    ) %>%
    select(-scrutin, -Tour, -date, -annee) %>%
    rename(!!data_source_name := data)
}

input =
  names(Reports) %>%
  map(.f = process_reports) %>%
  map(.f = left_join, x = Reports) %>%
  reduce(
    .f = inner_join,
    by = join_by(
      source,
      target,
      code_du_departement,
      code_de_la_circonscription
    )
  ) %>%
  distinct()

Stock =
  "Generalisation reports/Stock/" %>%
  arrow::open_dataset(
    format = "arrow",
    schema = arrow::schema(
      scrutin_source = arrow::string(),
      scrutin_target = arrow::string(),
      code_du_departement = arrow::string(),
      code_de_la_circonscription = arrow::string())
    )

input =
  Stock %>%
  distinct(scrutin_source,
           scrutin_target,
           code_du_departement,
           code_de_la_circonscription) %>%
  collect() %>%
  anti_join(x = input)

while(nrow(input) > 0) {
  output =
    input %>%
    sample_n(min(50, nrow(input))) %>%
    mutate(output = pmap(
      .f = redresser_donnees,
      .l = list(data_source = data_source, data_target = data_target),
      .progress = TRUE
    )) %>%
    mutate(regression = output %>%
             map(
               .f = purrr::safely(get_reg_ecolo),
               .progress = TRUE
             ))  %>%
    mutate(ERROR = regression %>%
             map(.f = ~ !is_null(.$error))) %>%
    unnest(cols = c(ERROR))

  output %>%
    filter(!ERROR) %>%
    mutate(safe_reg = regression %>%
             map(.f = ~ .$result)) %>%
    mutate(coeff = safe_reg %>%
             map(.f = extract_coefficients)) %>%
    mutate(reports = pmap(
      .f = append_report,
      .l = list(data = output, coefficients = coeff)
    )) %>%
    select(
      scrutin_source = source,
      scrutin_target = target,
      code_du_departement,
      code_de_la_circonscription,
      reports
    ) %>%
    unnest(cols = c(reports)) %>%
    arrow::write_dataset(
      path = "Generalisation reports/Stock",
      format = "arrow",
      partitioning = c(
        "code_du_departement",
        "code_de_la_circonscription",
        "scrutin_source",
        "scrutin_target"
      )
    )

  input =
    input %>%
    anti_join(
      y = output,
      by = join_by(
        source,
        target,
        code_du_departement,
        code_de_la_circonscription
      )
    )
}

Reports =
  "Generalisation reports/Stock/" %>%
  arrow::open_dataset(format = "arrow") %>%
  collect()  %>%
  group_by(scrutin_source,
           scrutin_target,
           code_du_departement,
           code_de_la_circonscription) %>%
  filter(100 * REPORT > sum(REPORT)) %>%
  ungroup()

Reports %>%
  filter(source %>%
           str_detect(pattern = "melenchon")) %>%
  ggplot(mapping = aes(x = report,
                       y = target)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(facets = ~ scrutin_source + scrutin_target,
             scales = "free_y") +
  lims(x = c(0,1))+
  theme(legend.position = "bottom")

