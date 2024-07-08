source(file = "Generalisation reports/Fonctions.R",
       encoding = "UTF-8")


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
  tidyr::unite(col = election, scrutin, Tour,
               remove = FALSE) %>%
  mutate(across(.cols = election,
                .fns = janitor::make_clean_names))

Reports =
  tidyr::crossing(Scrutins, Scrutins,
                  .name_repair = "unique") %>%
  janitor::clean_names() %>%
  filter(date_1 < date_5) %>%
  select(source = election_2, target = election_6)

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

input %>%
  count(source, target)

Stock =
  "Generalisation reports/Stock/" %>%
  arrow::open_dataset(
    format = "arrow",
    schema = arrow::schema(
      scrutin_source = arrow::string(),
      scrutin_target = arrow::string(),
      code_du_departement = arrow::string(),
      code_de_la_circonscription = arrow::string()
    )
  )

input =
  Stock %>%
  distinct(scrutin_source,
           scrutin_target,
           code_du_departement,
           code_de_la_circonscription) %>%
  collect() %>%
  anti_join(
    x = input,
    by = join_by(
      code_du_departement,
      code_de_la_circonscription,
      source == scrutin_source,
      target == scrutin_target
    )
  )

input %>%
  count(source, target)

while (nrow(input) > 0) {
  output =
    input %>%
    sample_n(min(50, nrow(input))) %>%
    mutate(output = pmap(
      .f = redresser_donnees,
      .l = list(data_source = data_source,
                data_target = data_target),
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
      ),
      existing_data_behavior = "delete_matching"
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
  mutate(code_de_la_circonscription =
           code_de_la_circonscription %>%
           sprintf(fmt = "%02d"))

Reports %>%
  filter(code_du_departement == "33",
         code_de_la_circonscription == "02") %>%
  SANKEY_natio(FROM = "lgs_t2",
               TO = "lgs_ant_t1")

Reports %>%
  filter(scrutin_target %>%
           str_detect(pattern = "lgs_ant"),
         scrutin_source == "lgs_t1") %>%
  split(f = str_c(.$code_du_departement,
                  .$code_de_la_circonscription)) %>%
  openxlsx::write.xlsx(
    asTable = TRUE,
    file = "Legislatives 2024/reports lgs par circo.xlsx")

Reports %>%
  filter(scrutin_target %>%
           str_detect(pattern = "lgs_ant"),
         scrutin_source == "pdt_t1") %>%
  split(f = str_c(.$code_du_departement,
                  .$code_de_la_circonscription)) %>%
  openxlsx::write.xlsx(
    asTable = TRUE,
    file = "Legislatives 2024/reports pdt par circo.xlsx")
