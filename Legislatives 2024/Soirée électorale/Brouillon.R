
Bilan %>%
  count(elu_e, Duels) %>%
  spread(elu_e, n, fill = 0) %>%
  janitor::adorn_totals(where = c("row", "col"))

Triangulaires =
  Bilan %>%
  filter(Duels %>%
           str_detect(pattern = "gulaires$")) %>%
  semi_join(x = input,
            by = join_by(elu_e, Departement, circo)) %>%
  group_by(Departement, circo) %>%
  mutate(rank = dense_rank(-voix)) %>%
  ungroup()

Triangulaires_RN =
  Triangulaires %>%
  filter(nuance %in% c("RN", "UXD", "EXD", "REC"),
         rank == 1) %>%
  distinct(Departement, circo) %>%
  semi_join(x = Triangulaires,
            by = join_by(Departement, circo))

Triangulaires_RN %>%
  select(Departement, circo, nuance, rank) %>%
  filter(rank %in% 2:3) %>%
  arrange(rank) %>%
  summarise(order = paste(nuance, collapse = " devant "),
            .by = c(Departement, circo)
  ) %>%
  count(
    "NFP se désiste" = order %>%
          str_detect(pattern = "devant UG"),
    "désistement pour NFP" =
        order %>%
          str_detect(pattern = "UG devant")) %>%
  arrange(-n) %>%
  janitor::adorn_totals()

Triangulaires_RN %>%
  filter(nuance == "UG",
         rank == 2) %>%
  distinct(circo) %>%
  tidyr::separate(col = circo, into = c("circo", "TOTO"), sep = " - ") %>%
  pull(circo)

Triangulaires_situ =
  Triangulaires %>%
  group_by(circo) %>%
  arrange(rank) %>%
  summarise(situ = paste(nuance, collapse = " -> "),
            .groups = "drop")



Triangulaires_RN %>%
  select(circo) %>%
  semi_join(x = input, by = "circo") %>%
  summarise(across(.cols=percent_exprimes, .fns=sum),
            .by = c(circo, nuance)
  )%>%
  spread(nuance, percent_exprimes, fill = 0) %>%
  mutate(
    Bardella = RN + UXD + REC,
    Macron = ENS + HOR
  ) %>%
  select(-RN, -UXD, -ENS, -HOR, -REC) %>%
  inner_join(x = Triangulaires_situ) %>%
  tidyr::separate(col = circo,
                  into = c("circo", "libelle_circo"),
                  sep = " - ") %>%
  inner_join(x = Etiquettes) %>%
  view()


