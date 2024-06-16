
Legislatives =
  "Legislatives 2022/resultats_raw.rds" %>%
  read_rds()

get_formula <- function(data) {
  noms = names(data)
  EURO = str_subset(string = noms, pattern = "^euro") %>% paste(collapse = ", ")
  PDT = str_subset(string = noms, pattern = "^lgs") %>% paste(collapse = ", ")
  FORMULE = str_c("cbind(",
                  EURO,
                  ") ~ cbind(",
                  PDT,  ")") %>% as.formula()

  return(FORMULE)
}

filter_mismatch <- function(data) {
  mismatch =
    data %>%
    group_by(code_de_la_commune, code_du_b_vote, scrutin) %>%
    summarise(inscrits = sum(voix), .groups = "drop") %>%
    spread(key = scrutin, value = inscrits, fill = 0) %>%
    filter(EURO != LGS)

  if (nrow(mismatch) > 0) {
    data =
      mismatch %>%
      anti_join(x = data,
                by = join_by(code_de_la_commune, code_du_b_vote))
  }

  return(data)
}

dedoublonner_nuances <- function(data) {
  data %>%
    group_by(code_de_la_commune, code_du_b_vote, scrutin, candidat) %>%
    summarise(voix = sum(voix), .groups = "drop") %>%
    return()
}

mini_chaine = purrr::compose(
  filter_mismatch, dedoublonner_nuances,
  pivot_sankey, get_reg_ecolo, extract_coefficients,
  .dir = "forward")


Blois =
  Legislatives %>%
  # filter(code_du_departement == "41",
  #        code_de_la_circonscription == "01") %>%
  split(f = .$Tour)

EURO_blois =
  Blois$T1 %>%
  semi_join(x = Europeennes,
            by = join_by(code_du_departement, code_de_la_commune, code_du_b_vote))


input =
  list(
    "EURO" =
      EURO_blois %>%
      inner_join(
        x = Bureaux_EURO,
        by = join_by(
          code_du_departement,
          code_de_la_commune,
          code_du_b_vote,
          libelle_du_departement,
          libelle_de_la_commune
        )
      ) %>%
      mutate(Tour = "T1"),
    "LGS" = Blois$T1
  ) %>%
  bind_rows(.id = "scrutin") %>%
  discard(.p = ~ any(is.na(.)))

Redressement =
  input %>%
  group_by(scrutin,
           code_du_departement,
           code_de_la_commune,
           code_du_b_vote) %>%
  summarise(inscrits = sum(voix, na.rm = TRUE), .groups = "drop") %>%
  spread(scrutin, inscrits) %>%
  mutate(DELTA = EURO - LGS, candidat = "abstentions") %>%
  mutate(scrutin = case_when(DELTA < 0 ~ "EURO", DELTA > 0 ~ "LGS"),
         voix = abs(DELTA)) %>%
  drop_na(scrutin) %>%
  select(-EURO, -LGS, -DELTA)

output =
  bind_rows(input,
            Redressement) %>%
  group_by(scrutin, code_du_departement, code_de_la_commune, code_du_b_vote,
           candidat) %>%
  summarise(voix = sum(voix, na.rm = TRUE),
            .groups = "drop") %>%
  right_join(x = Bureaux_Pdt,
             by = join_by(code_du_departement, code_de_la_commune, code_du_b_vote))

output_nest =
  output %>%
  group_nest(code_du_departement,
             code_de_la_circonscription,
             libelle_de_la_circonscription) %>%
  mutate(Reports = data %>%
           map(.f = safely(mini_chaine),
               .progress = TRUE)
  ) %>%
  mutate(safe_reports = Reports %>%
           map(.f = ~ .$result))

NUPES =
  output_nest   %>%
  select(starts_with("code"),
         safe_reports) %>%
  unnest(cols = c(safe_reports)) %>%
  filter(source == "lgs_nup")

NUPES %>%
  distinct(code_du_departement, code_de_la_circonscription) %>%
  anti_join(x = Bureaux_Pdt) %>%
  distinct(code_du_departement, code_de_la_circonscription)  %>%
  view()

Nego_abst =
  NUPES %>%
  group_by(code_du_departement, code_de_la_circonscription) %>%
  top_n(n = 1, wt = report) %>%
  ungroup() %>%
  select(starts_with("code"), target)


Nego_abst %>%
  count(target)

Nego =
  NUPES %>%
  filter(
    target %>%
           str_detect(pattern = "abstention", negate = TRUE)
    ) %>%
  group_by(code_du_departement, code_de_la_circonscription) %>%
  top_n(n = 1, wt = report) %>%
  ungroup() %>%
  select(starts_with("code"), target)

Nego %>%
  count(target)

pacman::p_load(openxlsx)

list(
  "Reports NUPES aux européennes" = NUPES,
  "Liste plus gros report NUPES" = Nego,
  "Idem avec abstentions" = Nego_abst) %>%
  map(.f = tidyr::unite,
      col = circo,
               code_du_departement, code_de_la_circonscription) %>%
  openxlsx::write.xlsx(
    asTable = TRUE,
    file = "Europeennes 2024/Reports NUPES.xlsx")

output_nest   %>%
  select(starts_with("code"),
         safe_reports) %>%
  unnest(cols = c(safe_reports)) %>%
  filter(source == "lgs_abstentions") %>%
    semi_join(y = Nego_abst,
              by = join_by(target)) %>%
    ggplot(mapping = aes(y = paste(source, target,
                                   sep = " vers "),
                         x = report)) +
    geom_boxplot() +
    lims(x = c(0, 1))


