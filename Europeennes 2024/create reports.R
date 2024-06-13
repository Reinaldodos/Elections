require(tidyverse)

source("~/Documents/Elections/Europeennes 2024/Fonctions report.R")

Presidentielles =
  "Presidentielles 2022/resultats.rds" %>%
  read_rds() %>%
  filter(Tour == "T1")

Europeennes =
  "Europeennes 2024/output/resultats.rds" %>%
  read_rds()

Bureaux_Pdt =
  Presidentielles %>%
  select(starts_with("code"), libelle_de_la_circonscription) %>%
  distinct()

Bureaux_EURO =
  Europeennes %>%
  select(starts_with("code"), starts_with("libelle")) %>%
  distinct() %>%
  inner_join(
    y = Bureaux_Pdt,
    by = join_by(code_du_departement, code_de_la_commune, code_du_b_vote)
  )

input =
  list(
  "EURO" =
    Europeennes %>%
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
  "PDT" = Presidentielles
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
  mutate(DELTA = EURO - PDT, candidat = "abstentions") %>%
  mutate(scrutin = case_when(DELTA < 0 ~ "EURO", DELTA > 0 ~ "PDT"),
         voix = abs(DELTA)) %>%
  drop_na(scrutin) %>%
  select(-EURO, -PDT, -DELTA)

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
           map(.f = safely(chaine_report),
               .progress = TRUE)
         )

output_nest %>%
  mutate(safe_reports = Reports %>%
           map(.f = ~ .$result)) %>%
  select(-data, -Reports) %>%
  unnest(cols = c(safe_reports)) %>%
  saveRDS(file = "Europeennes 2024/reports PDT sur EURO.rds")

output_final =
  output_nest %>%
  mutate(safe_reports = Reports %>%
           map(.f = ~ .$result)) %>%
  mutate(Sankey = safe_reports %>%
           map(.f = Get_Sankey))

pwalk(
  .progress = TRUE,
  .f = get_web_report,
  .l = list(
    departement = output_final$code_du_departement,
    circo = output_final$code_de_la_circonscription,
    sankey = output_final$Sankey
  )
)


