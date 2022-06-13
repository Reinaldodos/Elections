# Scores circos -----------------------------------------------------------

Scores_circos =
  output_T1 %>%
  group_by(code_du_departement, code_de_la_circonscription,
           candidat) %>%
  summarise(voix = sum(voix),
            .groups = "drop") %>%
  filter(!candidat %in% c("abstentions", "blancs", "nuls")) %>%
  spread(key = candidat, value = voix, fill = 0) %>%
  gather(key = candidat, value = voix,
         -code_du_departement, -code_de_la_circonscription)

Scores_circos %<>%
  transmute(
    circo =
      str_c(code_du_departement, code_de_la_circonscription) %>%
      str_remove_all(pattern = "^0"),
    candidat, voix)

rm(output_T1)
gc()

# Participation 2017 ------------------------------------------------------

Legislatives_2017 =
  "~/Téléchargements/Leg_2017_Resultats_T1_c.xlsx" %>%
  readxl::read_excel(sheet = "Circo. leg. T1",
                     skip = 2) %>%
  janitor::clean_names()

Legislatives_2017 %<>%
  transmute(code_du_departement,
            code_de_la_circonscription,
            Participation = percent_exp_ins/100)

Legislatives_2017 %<>%
  mutate(
    code_de_la_circonscription =
      str_c("0", code_de_la_circonscription) %>%
      str_sub(start = -2)
           ) %>%
  transmute(
    circo =
      str_c(code_du_departement, code_de_la_circonscription),
    Participation)

Participation_nationale =
  Scores_circos %>%
  group_by(circo) %>%
  summarise(Exprimes = sum(voix)) %>%
  inner_join(y = Legislatives_2017,
             by = "circo") %>%
  mutate(Inscrits = Exprimes / Participation) %>%
  summarise_at(.vars = vars(Exprimes, Inscrits),
               .funs = sum) %>%
  summarise(Participation_nationale = Exprimes/Inscrits) %>%
  pull()

list("Scores" = Scores_circos,
     "Participation" = Legislatives_2017,
     "Participation nationale" = Participation_nationale) %>%
  saveRDS(file = "Presidentielles 2022/Projections_legislatives/data_pour_legislatives.rds")
