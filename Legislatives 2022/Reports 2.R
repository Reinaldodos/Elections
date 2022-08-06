pacman::p_load(tidyverse, magrittr, data.table, janitor, readxl)
source(file = "Legislatives 2022/sankey_fonctions.R")

Reports = "Legislatives 2022/Reports_circo.rds" %>% read_rds()

Resultats =
  "Legislatives 2022/resultats.rds" %>%
  read_rds() %>%
  group_by(libelle_de_la_circonscription,
           libelle_du_departement,
           candidat) %>%
  summarise(voix = sum(voix, na.rm = TRUE),
            .groups = "drop")

Circos =
  Reports %>%
  distinct(libelle_du_departement,
           libelle_de_la_circonscription)

Reports =
  Resultats %>%
  rename(T1 = candidat) %>%
  inner_join(x = Reports,
             by = c("libelle_du_departement",
                    "libelle_de_la_circonscription",
                    "T1")) %>%
  mutate(REPORT_voix = report*voix)

saveRDS(object = Reports, file = "Legislatives 2022/Reports_circo.rds")
