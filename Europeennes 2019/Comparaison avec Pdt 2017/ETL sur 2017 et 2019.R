pacman::p_load(tidyverse, data.table, RSQLite)

BASE = src_sqlite(path = "Presidentielles 2017/BASE", create = F)

Resultats = tbl(src = BASE, "Resultats")
Chiffres = tbl(src = BASE, "Chiffres_globaux")
Bureaux = tbl(src = BASE, "Bureaux")

BASE_EUR = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = FALSE)


BUROS =
  tbl(BASE_EUR, "Bureaux") %>% as.data.frame %>%
  rename(row_EUR=rowid) %>%
  inner_join(y = Bureaux,
             by = c("Code.du.departement", "Libelle.de.la.commune",
                    "Code.du.b.vote"),
             copy = T) %>%
  distinct(rowid, row_EUR)


Presidentielle =
  Resultats %>% as.data.table() %>%
  spread(key = Listes, value = Voix, fill = 0) %>%
  inner_join(y = Chiffres %>% select(rowid, Abstentions, Blancs, Nuls),
             by = "rowid",
             copy = T) %>%
  inner_join(y = BUROS)


Europeennes =
  tbl(src = BASE_EUR, "Resultats") %>%
  as.data.table() %>%
  spread(key = Listes, value = Voix)

Europeennes =
  tbl(src = BASE_EUR, "Chiffres_globaux") %>%
  select(rowid, Abstentions, Blancs, Nuls) %>%
  inner_join(x = Europeennes, copy = T, by = "rowid")%>%
  rename(row_EUR=rowid) %>%
  inner_join(y=BUROS, by = "row_EUR")

MAT =
  inner_join(x = Presidentielle,
             y = Europeennes,
             by = c("rowid", "row_EUR")) %>%
  select(-row_EUR)

saveRDS(object = MAT, file = "Europeennes 2019/Comparaison avec Pdt 2017/Data Pdt_2017 et EUR_2019.rds")
