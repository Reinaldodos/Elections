pacman::p_load(tidyverse, data.table, RSQLite, psych, ggcorrplot)

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

MAT=
  inner_join(x = Presidentielle,
           y = Europeennes,
           by = c("rowid", "row_EUR")) %>%
  select(-row_EUR)

saveRDS(object = MAT, file = "Europeennes 2019/Data Pdt_2017 et EUR_2019.rds")

KOR =
  MAT %>% select(-rowid) %>%
  cor(use = "pairwise.complete.obs")

KOR %>% ggcorrplot::ggcorrplot(hc.order = T)

KOR %>% fa.parallel(n.obs = nrow(MAT))

OMEGA =
  KOR %>%
  psych::omega(fm = "ml",
               nfactors = 13,
               n.obs = nrow(MAT))

OMEGA$ECV

OUT =
  OMEGA$schmid$sl %>% as.data.frame(row.names = rownames(.)) %>%
  rownames_to_column(var = "Listes") %>%
  filter(u2 > h2) %>%
  arrange(desc(abs(g))) %>% pull(Listes)


MAT_princ = MAT %>% select(-OUT)
KOR_princ =
  MAT_princ %>% select(-rowid) %>%
  cor(use = "pairwise.complete.obs")

require(ggcorrplot)
KOR_princ %>% ggcorrplot(type = "full", hc.order = T, lab = F)

KOR_princ %>% fa.parallel(n.obs = nrow(MAT_princ))

FA =
  KOR_princ %>%
  fa(nfactors = 7,
     n.obs = nrow(MAT),
     fm = "ml")

FA %>% fa.diagram(simple = F, cut = 1/3)

hc_princ = KOR_princ %>% dist() %>% hclust(method = "ward.D")

hc_princ %>% ggdendrogram(rotate = T)
