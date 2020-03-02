pacman::p_load(tidyverse, data.table, RSQLite, psych, ggcorrplot)

BASE = src_sqlite(path = "Presidentielles 2017/BASE", create = F)

Resultats = tbl(src = BASE, "Resultats")
Chiffres = tbl(src = BASE, "Chiffres_globaux")

MAT=
  Resultats %>% as.data.table() %>%
  spread(key = Listes, value = Voix, fill = 0) %>%
  inner_join(y = Chiffres %>% select(rowid, Abstentions, Blancs, Nuls),
             by = "rowid", copy=T)

KOR =
  MAT %>% select(-rowid) %>%
  cor(use = "pairwise.complete.obs")

KOR %>% ggcorrplot::ggcorrplot(hc.order = T)

require(psych)
KOR %>% fa.parallel(n.obs = nrow(MAT), fa = "both")

KOR %>% psych::pca(nfactors = 2, n.obs = nrow(MAT)) %>% fa.diagram()

KOR %>%
  psych::fa(nfactors = 5, n.obs = nrow(MAT),
            rotate = "oblimin") %>%
  fa.diagram(simple = F, cut = .25)

hc = KOR %>% dist() %>% hclust(method = "ward.D")
hc %>% ggdendrogram(rotate = T)

KOR %>%
  psych::omega(nfactors = 5, n.obs = nrow(MAT))

