pacman::p_load(tidyverse, data.table, rio, here)
MAT =
  here("Europeennes 2019/Comparaison avec Pdt 2017/Data Pdt_2017 et EUR_2019.rds") %>%
  import()

KOR =
  MAT %>% select(-rowid) %>%
  cor(use = "pairwise.complete.obs")

pacman::p_load(ggcorrplot, ggdendro)

KOR %>% ggcorrplot::ggcorrplot(hc.order = T)
hc = KOR %>% dist() %>% hclust(method = "ward.D")
hc %>% ggdendrogram(rotate = T)

OUT = hc %>% cutree(k = 2)
OUT = OUT[OUT==2] %>% names

MAT_princ = MAT %>% select(-OUT)
KOR_princ =
  MAT_princ %>% select(-rowid) %>%
  cor(use = "pairwise.complete.obs")

KOR_princ %>% ggcorrplot(type = "full", hc.order = T, lab = F)

hc_princ = KOR_princ %>% dist() %>% hclust(method = "ward.D")


PHYLO <- function(N, hc_princ, ...) {
  require(ape)
  colours = RColorBrewer::brewer.pal(n = N, name = "Set2")
  clus4 = cutree(hc_princ,k = N)
  plot(
    as.phylo(hc_princ),
    tip.color = colours[clus4],
    cex = 0.7, ...
  )
}

hc_princ %>% ggdendrogram(rotate = T)

PHYLO(hc_princ = hc_princ, N = 5, type = "cladogram")

