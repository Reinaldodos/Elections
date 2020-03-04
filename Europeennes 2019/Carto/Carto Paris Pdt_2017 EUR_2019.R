pacman::p_load(tidyverse, data.table, RSQLite, psych, ggcorrplot)

BASE = src_sqlite(path = "Presidentielles 2017/BASE", create = F)

Resultats = tbl(src = BASE, "Resultats")
Chiffres = tbl(src = BASE, "Chiffres_globaux")
Bureaux = tbl(src = BASE, "Bureaux")

BASE_EUR = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = FALSE)

Bureaux_EUR=
  tbl(BASE_EUR, "Bureaux") %>% as.data.frame %>%
  rename(row_EUR = rowid) %>%
  filter(Code.du.departement=="75")

Bureaux_Pdt =
  Bureaux %>% as.data.table() %>%
  mutate_if(
    .predicate = is.character,
    .funs = iconv,
    from = "Latin1",
    to = "UTF-8"
  ) %>%
  filter(Code.du.departement=="75")

MAT_commune =
  "Europeennes 2019/Comparaison avec Pdt 2017/Data Pdt_2017 et EUR_2019.rds" %>%
  read_rds() %>%
  gather(key = Listes, value = Voix,-rowid) %>%
  filter(Voix > 0) %>%
  semi_join(y = Bureaux_Pdt) %>%
  spread(key = Listes, value = Voix, fill = 0)

KOR_commune =
  MAT_commune %>% select(-rowid) %>%
  cor()

KOR_commune %>% ggcorrplot(hc.order = T)

KOR_commune %>% fa.parallel(n.obs = nrow(MAT_commune))
FA = KOR_commune %>% fa(nfactors = 5, n.obs = nrow(MAT_commune))

IN =
  FA$uniquenesses[FA$uniquenesses < FA$communalities] %>%
  names

KOR_PARIS =
  MAT_commune %>% select(IN) %>%
  cor()

KOR_PARIS %>% ggcorrplot(hc.order = T)
KOR_PARIS %>% fa.parallel(n.obs = nrow(MAT_commune), fm = "ml")

hc_paris = KOR_PARIS %>% dist() %>% hclust(method = "ward.D")
hc_paris %>% ggdendrogram(rotate = T)

Blocs =
  hc_paris %>%
  cutree(k = 6) %>%
  as.data.table(keep.rownames = T) %>%
  rename(Liste = "rn", Bloc = ".") %>%
  mutate(Bloc=paste("Bloc", Bloc, sep = "_"))

Blocs %>% split(f = as.character(.$Bloc), x = .$Liste)

pacman::p_load(magrittr)
Blocs %<>%
  mutate(Bloc=
           recode_factor(.x = Bloc,
                         "Bloc_6"="Gauche",
                         "Bloc_5"="Droite",
                         "Bloc_4"="Centre",
                         "Bloc_3"="EXD",
                         "Bloc_2"="Abstention",
                         "Bloc_1"="WTF"))

Blocs %>% split(f = as.character(.$Bloc), x = .$Liste)

MAT_PARIS =
  MAT_commune %>%
  gather(key = Liste, value = Voix, -rowid) %>%
  inner_join(y=Blocs) %>%
  group_by(Bloc, rowid) %>%
  summarise(Voix=sum(Voix)) %>% ungroup %>%
  left_join(y=Bureaux_Pdt) %>%
  group_by(Code.du.b.vote) %>%
  mutate(Voix=Voix/sum(Voix)) %>% ungroup %>%
  spread(key = Bloc, value = Voix, fill = 0)


# Carto -------------------------------------------------------------------
pacman::p_load(tidyverse, data.table, rio, sf)

opendata = "/Users/reinaldodossantos/Downloads/secteurs-des-bureaux-de-vote/"
Carte =
  opendata %>% list.files(pattern = "shp", full.names = T) %>%
  st_read(dsn = .) %>%
  st_transform(crs = 2154)

input =
  Carte %>%
  mutate(arrondissem = str_c("0", arrondissem) %>% str_sub(start = -2),
         num_bv = str_c("0", num_bv) %>% str_sub(start = -2)) %>%
  mutate(id_bv = str_c(arrondissem, num_bv)) %>%
  inner_join(y = MAT_PARIS, by = c("id_bv" = "Code.du.b.vote"))

pacman::p_load(cartography, sf, osrm, rosm, ggmap)

Blocs$Bloc %>% levels %>% purrr::set_names() %>%
  walk(.f = ~ choroLayer(
    x = input %>% mutate_at(.vars = levels(Blocs$Bloc),
                            .funs = ~ 100 * .),
    var = .,
    nclass = 5,
    method = "fisher-jenks"
  ))

