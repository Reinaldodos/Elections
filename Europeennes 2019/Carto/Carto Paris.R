pacman::p_load(tidyverse, data.table, rio, sf)

opendata = "C:/Users/rdossant/Downloads/secteurs-des-bureaux-de-vote/"
input =
  opendata %>% list.files(pattern = "shp", full.names = T) %>%
  st_read(dsn = .) %>%
  st_transform(crs = 2154)

BASE = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = FALSE)

source("Europeennes 2019/Carto/Paris.R")

Results =
  tbl(src = BASE, "Resultats") %>% as.data.table() %>%
  group_by(rowid) %>% mutate(Inscrits = sum(Voix)) %>%
  ungroup %>%
  rename(Bloc = Listes)

Blocs = Results %>% distinct(Bloc) %>% pull

Results =
  Results %>%
  # filter(Code.du.departement %in% as.character(c(75,91,92,93,94,95,77,78))) %>%
  spread(key = Bloc, value = Voix)

# Choropleth -------------------------------------------------------------
pacman::p_load(cartography)
koropless =
  Results %>%
  inner_join(x = input,
             by = "rowid")

koropless %>%
  mutate_at(.vars = Blocs, .funs = ~100*./Inscrits) %>%
  choroLayer(var = "EXD",
             nclass = 5,
             method = "sd")

pacman::p_load(classInt)

GROUP_KOR =
  koropless %>%
  mutate_at(.vars = Blocs, .funs = ~100*./Inscrits) %>%
  mutate(Groupe = cut(
    EXD,
    breaks = classIntervals(var = EXD, n = 5, style = "sd")$brks,
    include.lowest = T
  )) %>%
  group_by(Groupe) %>%
  summarize(EXD = sum(EXD * Inscrits) / sum(Inscrits),
            Inscrits = sum(Inscrits))

GROUP_KOR %>%
  choroLayer(var = "EXD")

# Lissage -----------------------------------------------------------------
base_temp =
  koropless %>% st_centroid() %>% st_coordinates() %>% data.table() %>%
  cbind.data.frame(koropless) %>%
  rename(x = X, y = Y)

pacman::p_load(btb, spatstat)

base.ppp = spatstat::ppp(base_temp$x, base_temp$y,
                         c(min(base_temp$x), max(base_temp$x)),
                         c(min(base_temp$y), max(base_temp$y)))

SIGMA = bw.diggle(X = base.ppp)

Liss =
  base_temp %>%
  keep(.p = is.numeric) %>%
  kernelSmoothing(iCellSize = SIGMA,
                  iBandwidth = SIGMA * 2,
                  sEPSG = 2154)

Liss %>%
  mutate_at(.vars = Blocs, .funs = ~100*./Inscrits) %>%
  choroLayer(var = "EXD", nclass = 5, method = "sd")

# Anamorphose -------------------------------------------------------------
pacman::p_load(cartogram, lwgeom)

anamorf =
  Liss %>%
  st_cast(to = "MULTIPOLYGON") %>%
  cartogram_cont(weight = "Inscrits", maxSizeError = 1.01)

anamorf %>%
  mutate_at(.vars = Blocs, .funs = ~ 100 * . / Inscrits) %>%
  choroLayer(var = "EXD", nclass = 5, method = "sd")

# Carroyage ---------------------------------------------------------------
anamorf %>%
  mutate_at(.vars = Blocs, .funs = ~ 100 * . / Inscrits) %>%
  mutate(Groupe = cut(
    x = EXD,
    breaks = classIntervals(var = EXD, n = 5, style = "sd")$brks,
    include.lowest = T
  )) %>%
  group_by(Groupe) %>%
  summarize(EXD = sum(EXD * Inscrits) / sum(Inscrits),
            Inscrits = sum(Inscrits)) %>%
  choroLayer(var = "EXD")
