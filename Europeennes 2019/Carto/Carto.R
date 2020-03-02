pacman::p_load(tidyverse, data.table, rio, sf)

# opendata = "C:/Users/rdossant/Downloads/"
opendata = "~/Downloads/"


input =
  opendata %>% list.files(pattern = "\\.shp", full.names = T) %>%
  st_read(dsn = .) %>%
  st_transform(crs = 2154)

BASE = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = FALSE)

Results =
  tbl(src = BASE, "Resultats_bloc_commune") %>%
  as.data.table() %>%
  mutate(Code.de.la.commune =
           str_c("00000", Code.de.la.commune) %>%
           str_sub(start = -3)) %>%
  mutate(insee = str_c(Code.du.departement, Code.de.la.commune)) %>%
  select(insee, contains("departement"), Bloc, Voix) %>%
  group_by(insee) %>% mutate(Inscrits = sum(Voix)) %>% ungroup

Blocs = Results %>% distinct(Bloc) %>% pull

Results =
  Results %>%
  filter(Code.du.departement == "80") %>%
  spread(key = Bloc, value = Voix)

# Choropleth -------------------------------------------------------------
pacman::p_load(cartography)
koropless =
  Results %>%
  inner_join(x = input, by = "insee")

# koropless %>%
#   mutate_at(.vars = Blocs, .funs = ~100*./Inscrits) %>%
#   choroLayer(var = "EXD",
#              nclass = 5,
#              method = "sd")

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

# GROUP_KOR %>%
#   choroLayer(var = "EXD")

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

# Liss %>%
#   mutate_at(.vars = Blocs, .funs = ~100*./Inscrits) %>%
#   choroLayer(var = "EXD", nclass = 5, method = "sd")

# Anamorphose -------------------------------------------------------------
pacman::p_load(cartogram, lwgeom)

anamorf =
  Liss %>%
  st_cast(to = "MULTIPOLYGON") %>%
  cartogram_cont(weight = "Inscrits",
                 maxSizeError = 1.01)

# anamorf %>%
#   mutate_at(.vars = Blocs, .funs = ~ 100 * . / Inscrits) %>%
#   choroLayer(var = "EXD", nclass = 5, method = "sd")

# Carroyage ---------------------------------------------------------------
source("Europeennes 2019/Carto/Impression des cartes.R")
