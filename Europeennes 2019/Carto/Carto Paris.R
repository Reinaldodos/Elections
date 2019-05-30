pacman::p_load(tidyverse, data.table, rio, sf)

opendata = "/Users/reinaldodossantos/Downloads/secteurs-des-bureaux-de-vote/"
input =
  opendata %>% list.files(pattern = "shp", full.names = T) %>%
  st_read(dsn = .)

BASE = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = FALSE)

source("Europeennes 2019/Carto/Paris.R")

Results =
  tbl(src = BASE, "Resultats_bloc_BdV") %>% as.data.table() %>%
  group_by(rowid) %>% mutate(Inscrits = sum(Voix)) %>%
  mutate(Voix = Voix / Inscrits * 100) %>% ungroup %>%
  spread(key = Bloc, value = Voix)

# Choropleth -------------------------------------------------------------
pacman::p_load(cartography)
koropless =
  Results %>%
  inner_join(x = input,
             by = "rowid")

breaks = classInt::classIntervals(var = koropless$EXD, n = 8, style = "pretty")

koropless %>%
  mutate(Groupe = cut(EXD, breaks = breaks$brks, include.lowest = T)) %>%
  group_by(Groupe) %>%
  summarize(EXD = sum(EXD*Inscrits)/sum(Inscrits),
            Inscrits = sum(Inscrits)) %>%
  # cartogram_cont(itermax = 15, weight = "Inscrits") %>%
  choroLayer(var = "EXD", breaks = breaks$brks)

# Anamorphose -------------------------------------------------------------
pacman::p_load(cartogram, lwgeom)

anamorf =
  koropless %>%
  st_cast(to = "MULTIPOLYGON") %>%
  cartogram_cont(weight = "Inscrits", itermax = 5)

anamorf %>%
  choroLayer(var = "EXD",
             nclass = 5,
             method = "sd")

# Carroyage ---------------------------------------------------------------
KARO =
  koropless %>%
  getGridLayer(cellsize = .0025 ^ 2,
               type = "hexagonal",
               var = "EXD")

KARO %>%
  choroLayer(var = "EXD",
             nclass = 5,
             method = "sd")

# Lissage -----------------------------------------------------------------
base_temp =
  koropless %>% st_centroid() %>% st_coordinates() %>% data.table() %>%
  cbind.data.frame(koropless)


base.ppp = spatstat::ppp(base_temp$X, base_temp$Y,
                         c(min(base_temp$X), max(base_temp$X)),
                         c(min(base_temp$Y), max(base_temp$Y)))

DENS =
  spatstat::density.ppp (base.ppp, sigma = .0025,
                         weights = base_temp$Droite)
DENS %>% plot()
