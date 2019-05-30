pacman::p_load(tidyverse, data.table, rio, sf)

opendata = "/Users/reinaldodossantos/Downloads/communes-20190101-shp/"

input =
  opendata %>% list.files(pattern = "shp", full.names = T) %>%
  st_read(dsn = .)

BASE = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = FALSE)

Results =
  tbl(src = BASE, "Resultats_bloc_commune") %>%
  as.data.table() %>%
  mutate(Code.de.la.commune =
           str_c("00000", Code.de.la.commune) %>%
           str_sub(start = -3)) %>%
  mutate(insee = str_c(Code.du.departement, Code.de.la.commune)) %>%
  select(insee, contains("departement"), Bloc, Voix) %>%
  group_by(insee) %>% mutate(Inscrits = sum(Voix)) %>%
  mutate(Voix = Voix / Inscrits * 100) %>% ungroup %>%
  spread(key = Bloc, value = Voix)

Results =
  Results %>% filter(Code.du.departement=="33")

# Choropleth -------------------------------------------------------------
pacman::p_load(cartography)
koropless =
  Results %>%
  inner_join(x = input, by = "insee")

koropless %>%
  choroLayer(var = "EXD",
             nclass = 5,
             method = "sd")

breaks = classInt::classIntervals(var = koropless$EXD, n = 5, style = "sd")

GROUP_KOR =
  koropless %>%
  mutate(Groupe = cut(EXD, breaks = breaks$brks, include.lowest = T)) %>%
  group_by(Groupe) %>%
  summarize(EXD = sum(EXD * Inscrits) / sum(Inscrits),
            Inscrits = sum(Inscrits))

GROUP_KOR %>%
  choroLayer(var = "EXD", breaks = breaks$brks)

# Anamorphose -------------------------------------------------------------
pacman::p_load(cartogram, lwgeom)

anamorf =
  GROUP_KOR %>%
  st_cast(to = "MULTIPOLYGON") %>%
  cartogram_cont(weight = "Inscrits",
                 itermax = 2)

anamorf %>%
  choroLayer(var = "EXD",
             breaks = breaks$brks)

# Carroyage ---------------------------------------------------------------
KARO =
  koropless %>%
  getGridLayer(cellsize = .025 ^ 2,
               type = "hexagonal",
               var = "EXD")

KARO %>%
  choroLayer(var = "EXD",
             breaks = breaks$brks)

KARO %>%
  st_join(koropless %>% select(Inscrits, geometry)) %>%
  mutate(Groupe = cut(EXD, breaks = breaks$brks, include.lowest = T)) %>%
  group_by(Groupe) %>%
  summarize(EXD = sum(EXD * Inscrits) / sum(Inscrits),
            Inscrits = sum(Inscrits)) %>%
  choroLayer(var = "EXD", breaks = breaks$brks)

# Lissage -----------------------------------------------------------------
base_temp =
  koropless %>% st_centroid() %>% st_coordinates() %>% data.table() %>%
  cbind.data.frame(koropless)


base.ppp = spatstat::ppp(base_temp$X, base_temp$Y,
                         c(min(base_temp$X), max(base_temp$X)),
                         c(min(base_temp$Y), max(base_temp$Y)))

DENS =
  spatstat::density.ppp (base.ppp, sigma = .025,
                         weights = base_temp$EXD)

DENS %>% plot()
