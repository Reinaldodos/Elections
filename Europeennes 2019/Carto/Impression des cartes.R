TOTO =
    anamorf %>%
    mutate_at(.vars = Blocs, .funs = ~ 100 * . / Inscrits)

KARTO <- function(BLOK) {
  require(classInt)
  Choix = sym(BLOK)
  TOTO %>%
    rename(Choix = !!Choix) %>%
    mutate(Groupe = cut(
      x = Choix,
      breaks = classIntervals(var = Choix, n = 5, style = "sd")$brks,
      include.lowest = T
    )) %>%
    group_by(Groupe) %>%
    summarize(Score = sum(Choix * Inscrits) / sum(Inscrits),
              Inscrits = sum(Inscrits)) %>%
    choroLayer(var = "Score") %>%
    layoutLayer(title = BLOK)
}

KARTO(BLOK = "EXD")

jpeg(filename = "Europeennes 2019/Carto/Cartes.jpeg")
Blocs %>% walk(.f = KARTO)
dev.off()
