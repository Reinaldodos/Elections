require(tidyverse)

clean_BASE <- function(BASE) {
  input =
    BASE %>%
    collect() %>%
    mutate(across(
      .cols = starts_with("percent"),
      .fns = \(x)
      str_replace_all(
        string = x,
        pattern = ",",
        replacement = "\\."
      ) %>%
        as.numeric()
    ))

  input$voix =
    input$voix %>%
    str_remove_all(pattern = "[^[0-9]]") %>%
    as.numeric()

  return(input)
}

BASE =
  "Legislatives 2024/Soirée électorale/Stock/" %>%
  arrow::open_dataset(format = "arrow")

input =
  BASE %>%
  clean_BASE()

output =
  input %>%
  drop_na(voix)

input %>%
  group_nest(Departement, circo, url) %>%
  jsonlite::write_json(path = "Legislatives 2024/Soirée électorale/bilan.json")

Elus =
  input %>%
  filter(elu_e == "OUI")

Elus_partiels =
  Elus %>%
  anti_join(x = input,
            by = join_by(Departement, circo)) %>%
  group_by(circo) %>%
  top_n(n = 1, wt = voix)

Etiquettes_NFP =
  "Legislatives 2024/tableau-final-accord-frontpopulaire-leg2024-1.xlsx" %>%
  readxl::read_excel() %>%
  janitor::clean_names() %>%
  select(circo, etiquette) %>%
  drop_na() %>%
  mutate(circo =
           circo %>%
           str_remove_all(pattern = "[^[Z 0-9]]"))

Bilan =
  list(Elus, Elus_partiels) %>%
  data.table::rbindlist(fill = TRUE) %>%
  tidyr::separate(col = circo,
                  into = c("code_circo",
                           "libelle_circo"),
                  sep = " - ") %>%
  left_join(y = Etiquettes_NFP,
            by = join_by(code_circo == circo)) %>%
  mutate(
    groupe_parlementaire = case_when(
      nuance == "UG" ~ etiquette,
      .default = nuance
    )
  ) %>%
  replace_na(replace = list(groupe_parlementaire = "REG"))

Circos =
  Bilan %>%
  distinct(Departement, code_circo)

if(nrow(Circos) < 577) {
  Bilan =
    BASE %>%
    distinct(Departement, circo) %>%
    collect() %>%
    tidyr::separate(col = circo,
                    into = c("code_circo",
                             "libelle_circo"),
                    sep = " - ") %>%
    left_join(y = Bilan,
              by = join_by(Departement, code_circo, libelle_circo),
              copy = TRUE)
}

Ordre =
  tribble(
    ~ groupe_parlementaire, ~ parti, ~ couleur, ~Bloc,
  "FI", "France Insoumise", "darkred", "NFP",
  "PCF", "Parti Communiste Français" ,"red","NFP",
  "PE", "Europe Ecologie", "#23850b","NFP",
  "PS", "Parti Socialiste", "#db4299","NFP",
  "SOC", "Parti Socialiste", "#db4299","",
  "DIV", "Parti Socialiste", "#db4299","",
  "DVG", "Divers Gauche", "#f29999","",
  "ECO", "Génération Ecologie", "#5fe63e","NFP",
  "REG", "Régionaliste", "#4a5249","",
  NA, "Résultat non parvenu", "#c7c1c1","",
  "DVC", "Divers Centre", "#f7c040", "Macronie",
  "ENS", "Ensemble !", "orange", "Macronie",
  "HOR", "Horizons", "cyan", "Macronie",
  "UDI", "UDI", "#177380", "Droite",
  "DVD", "Divers Droite", "#0f4bf2", "Droite",
  "LR", "Les Républicains", "blue", "Droite",
  "UXD", "Les amis d'Eric Ciotti", "brown", "EXD",
  "RN", "Rassemblement National", "#401515", "EXD",
  "EXD", "Extrême Droite", "black", "EXD"
)

Ordre$parti =
  factor(x = Ordre$parti,
         levels = unique(Ordre$parti))

Assemblee =
  Bilan %>%
  left_join(y = Ordre) %>%
  count(parti, couleur) %>%
  ggparliament::parliament_data(
    election_data = .,
    type = "semicircle",
    party_seats = .$n,
    # plot_order = .$parti,
    parl_rows = 12
  )


Assemblee %>%
  ggplot(mapping = aes(x = x, y = y,
                       colour = parti)) +
  ggparliament::geom_parliament_seats() +
  ggparliament::draw_majoritythreshold(n = 289,
                                       label = FALSE,
                                       type = 'semicircle') +
  ggparliament::draw_partylabels(type = "semicircle",
                   party_names = parti,
                   party_seats = n,
                   party_colours = couleur) +
  ggparliament::theme_ggparliament() +
  labs(colour = "Groupe parlementaire",
       title = "Assemblée Nationale") +
  theme(legend.position = "bottom") +
  scale_colour_manual(
    values = Ordre$couleur,
    limits = Ordre$parti
    )

Bilan %>%
  left_join(y = Ordre) %>%
  count(parti) %>%
  arrange(-n)
