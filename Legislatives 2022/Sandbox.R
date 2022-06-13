Selection %<>%
  mutate(Bloc = recode(
    .x = Nuances,
    "DIV" = "Inconnu",
    "DSV" = "Inconnu",
    "DVC" = "LRM",
    "DVD" = "Droite",
    "DVG" = "Gauche",
    "DXD" = "EXD",
    "DXG" = "Gauche",
    "ECO" = "Ecolos",
    "ENS" = "LRM",
    "LR" = "Droite",
    "NUP" = "Gauche",
    "RDG" = "Gauche",
    "REC" = "EXD",
    "REG" = "Inconnu",
    "RN" = "EXD",
    "UDI" = "Droite"))

Selection %>%
  group_by(out, Bloc) %>%
  summarise(score = sum(`% Exprimés`), .groups = "drop") %>%
  spread(Bloc, score, fill = 0) %>%
  right_join(x = Battles) %>%
  filter(Gauche > LRM + Droite) %>%
  view()



