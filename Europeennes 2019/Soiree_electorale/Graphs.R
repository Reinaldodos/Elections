
Non_exprimes = c("Abstentions", "Nuls", "Blancs")

Inscrits = Results %>%
  group_by(Region, Departement, Ville) %>%
  summarise(Inscrits = sum(Voix)) %>% ungroup

Exprimes =
  Results %>%
  filter(!Listes %in% Non_exprimes) %>%
  group_by(Region, Departement, Ville) %>%
  summarise(Exprimes = sum(Voix)) %>% ungroup

Results = list(Results, Inscrits, Exprimes) %>% reduce(inner_join)

Cut =
  Results %>%
  filter(!Listes %in% Non_exprimes) %>%
  summarise(TOT = sum(Voix)) %>% pull(TOT)

Tableau =
  Results %>%
  group_by(Listes) %>%
  summarise(TOT = sum(Voix))

SAFE =
  Tableau %>% filter(TOT > 0.03 * Cut) %>% pull(Listes) %>%
  as.character() %>% setdiff(Non_exprimes)


