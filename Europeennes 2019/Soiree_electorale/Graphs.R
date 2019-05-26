
Non_exprimes = c("Abstentions", "Nuls", "Blancs")

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


