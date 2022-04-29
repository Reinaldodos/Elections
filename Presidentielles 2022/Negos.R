Gauche = c(
  "mélenchon",
  "jadot",
  "roussel",
  "hidalgo",
  "poutou"
) %>% str_c("_t1") %>% str_to_upper()

Proportionnelle =
  Scores %>%
  mutate(candidat = str_c(candidat, "_T1")) %>%
  filter(candidat %in% Gauche) %>%
  group_by(candidat) %>%
  summarise(voix = sum(voix)) %>%
  mutate(Sieges = round(577 * voix / sum(voix))) %>%
  arrange(Sieges)

Scores_Gauche =
  Scores_circo %>%
  gather(key = Circo, value = score, -candidat) %>%
  filter(candidat %in% Gauche) %>%
  group_by(Circo) %>%
  summarise(score = sum(score)) %>%
  mutate(Rank = dense_rank(-score)) %>%
  mutate(Gagnable = Rank <= 300)

Negos =
  Scores_circo %>%
  gather(key = Circo, value = score, -candidat) %>%
  filter(candidat %in% Gauche) %>%
  group_by(candidat) %>%
  mutate(Rank = dense_rank(-score)) %>% ungroup() %>%
  inner_join(y = Proportionnelle)

Pot_commun = tibble()
for (Tour_de_nego in 1:nrow(Proportionnelle)) {
  Joueur = Proportionnelle[Tour_de_nego, ] %>% as.list()
  Deal =
    Negos %>%
    filter(candidat == Joueur$candidat) %>%
    top_n(n = Joueur$Sieges, wt = score)

  Pot_commun %<>% bind_rows(Deal)
  Negos %<>% anti_join(y = Pot_commun, by = "Circo")
}

Pot_commun %>%
  group_by(Circo) %>%
  summarise(battle = paste(candidat, collapse = " ou ")) %>%
  inner_join(x = Circos)  %>%
  arrange(code_du_departement, code_de_la_circonscription) %>%
  split(x = .$Circo, f = .$battle)
