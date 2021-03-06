
Results_blocs =
  MAT %>%
  gather(key = Listes, value = Voix, -rowid) %>%
  inner_join(y = Blocs, by = "Listes") %>%
  group_by(rowid, Bloc) %>%
  summarise(Voix = sum(Voix)) %>% ungroup

Results_blocs %>%
  group_by(Bloc) %>% summarise(Voix=sum(Voix)) %>%
  filter(Bloc != "Abstention") %>%
  mutate(Score = (Voix / sum(Voix)) %>% scales::percent()) %>% view()

Results_blocs_ville =
  Results_blocs %>%
  inner_join(y = tbl(src = BASE, "Bureaux"),
             by = "rowid",
             copy = T) %>%
  group_by(
    Code.du.departement,
    Code.de.la.commune,
    Libelle.du.departement,
    Libelle.de.la.commune,
    Bloc
  ) %>%
  summarise(Voix = sum(Voix)) %>% ungroup

copy_to(dest = BASE, df = Results_blocs, name = "Resultats_bloc_BdV", overwrite = T, temporary = F, analyze = T)

copy_to(dest = BASE, df = Results_blocs_ville, name = "Resultats_bloc_commune", overwrite = T, temporary = F, analyze = T)

