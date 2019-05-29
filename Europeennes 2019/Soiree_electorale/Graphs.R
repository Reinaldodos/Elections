pacman::p_load(tidyverse, data.table, rio, RSQLite)

"Chargement de la base" %>% print()

BASE = src_sqlite(path = "BASE", create = FALSE)

"Chargement OK" %>% print()

"Mise en forme des resultats" %>% print()

Results =
  inner_join(
  tbl(src = BASE, "Resultats") %>%
    inner_join(tbl(src = BASE, "Bureaux"), by = "rowid") %>%
    group_by(
      Code.du.departement,
      Code.de.la.commune,
      Listes
    ) %>%
    summarise(TOTAL = sum(Voix)),
  tbl(src = BASE, "Chiffres_globaux") %>%
    inner_join(tbl(src = BASE, "Bureaux"),by = "rowid") %>%
    select(-rowid, -Code.du.b.vote) %>%
    group_by(Code.du.departement, Code.de.la.commune, Libelle.du.departement, Libelle.de.la.commune) %>%
    summarise_all(.funs = sum),
  by = c("Code.du.departement", "Code.de.la.commune")) %>%
  as.data.table() %>% mutate_all(type.convert)

Order = Tableau %>% arrange(desc(TOTAL)) %>% pull(Listes) %>% as.character()
Results$Listes = factor(x = Results$Listes, levels = Order)

Tableau =
  tbl(src = BASE, "Resultats") %>%
  group_by(Listes) %>% summarise(TOTAL = sum(Voix, na.rm = T)) %>% ungroup %>%
  as.data.table() %>% mutate_all(type.convert) %>%
  mutate(Score = TOTAL/sum(TOTAL))

Tableau$Listes = factor(x = Tableau$Listes, levels = Order)

SAFE =
  Tableau %>%
  filter(Score>0.03) %>% pull(Listes) %>% as.character()
