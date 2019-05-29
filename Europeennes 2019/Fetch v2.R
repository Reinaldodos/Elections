pacman::p_load(tidyverse, data.table, rio)
input =
  "https://www.data.gouv.fr/fr/datasets/r/0b723438-41d3-40cb-8ce0-bf6ff2184dd1" %>%
  import() %>% rowid_to_column()

FETCH <- function(N) {
  test = input[, c(1, 22 + N * 7, 25 + N * 7)]
  names(test) = c("rowid", "Listes", "Voix")
  return(test)
}

Scores = 0:33 %>% map_df(FETCH)

Inscrits = input[, 1:20]

output = inner_join(x = Inscrits, y = Scores)

names(output) = names(output) %>% iconv(from = "Latin1", to = "UTF-8")

output =
  output %>%
  mutate_all(.funs = ~ iconv(from = "Latin1", to = "UTF-8", x = .)) %>%
  mutate_all(type.convert) %>%
  select(-contains("%")) %>% select(-contains("/"))

Bureaux = output %>% select(rowid, contains("Code"), contains("Libellé")) %>% unique()
Resultats = output %>% select(rowid, Listes, Voix) %>% unique()
Chiffres = output %>% select(rowid, c(names(Resultats), names(Bureaux)) %>% setdiff(names(output), .)) %>% unique()

pacman::p_load(RSQLite)
BASE = src_sqlite(path = "Europeennes 2019/Soiree_electorale/BASE", create = TRUE)

names(Bureaux) = names(Bureaux) %>% make.names() %>% stringi::stri_trans_general(id = "Latin-ascii")
names(Chiffres)=names(Chiffres) %>% make.names() %>% stringi::stri_trans_general(id = "Latin-ascii")
names(Resultats)=names(Resultats) %>% make.names() %>% stringi::stri_trans_general(id = "Latin-ascii")

copy_to(dest = BASE, df = Bureaux, name = "Bureaux", overwrite = T, temporary = F, encoding = "UTF-8")
copy_to(dest = BASE, df = Chiffres, name = "Chiffres_globaux", overwrite = T, temporary = F, encoding = "UTF-8")
copy_to(dest = BASE, df = Resultats, name = "Resultats", overwrite = T, temporary = F, encoding = "UTF-8")
