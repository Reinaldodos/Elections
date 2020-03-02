pacman::p_load(tidyverse, data.table, rio, RSQLite, ggdendro)
input =
  "https://www.data.gouv.fr/s/resources/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-par-bureaux-de-vote/20170427-100955/PR17_BVot_T1_FE.txt" %>%
  import(encoding="UTF-8") %>% rowid_to_column()
names(input) = names(input) %>% iconv(from = "Latin1", to = "UTF-8")

FETCH <- function(N) {
  test = input[, c(1, 25 + N * 7, 27 + N * 7)]
  names(test) = c("rowid", "Listes", "Voix")
  return(test)
}

Scores = map_df(.f = FETCH, .x = 0:10)

Inscrits = input[, 1:21]
output =
  inner_join(x = Inscrits, y = Scores, by = "rowid") %>%
  # mutate_all(type.convert) %>%
  select(-contains("%")) %>% select(-contains("/"))

Bureaux = output %>% select(rowid, contains("Code"), contains("Libellé")) %>% unique()
Resultats = output %>% select(rowid, Listes, Voix) %>% unique()
Chiffres = output %>% select(rowid, c(names(Resultats), names(Bureaux)) %>% setdiff(names(output), .)) %>% unique()

names(Bureaux) = names(Bureaux) %>% make.names() %>% stringi::stri_trans_general(id = "Latin-ascii")
names(Chiffres)=names(Chiffres) %>% make.names() %>%
  stringi::stri_trans_general(id = "Latin-ascii")
names(Resultats)=names(Resultats) %>% make.names() %>% stringi::stri_trans_general(id = "Latin-ascii")

require(magrittr)
Resultats %<>%
  mutate_if(
    .predicate = is.character,
    .funs = iconv,
    from = "Latin1",
    to = "UTF-8"
  )


pacman::p_load(RSQLite)
BASE = src_sqlite(path = "Presidentielles 2017/BASE", create = TRUE)

copy_to(dest = BASE, df = Bureaux, name = "Bureaux", overwrite = T, temporary = F, encoding = "UTF-8")
copy_to(dest = BASE, df = Chiffres, name = "Chiffres_globaux", overwrite = T, temporary = F, encoding = "UTF-8")
copy_to(dest = BASE, df = Resultats, name = "Resultats", overwrite = T, temporary = F, encoding = "UTF-8")
