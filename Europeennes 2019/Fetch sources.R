pacman::p_load(rvest, tidyverse, data.table)

url = "https://elections.interieur.gouv.fr/europeennes-2019/"

Departements =
  url %>% read_html() %>% html_nodes(css = "#listeDpt") %>% html_children() %>%
  html_attr(name = "value") %>%
  str_c(url %>% str_remove_all(pattern = "index.html"), .)

DEP <- function(dept) {
  print(dept)
  dept %>% read_html() %>% html_nodes(css = ".pub-index-communes a") %>%
    html_attr(name = "href") %>% str_remove_all(pattern = "\\.\\./") %>%
    str_c(url %>% str_remove_all(pattern = "index.html"), .) %>%
    return()
}

Lettres_villes = Departements %>% map(.f = DEP) %>% flatten_chr()

saveRDS(object = Lettres_villes, file = "Europeennes 2019/sources.rds")
