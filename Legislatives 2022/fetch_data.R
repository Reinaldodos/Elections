pacman::p_load(rvest, magrittr, tidyverse, data.table)
url = "https://www.resultats-elections.interieur.gouv.fr/legislatives-2022/"
Links = url %>% str_c(., "index.html") %>% read_html()

Departements =
  Links %>%
  html_elements(css = ".Style6") %>%
  html_attr(name = "href") %>%
  str_split(pattern = "/") %>%
  map_chr(.f = ~ .[2]) %>%
  str_c(url, ., "/index.html")

Get_circos <- function(test) {
  test %>%
    read_html() %>%
    html_elements(css = "p a") %>%
    html_attr(name = "href") %>%
    str_remove_all(pattern = "\\.\\.") %>%
    str_c(url, .) %>%
    return()
}

Get_paris <- function(test) {
  test %>%
    read_html() %>%
    html_elements(css = ".table-striped a") %>%
    html_attr(name = "href") %>%
    str_subset(pattern = "AR", negate = T) %>%
    str_remove_all(pattern = "\\.\\.") %>%
    str_c(url, .) %>%
    return()
}

Paris =
  "https://www.resultats-elections.interieur.gouv.fr/legislatives-2022/075/index.html" %>%
  Get_paris()


Circos =
  Departements %>%
  map(.f = Get_circos) %>%
  flatten_chr() %>%
  append(Paris)

Get_results <- function(test) {
  test %<>% read_html()

  Resultats =
    test %>% html_table() %>%
    rbindlist(fill = T) %>%
    drop_na(Nuances)

  nom_circo = test %>% html_element(css = "h3:nth-child(1)") %>% html_text()
  print(nom_circo)
  list(circo = nom_circo,
       resultats = Resultats) %>%
    return()
}

safe_results = safely(Get_results)

Selection=NULL
