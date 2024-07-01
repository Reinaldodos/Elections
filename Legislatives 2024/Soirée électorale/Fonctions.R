pacman::p_load(tidyverse, rvest)

get_tables <- function(url) {
  Tables =
    url %>%
    read_html() %>%
    html_table()

  list(
    "Resultats" = Tables[[1]],
    "Participation" = clean_votants(table = Tables[[2]])
  ) %>%
    map(.f = janitor::clean_names) %>%
    return()
}

clean_votants <- function(table) {
  data =
    table  %>%
    as.data.frame() %>%
    column_to_rownames(var = "X1")

  names(data) = data[1,] %>% as.vector()

  data %>%
    slice(2:nrow(data)) %>%
    return()
}

get_circos <- function(departement, urlweb, XPATH) {
  Liste_Circos =
    departement %>%
    build_url(urlweb = urlweb) %>%
    read_html() %>%
    html_elements(xpath = XPATH) %>%
    html_children()

  cbind.data.frame(
    url_circo =
      Liste_Circos %>%
      html_attr(name = "value"),
    circo =
      Liste_Circos %>%
      html_text() %>%
      str_trim()
  ) %>%
    filter(url_circo %>%
             str_detect(pattern = "html$")) %>%
    return()
}

build_url <- function(urlweb, url_dpt) {
  list(
    urlweb %>%
      str_remove_all(pattern = "index.html$"),
    url_dpt %>%
      str_remove_all(pattern = "^\\./")
  ) %>%
    reduce(.f = str_c) %>%
    return()
}

