html_get_lien <- function(url, ...) {
  output =
    url %>%
    read_html() %>%
    html_elements(...) %>%
    html_attr(name = "href")

  return(output)
}

append_liens <- function(parent, enfant, ...) {
  enfant =
    parent %>%
    str_remove_all(...) %>%
    str_c(enfant)

  return(enfant)
}

get_departements <- function(url_resultats) {
  pages_resultats =
    url_resultats %>%
    read_html()

  liens_departements =
    pages_resultats %>%
    html_children() %>%
    html_elements(css = "main div#container-map svg path") %>%
    html_attr(name = "onclick")

  url_dpts =
    liens_departements %>%
    str_remove_all(pattern = "parent.document.location.href='./") %>%
    str_remove_all(pattern = "'")

  url_dpts = url_dpts[!is.na(url_dpts)]

  url_dpts=
    url_resultats %>%
    str_remove(pattern = "/index.html") %>%
    str_c(url_dpts, sep = "/")

  return(url_dpts)
}

html_get_lettres <- function(departement) {
  url_lettres =
    departement %>%
    html_get_lien(css = ".fr-pagination__link") %>%
    paste(departement, ., sep = "/") %>%
    str_remove_all(pattern = "index.html/./")

  return(url_lettres)
}

get_communes <- function(lettre) {
  test_communes =
    lettre %>%
    html_get_lien(css = ".fr-mt-2w a")

  output =
    lettre %>%
    str_remove_all(pattern = stringr::regex(pattern = "[A-Z].html")) %>%
    paste(test_communes, sep = "/") %>%
    str_remove_all(pattern = "/./")

  return(output)
}

get_tables_bled <- function(url_bled) {
  page_bled =
    url_bled %>%
    read_html()

  nom_bled =
    page_bled %>%
    html_elements(css = ".fr-mb-1v") %>%
    html_text() %>%
    stringr::str_trim()

  Tables =
    page_bled %>%
    html_table()

  output =
    Tables[[2]] %>%
    filter(X1 %in% c("Abstentions", "Blancs", "Nuls")) %>%
    transmute(
      `Liste des candidatures par suffrages exprimés` = X1,
      Voix = X2)

  list(output,
       Tables[[1]]) %>%
    data.table::rbindlist(fill = TRUE) %>%
    cbind.data.frame(
    Commune = nom_bled
  ) %>%
    return()
}
