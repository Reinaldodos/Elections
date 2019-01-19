require(rvest)
url = "http://www.bbc.com/news/politics/constituencies"

require(data.table)
CONSTITUENCIES =
  url %>% read_html() %>% html_table() %>% rbindlist()
names(CONSTITUENCIES) =
  c("Constituencies", "Nation")

URLS = url %>%
  read_html() %>%
  html_nodes(css = ".az-item__letter-link") %>%
  html_attr(name = "href")

require(foreach)
constituencies =
  foreach(URL = URLS) %do%
  {
    paste(url, URL, sep = "") %>%
      read_html() %>%
      html_nodes(css = paste(URL, "a", sep = " ")) %>%
      html_attr(name = "href")
  } %>%
  unlist()

constituencies =
  paste("http://www.bbc.com", constituencies, sep = "")


Fetchez <- function(constituency)
  {

  test =
    constituency %>%
    read_html() %>%
    html_table(fill = TRUE)
  foo=test[[2]]

  Result =
    constituency %>%
    read_html() %>%
    html_nodes(css = ".constituency-title__title") %>%
    html_text() %>%
    cbind.data.frame(foo)

  Result[["."]] %>% unique() %>% as.character() %>%  print()
  return(Result)

}

Results =
  lapply(constituencies, Fetchez) %>%
  rbindlist()

saveRDS(object = Results, file = "Resultats")

