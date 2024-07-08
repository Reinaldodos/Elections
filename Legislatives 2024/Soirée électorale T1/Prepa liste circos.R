source(file = "Legislatives 2024/Soirée électorale/Fonctions.R",
       encoding = "UTF-8")

url_web = "https://www.resultats-elections.interieur.gouv.fr/legislatives2024/index.html"

Liste_Departements =
  url_web %>%
  read_html() %>%
  html_elements(xpath = "/html/body/main/div[2]/div/div[2]/div/div/div/div/select") %>%
  html_children()

Departements =
  cbind.data.frame(
    url = Liste_Departements %>%
      html_attr(name = "value"),
    Departement = Liste_Departements %>%
      html_text() %>%
      str_trim()
  ) %>%
  filter(url %>%
           str_detect(pattern = "html$"))

Circos =
  Departements %>%
  mutate(
    Circos = url %>%
      map(
        .f = get_circos,
        urlweb = url_web,
        XPATH = "/html/body/main/div[2]/div/div[2]/div[2]/div[2]/div/div/select",
        .progress = TRUE
      )
  ) %>%
  unnest(cols = c(Circos)) %>%
  filter(url_circo %>%
           str_detect(pattern = "html$"))

Paris =
  Departements %>%
  filter(Departement %>%
           str_detect(pattern = "^75"))

Circos =
  Paris %>%
  mutate(
    Circos = url %>%
      map(
        .f = get_circos,
          urlweb = url_web,
          XPATH = "/html/body/main/div[2]/div/div[2]/div[2]/div/div/div/select",
        .progress = TRUE
      )
  ) %>%
  unnest(cols = c(Circos)) %>%
  filter(url_circo %>%
           str_detect(pattern = "html$")) %>%
  bind_rows(Circos)

input =
  Circos %>%
  transmute(
    Departement,
    circo,
    url = build_url(urlweb = url, url_dpt = url_circo) %>%
      build_url(urlweb = url_web)
  )

input %>%
  saveRDS(file = "Legislatives 2024/Soirée électorale/liste circos.rds")
