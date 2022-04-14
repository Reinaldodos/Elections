pacman::p_load(tidyverse, data.table, rio, RSQLite, ggdendro, magrittr)
input =
  "Presidentielles 2022/data/" %>%
  list.files(full.names = T, pattern = ".json") %>%
  rio::import_list()

input %<>% purrr::transpose()

GLOBAL =
  input$`Global T1` %>%
  rbindlist(idcol = "file") %>%
  janitor::clean_names() %>%
  transmute(file,
            Candidat = x1,
            Voix = nombre %>%
              str_remove_all(pattern = "[^[0-9]]") %>%
              as.integer()) %>%
  filter(Candidat %in% c("Abstentions", "Blancs", "Nuls"))

DATA =
  input$`Scores T1` %>%
  rbindlist(idcol = "file") %>%
  janitor::clean_names() %>%
  transmute(file, Candidat = liste_des_candidats,
            Voix = voix %>%
              str_remove_all(pattern = "[^[0-9]]") %>%
              as.integer()) %>%
  bind_rows(GLOBAL)

DATA %<>%
  filter(
    str_detect(string = file, pattern = "html"),
    str_detect(string = file, pattern = "013055.html",
               negate = TRUE))

DATA %>%
  group_by(Candidat) %>%
  summarise(Voix = sum(Voix)) %>%
  arrange(-Voix) %>%
  data.table()

DATA2 =
  DATA %>%
  group_by(file) %>%
  mutate(Voix = Voix/sum(Voix)) %>% ungroup()

DATA %>%
  ggplot(mapping = aes(x = Voix, y = Candidat)) +
  geom_violin()

KOR =
  DATA2 %>%
  spread(key = Candidat, value = Voix, fill = 0) %>%
  select(-file) %>%
  # t() %>% scale() %>% t() %>%
  # scale() %>%
  cor()

KOR %>% ggcorrplot::ggcorrplot(hc.order = T)
                               # lab = T, type = "lower")

CLUST = KOR %>% dist() %>% hclust()

CLUST %>% ggdendro::ggdendrogram(rotate = T)

Blocs =
  CLUST %>% cutree(k = 7) %>%
  as.data.table(keep.rownames = T) %>%
  rename(Listes = "rn", Groupe = ".") %>%
  arrange(Groupe) %>%
  mutate(
    Bloc = case_when(
      Groupe == 1 ~ "Abstention",
      Groupe == 2 ~ "Contre",
      Groupe == 3 ~ "Droite",
      Groupe == 4 ~ "Gauche",
      Groupe == 5 ~ "Centre",
      Groupe == 6 ~ 'Lassalle',
      Groupe == 7 ~ 'EXD',
      TRUE ~ Listes
    )) %>% as_tibble()

DATA %>%
  inner_join(y = Blocs, by = c("Candidat" = "Listes")) %>%
  group_by(Bloc) %>%
  summarise(Voix = sum(Voix)) %>%
  mutate(Score = scales::percent(Voix/sum(Voix))) %>%
  arrange(-Voix) %>%
  data.table()
