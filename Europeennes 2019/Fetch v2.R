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

names(output)=names(output) %>% iconv(from = "Latin1", to = "UTF-8")

output =
  output %>%
  mutate_all(.funs = ~ iconv(from = "Latin1", to = "UTF-8", x = .))

output =
  output %>% mutate_all(type.convert)

output %>%
  group_by(Listes, `Libellé de la commune`) %>%
  summarise(TOT = sum(Voix)) %>% ungroup %>%
  group_by(`Libellé de la commune`) %>%
  mutate(Exprimes = sum(TOT)) %>% ungroup %>%
  ggplot(mapping = aes(
    x = Exprimes,
    y = TOT / Exprimes,
    colour = Listes
  )) +
  geom_smooth() +
  scale_x_log10()

