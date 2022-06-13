Resultats =
  Circos %>% setdiff(Selection$url) %>%
  set_names() %>%
  map(safe_results) %>%
  purrr::transpose() %>% map(compact)

Done = Resultats %$% result

to_numeric = function(string) {
  string %>% str_replace(pattern = ",", replacement = ".") %>%
    as.numeric() %>% return()
}

Selection =
  Done %>%
  map(reduce, cbind.data.frame) %>%
  rbindlist(idcol = "url") %>%
  list(., Selection) %>% rbindlist() %>%
  distinct() %>%
  mutate_at(.vars = c("% Inscrits", "% Exprimés"), .funs = to_numeric)

Elus =
  Selection %>%
  filter(`% Exprimés` > 50, `% Inscrits` > 25)

Second_tour =
  Selection %>%
  anti_join(y = Elus, by = "out") %>%
  group_by(out) %>%
  mutate(Rank = dense_rank(-`% Exprimés`)) %>%
  ungroup() %>%
  filter(Rank <= 2 | `% Inscrits` > 12.5)

list(Elus, Second_tour) %>% rbindlist(fill = T) %>%
  anti_join(x = Selection, by = "out")

list(Elus, Second_tour) %>% rbindlist(fill = T) %>%
  group_by(Nuances) %>%
  tally(n_distinct(out)) %>% arrange(-n)

Battles =
  list(Elus, Second_tour) %>% rbindlist(fill = T) %>%
  group_by(out) %>%
  arrange(Nuances) %>%
  summarise(Candidats = paste(Nuances, collapse = " / "))

Battles%>%
  count(Candidats) %>%
  arrange(-n) %>%
  janitor::adorn_totals() %>%
  view()
