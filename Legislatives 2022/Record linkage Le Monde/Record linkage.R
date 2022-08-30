pacman::p_load(rio, magrittr,
               data.table, tidyverse)

load(file = "Legislatives 2022/Record linkage Le Monde/donnees.Rdata")

LeMonde %<>% mutate_all(.funs = type.convert)
Candidats %<>% mutate_all(.funs = type.convert)
# Nettoyer Le Monde -------------------------------------------------------
LeMonde %<>%
  mutate(sexe = recode(.x = civilite,
                       "M."= "M",
                       "Mme" = "F"
                       )) %>%
  rename(num_panneau = numero_panneau,
         code_de_la_circonscription = numero_circonscription)

clean_string <- function(string) {
  string %>%
    stringi::stri_trans_general(id = "latin-ascii") %>%
    stringr::str_to_lower() %>%
    return()
}

LeMonde %<>%
  mutate_at(.vars = c("nom", "prenom"),
            .funs = clean_string)

Candidats %<>%
  mutate_at(.vars = c("nom", "prenom"),
            .funs = clean_string)


# Record linkage ----------------------------------------------------------
pacman::p_load(fastLink)

output =
  fastLink::fastLink(
  dfA = Candidats,
  dfB = LeMonde,
  varnames = setdiff(names(Candidats),
                     c("code_du_departement", "departement")
                     ),
  verbose = TRUE,
  return.df = TRUE)

Paires =
  list(
  output$dfA.match,
  output$dfB.match) %>%
  reduce(.f = cbind.data.frame) %>%
  janitor::clean_names()

Candidats =
  Paires %>%
  distinct(departement, code_du_departement) %>%
  left_join(x = Candidats, by = "code_du_departement")

FINAL =
  full_join(
    x = Candidats %>% drop_na(num_panneau),
    y = LeMonde,
    by = c("departement", "code_de_la_circonscription", "num_panneau"))

FINAL %>%
  select(sort(names(FINAL))) %>%
  view()

FINAL %>%
  drop_na() %>%
  anti_join(x = FINAL) %>% view()


Nuances =
  FINAL %>% drop_na() %>%
  select(code_du_departement, code_de_la_circonscription, num_panneau,
        contains("nuance"), contains("agrege"))

Scores_nuance =
  Nuances %>%
  inner_join(y = Bureaux %>% mutate_all(type.convert)) %>%
  inner_join(y = Scores%>% mutate_all(type.convert))

Scores_nuance %>%
  saveRDS(
    file = "Legislatives 2022/Record linkage Le Monde/scores_nuances.rds")
