pacman::p_load(rio, tidyverse, magrittr)


# Fetch partielle ---------------------------------------------------------

input = "Partielle_Ariege.csv" %>% import(fill = TRUE, sep = "|", 
                                          skip = 1, header = FALSE) %>% 
  tidyr::extract(
    col = V1, 
    into = c("Commune",
             "Code commune", "BV", "Circo", "Canton", "Inscrits prévus", 
             "Inscrits", "Abstentions", "Votants", "Blancs", "Nuls", "Exprimés",
             "Mme LAPEYRE Gisèle",
             "Mme TAURINE Bénédicte",
             "M. JOSSINET François-Xavier",
             "M. GARNIER Jean-Marc",
             "Mme FROGER Martine",
             "M. CLARACO Robert",
             "Mme TRIBOUT Anne-Sophie"
             ),
    regex = "([^0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*) ([0-9]*)"
      ) %>% 
  mutate_all(.funs = type.convert)

output =
  input %>% 
  select(-Inscrits, -`Inscrits prévus`, -Votants, -Exprimés) %>% 
  gather(key = Candidat, value = Voix, 
         Abstentions:`Mme TRIBOUT Anne-Sophie`) 


# Fetch legislatives ------------------------------------------------------

Legislatives = 
  "https://www.data.gouv.fr/fr/datasets/r/d6d0e0c4-ff00-43bf-a0c9-c195b71f223a" %>%
  # "resultats-par-niveau-burvot-t1-france-entiere.xlsx" %>%
  rio::import(format = "xlsx") %>% 
  discard(.p = ~all(is.na(.))) %>% 
  filter(`Code du département`=="09",
         `Code de la circonscription`=="01") %>%  
  mutate_all(.funs = type.convert)

Bureaux = Legislatives[,1:7]

output %<>% 
  select(`Code de la commune` = `Code commune`, 
         `Code du b.vote` = BV, Candidat, Voix)
output %<>%
  mutate(`Code du b.vote` = case_when(`Code de la commune` == 42 ~ 1,
                                      TRUE ~ as.numeric(`Code du b.vote`)))

Get_data <- function(N) {
  Nom = Legislatives[,24+8*N]
  Prenom = Legislatives[,25+8*N]
  Nuance = Legislatives[,26+8*N]
  Voix = Legislatives[,27+8*N]
  
  cbind.data.frame(Bureaux, 
                   Candidat = paste(Prenom, Nom),
                   Nuance, Voix) %>% 
    return()
}
Legislatives_exprimes = 
  map(.x = 1:9, .f = Get_data) %>% 
  bind_rows()

output_Legislatives = 
  Legislatives %>% 
  select(Abstentions, Blancs, Nuls) %>% 
  cbind.data.frame(Bureaux, .) %>% 
  gather(key = Candidat, value = Voix, 
         Abstentions, Blancs, Nuls) %>% 
  bind_rows(Legislatives_exprimes) 


# Match abstentions -------------------------------------------------------
DELTA =
  list(
    output %>%
      group_by(`Code de la commune`, `Code du b.vote`) %>%
      summarise(
        Total_partielle = sum(Voix, na.rm = TRUE),
        .groups = "drop"
      ),
    output_Legislatives %>%
      group_by(`Code de la commune`, `Code du b.vote`) %>%
      summarise(
        Total_legislative = sum(Voix, na.rm = TRUE),
        .groups = "drop"
      )
  ) %>%
  reduce(.f = full_join) %>%
  mutate(DELTA = Total_legislative - Total_partielle) %>%
  filter(DELTA != 0) %>% split(f = .$DELTA > 0)

Abstentions_legislatives = 
  DELTA$`FALSE` %>% 
  select(-starts_with("Total")) %>% 
  inner_join(x = output_Legislatives %>% 
              filter(Candidat == "Abstentions")) %>% 
  mutate(Voix = Voix - DELTA)

output_Legislatives %<>% 
  anti_join(y = Abstentions_legislatives,
  by = c("Code de la commune", "Code du b.vote", "Candidat")) %>% 
  bind_rows(Abstentions_legislatives) %>% 
  discard(.p = ~any(is.na(.)))

Abstentions_partielle = 
  DELTA$`TRUE` %>% 
  select(-starts_with("Total")) %>% 
  inner_join(x = output %>% 
              filter(Candidat == "Abstentions")) %>% 
  mutate(Voix = Voix + DELTA)

output %<>% 
  anti_join(y = Abstentions_partielle,
  by = c("Code de la commune", "Code du b.vote", "Candidat")) %>% 
  bind_rows(Abstentions_partielle) %>% 
  discard(.p = ~any(is.na(.)))

anti_join(x = output_Legislatives,
          y = output,
         by = c("Code de la commune", "Code du b.vote"))
anti_join(y = output_Legislatives,
          x = output,
         by = c("Code de la commune", "Code du b.vote"))

Donnees = 
  list("Legislatives" = output_Legislatives,
       "Partielle" = output) %>%
  bind_rows(.id = "Type") %>%
  tidyr::unite(col = Candidat, Type, Candidat) %>%
  select(`Code du b.vote`, `Code de la commune`, Candidat, Voix) %>%
  spread(Candidat, Voix)
