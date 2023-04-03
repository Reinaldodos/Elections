pacman::p_load(eiPack)

Modele =
  eiPack::ei.reg(
    data = Donnees,
    formula = cbind(
      Partielle_Abstentions,
      Partielle_Blancs,
      `Partielle_M. CLARACO Robert`,
      `Partielle_M. GARNIER Jean-Marc`,
      `Partielle_M. JOSSINET François-Xavier`,
      `Partielle_Mme FROGER Martine`,
      `Partielle_Mme LAPEYRE Gisèle`,
      `Partielle_Mme TAURINE Bénédicte`,
      `Partielle_Mme TRIBOUT Anne-Sophie`,
      Partielle_Nuls
    ) ~ cbind(
      Legislatives_Abstentions,
      `Legislatives_Anne-Sophie TRIBOUT`,
      `Legislatives_Bénédicte TAURINE`,
      Legislatives_Blancs,
      `Legislatives_Claire COULY`,
      `Legislatives_François-Xavier JOSSINET`,
      `Legislatives_Jean-Marc GARNIER`,
      `Legislatives_Jean-Pierre SALVAT`,
      `Legislatives_Martine FROGER`,
      Legislatives_Nuls,
      `Legislatives_Renaud FABART`,
      `Legislatives_Yannick LOMRÉ`
    )
  )


Reports = 
  list(
  Modele$coefficients %>%
    data.frame() %>%
    rownames_to_column(var = "Legislative") %>%
    gather(key = Partielle, value = "Coef",-Legislative),
  Modele$se %>%
    data.frame() %>%
    rownames_to_column(var = "Legislative") %>%
    gather(key = Partielle, value = "se",-Legislative)
) %>% 
  reduce(.f = inner_join) %>% 
  mutate(
    low = Coef + se * qnorm(p = 0.025),
    high = Coef + se * qnorm(p = 0.975))

Reports %>% 
  mutate_at(.vars = vars(Coef, low, high), .funs = scales::percent, accuracy = .1) %>% 
  filter(se < .1) %>% 
  view()


Reports %>% 
  filter(se < .1,
         low > 0) %>%
  ggplot(mapping = aes(y = Partielle,
                       colour = Legislative)) +
  geom_errorbarh(mapping = aes(xmin = low, xmax = high),
                 position = "dodge") +
  theme(legend.position = "bottom")

Get_sankey <- function(links) {
  pacman::p_load(networkD3)
  nodes <- data.frame(name = c(
    as.character(links$Legislative),
    as.character(links$Partielle)
  ) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$Legislative, nodes$name) - 1
  links$IDtarget <- match(links$Partielle, nodes$name) - 1
  
  # Make the Network
  p <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "Report",
    NodeID = "name",
    fontSize = 16,
    sinksRight = T
  )
  
  return(p)
}

links =
  output_Legislatives %>%
  group_by(Legislative = paste("Legislatives", Candidat, sep = "_")) %>%
  summarise(Voix = sum(Voix)) %>%
  left_join(x = Reports)%>% 
  transmute(
    Legislative = str_remove_all(string = Legislative, pattern = "Legislatives_"), 
    Partielle = str_remove_all(string = Partielle, pattern = "Partielle_"),
            Report = round(Coef * Voix)) 

links = 
  Legislatives_exprimes %>% 
  distinct(Legislative = Candidat, Nuance) %>% 
  right_join(y = links) %>% 
  tidyr::unite(col = Legislative, Legislative, Nuance, sep = " - ")

links %>% 
  filter(Report > 200) %>% 
  Get_sankey()
