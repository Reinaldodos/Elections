pacman::p_load(eiPack)
BASE = src_sqlite(path = "Presidentielles 2017/BASE", create = F)

Exprimes_T1 =
  tbl(src = BASE, "Resultats") %>%
  collect() %>% spread(Listes, Voix)
output_T1 =
  tbl(src = BASE, "Chiffres_globaux") %>%
  select(rowid, Abstentions, Blancs, Nuls) %>%
  collect() %>%
  inner_join(y = Exprimes_T1, by = "rowid") %>%
  gather(key = candidat, value = voix, -rowid, na.rm = T)

Electorat_stable =
  list(
    output_T1 %>%
      group_by(rowid) %>%
      summarise(Tot_T1 = sum(voix)),
    output_T2 %>%
      group_by(rowid) %>%
      summarise(Tot_T2 = sum(voix))
  ) %>% reduce(.f = inner_join) %>%
  filter(Tot_T1 == Tot_T2)

input =
  list(
  "T1" = output_T1,
  "T2" = output_T2) %>%
  bind_rows(.id = "Tour") %>%
  tidyr::unite(col = candidat, candidat, Tour) %>%
  spread(key = candidat, value = voix, fill = 0)

input %<>% janitor::clean_names()
input %<>% semi_join(y = Electorat_stable, by = "rowid")

input %<>%
  mutate_at(.vars = vars(contains("_t")),
            .funs = as.integer)


# Sankey diagram ----------------------------------------------------------
pacman::p_load(networkD3)

# A connection data frame is a list of flows with intensity for each flow
links <- TEST

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$T1),
         as.character(links$T2)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$T1, nodes$name)-1
links$IDtarget <- match(links$T2, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "REPORT_voix", NodeID = "name",
                   sinksRight=FALSE)
p

# save the widget
pacman::p_load(htmlwidgets)
saveWidget(p,
           file = paste0(getwd(), "/Sankey/reports_2017.html"),
           selfcontained = TRUE)
