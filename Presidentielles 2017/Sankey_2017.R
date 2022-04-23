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

out.reg =
  ei.reg(
    formula =
      cbind(abstentions_t2, macron_t2, le_pen_t2, blancs_t2, nuls_t2) ~
      cbind(
        abstentions_t1,
        arthaud_t1,
        asselineau_t1,
        blancs_t1,
        cheminade_t1,
        dupont_aignan_t1,
        fillon_t1,
        hamon_t1,
        lassalle_t1,
        le_pen_t1,
        macron_t1,
        melenchon_t1,
        nuls_t1,
        poutou_t1),
    data = input %>% sample_n(size = 20000))

Modele =
  list(
    out.reg$coefficients %>%
      as.data.frame() %>% rownames_to_column("T1") %>%
      gather(key = T2, value = report,-T1),
    out.reg$se %>%
      as.data.frame() %>% rownames_to_column("T1") %>%
      gather(key = T2, value = se,-T1)
  ) %>%
  reduce(.f = inner_join)

Modele[Modele$report > 1, ]$report = 1
Modele[Modele$report < 0,]$report = 0

Modele =
  input %>%
  summarise_all(.funs = sum) %>%
  gather(key = T1, value = voix) %>%
  inner_join(x= Modele) %>%
  mutate(REPORT_voix = voix*report)

# Sankey diagram ----------------------------------------------------------
pacman::p_load(networkD3)

# A connection data frame is a list of flows with intensity for each flow
links <- Modele

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
