Get_reports <- function(input) {
  out.reg =
    ei.reg(
      formula =
        cbind(abstentions_t2, macron_t2, le_pen_t2, blancs_t2, nuls_t2) ~
        cbind(
          abstentions_t1,
          arthaud_t1,
          blancs_t1,
          dupont_aignan_t1,
          hidalgo_t1,
          jadot_t1,
          lassalle_t1,
          le_pen_t1,
          macron_t1,
          melenchon_t1,
          nuls_t1,
          pecresse_t1,
          poutou_t1,
          roussel_t1,
          zemmour_t1
        ),
      data = input
    )

  Modele =
    list(
      out.reg$coefficients %>%
        as.data.frame() %>% rownames_to_column("T1") %>%
        gather(key = T2, value = report, -T1),
      out.reg$se %>%
        as.data.frame() %>% rownames_to_column("T1") %>%
        gather(key = T2, value = se, -T1)
    ) %>%
    reduce(.f = inner_join, by = c("T1", "T2"))

  # Modele[Modele$report > 1,]$report = 1
  # Modele[Modele$report < 0, ]$report = 0

  Modele =
    input %>%
    select_if(.predicate = is.numeric) %>%
    summarise_all(.funs = sum) %>%
    gather(key = T1, value = voix) %>%
    inner_join(x = Modele, by = "T1") %>%
    mutate(REPORT_voix = voix * report)

  return(Modele)
}
safe_Report = safely(Get_reports)
Get_safe_report <- function(input, size) {
  RAPPORT = safe_Report(input = sample_frac(tbl = input, size = size))
  while (is_null(RAPPORT$result)) {
    RAPPORT = safe_Report(input = sample_frac(tbl = input, size = size))
  }
  return(RAPPORT$result)
}

Get_Sankey <- function(links) {
  pacman::p_load(networkD3)
  # A connection data frame is a list of flows with intensity for each flow
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
  print("DONE")
  return(p)
}
