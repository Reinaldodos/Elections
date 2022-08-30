pacman::p_load(magrittr, tidyverse, rio, data.table)
get_formula <- function(input_sankey) {
  noms = names(input_sankey)
  T1 = str_subset(string = noms, pattern = "_t1") %>% paste(collapse = ", ")
  T2 = str_subset(string = noms, pattern = "_t2") %>% paste(collapse = ", ")
  FORMULE = str_c("cbind(",
                  T2,
                  ") ~ cbind(",
                  T1,  ")") %>% as.formula()

  return(FORMULE)
}

Get_reports <- function(input) {
  out.reg =
    ei.reg(
      formula = get_formula(input),
      data = input
    )

  Modele =
    list(
      out.reg$coefficients %>%
        as.data.frame() %>% rownames_to_column("T1") %>%
        gather(key = T2, value = report,-T1),
      out.reg$se %>%
        as.data.frame() %>% rownames_to_column("T1") %>%
        gather(key = T2, value = se,-T1)
    ) %>%
    reduce(.f = inner_join, by = c("T1", "T2"))

  Modele[Modele$report > 1, ]$report = 1
  Modele[Modele$report < 0,]$report = 0

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
  nodes <- data.frame(name = c(as.character(links$T1),
                               as.character(links$T2)) %>% unique())

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$T1, nodes$name) - 1
  links$IDtarget <- match(links$T2, nodes$name) - 1

  # Make the Network
  p <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "REPORT_voix",
    NodeID = "name",
    sinksRight = FALSE)
  return(p)
}

get_input_sankey <- function(input) {
  input_Sankey =
    input %>%
    # semi_join(y = Electorat_stable, by = "rowid") %>%
    tidyr::unite(col = candidat, candidat, Tour) %>%
    group_by(rowid, candidat) %>%
    summarise(voix = sum(voix, na.rm = T), .groups = "drop") %>%
    spread(key = candidat, value = voix, fill = 0)

  input_Sankey %<>% janitor::clean_names()
  # input_Sankey %<>% semi_join(y = Electorat_stable, by = "rowid")

  input_Sankey %<>%
    mutate_at(.vars = vars(contains("_t")),
              .funs = as.integer)

  return(input_Sankey)
}
