require(tidyverse)

redresser_donnees <- function(data_source, data_target) {
  Bureaux_source =
    data_source %>%
    select(code_de_la_commune, code_du_b_vote) %>%
    distinct()

  Bureaux_target =
    data_target %>%
    select(code_de_la_commune, code_du_b_vote) %>%
    distinct() %>%
    inner_join(
      y = Bureaux_source,
      by = join_by(code_de_la_commune, code_du_b_vote)
    )

  input =
    list("target" = data_target,
         "source" = data_source) %>%
    map(.f = semi_join,
        y = Bureaux_target,
        by = join_by(code_de_la_commune, code_du_b_vote)
    ) %>%
    bind_rows(.id = "type") %>%
    discard(.p = ~ any(is.na(.)))

  Redressement =
    input %>%
    group_by(type,
             code_de_la_commune,
             code_du_b_vote) %>%
    summarise(inscrits = sum(voix, na.rm = TRUE), .groups = "drop") %>%
    spread(type, inscrits) %>%
    mutate(DELTA = target - source, candidat = "abstentions") %>%
    mutate(type = case_when(DELTA < 0 ~ "target",
                            DELTA > 0 ~ "source"),
           voix = abs(DELTA)) %>%
    drop_na(type) %>%
    select(-source, -target, -DELTA)

  output =
    bind_rows(input,
              Redressement) %>%
    group_by(type, code_de_la_commune, code_du_b_vote,
             candidat) %>%
    summarise(voix = sum(voix, na.rm = TRUE),
              .groups = "drop") %>%
    right_join(x = Bureaux_source,
               by = join_by(code_de_la_commune, code_du_b_vote))

  return(output)
}

get_formula <- function(data) {
  Noms =
    split(x = data$candidat, f = data$type) %>%
    map(.f = as_factor) %>%
    map(.f = levels) %>%
    map(janitor::make_clean_names) %>%
    imap(.f = \(x, idx) str_c(idx, x, sep = "_")) %>%
    map(.f = paste, collapse = ", ")


  FORMULE = str_c("cbind(", Noms$target, ") ~ cbind(", Noms$source, ")") %>%
    as.formula()

  return(FORMULE)
}

pivot_sankey <- function(output) {
  output %>%
    tidyr::unite(col = bulletin, type, candidat) %>%
    pivot_wider(
      names_from = bulletin,
      values_from = voix,
      values_fill = 0
    ) %>%
    janitor::clean_names()
}

get_reg_ecolo <- function(data) {
  out.reg =
    eiPack::ei.reg(formula = get_formula(data),
                   data = pivot_sankey(data)
    )

  return(out.reg)
}

extract_coefficients <- function(out.reg) {
  output_ecolo =
    list(
      out.reg$coefficients %>%
        as.data.frame() %>%
        rownames_to_column(var = "source") %>%
        pivot_longer(
          cols = -source,
          names_to = "target",
          values_to = "report"
        ),
      out.reg$se %>%
        as.data.frame() %>%
        rownames_to_column(var = "source") %>%
        pivot_longer(
          cols = -source,
          names_to = "target",
          values_to = "se"
        )
    ) %>%
    reduce(.f = inner_join, by = join_by(source, target))

  return(output_ecolo)
}

append_report <- function(data, coefficients) {
  data %>%
    group_by(type, candidat) %>%
    summarise(voix = sum(voix), .groups = "drop") %>%
    tidyr::unite(col = bulletin, type, candidat) %>%
    mutate(bulletin = janitor::make_clean_names(bulletin)) %>%
    inner_join(x = coefficients, by = join_by(source == bulletin)) %>%
    mutate(REPORT = report * voix) %>%
    return()
}

Get_Sankey <- function(data) {
  require(networkD3)

  if (is_null(data)) {
    return()
  }

  links =
    data %>%
    filter(REPORT > 500)

  # A connection data frame is a list of flows with intensity for each flow
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <-
    data.frame(name = c(as.character(links$source), as.character(links$target)) %>%
                 unique())

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1

  # Make the Network
  sankey <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "REPORT",
    NodeID = "name",
    sinksRight = FALSE,
    units = "voix"
  )

  # sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h1("Title"))
  sankey <-
    htmlwidgets::appendContent(sankey, htmltools::tags$p("github.com/Reinaldodos"))

  return(sankey)
}

