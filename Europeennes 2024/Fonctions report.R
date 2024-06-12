pivot_sankey <- function(input_test) {
  input_sankey =
    input_test %>%
    tidyr::unite(col = bulletin, scrutin, candidat) %>%
    pivot_wider(names_from = bulletin,
                values_from = voix,
                values_fill = 0) %>%
    janitor::clean_names()
  return(input_sankey)
}

get_formula <- function(data) {
  noms = names(data)
  EURO = str_subset(string = noms, pattern = "^euro") %>% paste(collapse = ", ")
  PDT = str_subset(string = noms, pattern = "^pdt") %>% paste(collapse = ", ")
  FORMULE = str_c("cbind(",
                  EURO,
                  ") ~ cbind(",
                  PDT,  ")") %>% as.formula()

  return(FORMULE)
}

get_reg_ecolo <- function(data) {
  out.reg =
    eiPack::ei.reg(formula = get_formula(data),
                   data = data)

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
    group_by(scrutin, candidat) %>%
    summarise(voix = sum(voix), .groups = "drop") %>%
    tidyr::unite(col = bulletin, scrutin, candidat) %>%
    mutate(bulletin = janitor::make_clean_names(bulletin)) %>%
    inner_join(x = coefficients,
               by = join_by(source == bulletin)) %>%
    mutate(REPORT = report * voix) %>%
    return()
}

chaine_report <- function(data) {
  data %>%
    pivot_sankey() %>%
    get_reg_ecolo() %>%
    extract_coefficients() %>%
    append_report(data = data) %>%
    return()
}

Get_Sankey <- function(data) {
  require(networkD3)

  if(is_null(data)){
    return()}

  links =
    data %>%
    filter(REPORT > 500)

  # A connection data frame is a list of flows with intensity for each flow
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <-
    data.frame(name = c(as.character(links$source),
                        as.character(links$target)) %>%
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
    htmlwidgets::appendContent(sankey,
                               htmltools::tags$p("github.com/Reinaldodos"))

  return(sankey)
}


get_web_report <- function(departement, circo, sankey) {
  fichier = sprintf("Europeennes 2024/Reports/%s_%s.html",
                    departement,
                    circo)

  if (!is_null(sankey))

    htmltools::save_html(file = fichier,
                         html = sankey)
}
