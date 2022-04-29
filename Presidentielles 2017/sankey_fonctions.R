replace_sample = function(data, N) {
  N = min(N, nrow(data))
  data %>%
    sample_n(size = N) %>%
    return()
}

Get_reports <- function(input) {
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
          poutou_t1
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

  Modele[Modele$report > 1,]$report = 1
  Modele[Modele$report < 0, ]$report = 0

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

Get_reports_ABS <- function(input) {
  out.reg =
    ei.reg(
      formula =
        cbind(abstentions_t2, macron_t2, le_pen_t2) ~
        cbind(
          abstentions_t1,
          arthaud_t1,
          asselineau_t1,
          cheminade_t1,
          dupont_aignan_t1,
          fillon_t1,
          hamon_t1,
          lassalle_t1,
          le_pen_t1,
          macron_t1,
          melenchon_t1,
          poutou_t1
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

  Modele[Modele$report > 1,]$report = 1
  Modele[Modele$report < 0, ]$report = 0

  Modele =
    input %>%
    select_if(.predicate = is.numeric) %>%
    summarise_all(.funs = sum) %>%
    gather(key = T1, value = voix) %>%
    inner_join(x = Modele, by = "T1") %>%
    mutate(REPORT_voix = voix * report)

  return(Modele)
}
safe_Report_ABS = safely(Get_reports_ABS)
Get_safe_report_ABS <- function(input, size) {
  RAPPORT = safe_Report_ABS(input = sample_frac(tbl = input, size = size))
  while (is_null(RAPPORT$result)) {
    RAPPORT = safe_Report_ABS(input = sample_frac(tbl = input, size = size))
  }
  return(RAPPORT$result)
}
