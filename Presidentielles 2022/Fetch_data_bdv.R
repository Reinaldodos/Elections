pacman::p_load(tidyverse, magrittr, data.table, janitor, readxl)

input <- "Presidentielles 2022/resultats-par-niveau-burvot-t1-france-entiere.xlsx" %>%
  readxl::read_excel()

input %<>% janitor::clean_names()
input <- input %>% janitor::clean_names()


CLINNE <- function(data) {
  names(data) = c("rowid", "candidat", "voix")
  return(data)
}
Scores =
  # Generate indices for extracting candidate data columns (7*0:11 creates a sequence: 0, 7, 14, ..., 77)
  map(.x = 7*0:11, .f = ~input[,c(1, 25 + ., 27 + .)]) %>%
  map(.f = CLINNE) %>%
  rbindlist()
Bureaux = input[, 1:8]

Abstention =
  input %>% select(rowid, abstentions, blancs, nuls) %>%
  pivot_longer(cols = c("abstentions", "blancs", "nuls"),
               names_to = "candidat",
               values_to = "voix")

output_T1 =
  bind_rows(Scores, Abstention) %>%
  inner_join(x = Bureaux, by = "rowid")



output_T1 %<>%
  # Grouping by 'rowid' and calculating the score as the proportion of 'voix' within each group.
  # Note: This step may be computationally expensive for large datasets. Consider using data.table for optimization if needed.
  group_by(rowid) %>%
  mutate(score = voix/sum(voix)) %>%
  ungroup()

# Removing large intermediate variables to free up memory, as they are no longer needed.
rm(input, Scores, Abstention)
# Explicitly calling garbage collection to free up memory after removing large objects
gc()
