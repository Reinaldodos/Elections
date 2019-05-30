PARIS =
  tbl(src = BASE, "Bureaux") %>%
  filter(Code.du.departement == "75") %>% as.data.table()

input=
  input %>%
  mutate(arrondissem = str_c("0", arrondissem) %>% str_sub(start = -2),
         num_bv = str_c("0", num_bv) %>% str_sub(start = -2)) %>%
  mutate(id_bv = str_c(arrondissem, num_bv)) %>%
  inner_join(y = PARIS, by = c("id_bv" = "Code.du.b.vote"))
