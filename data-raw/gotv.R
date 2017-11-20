library(dplyr)
library(haven)
library(readr)
library(forcats)
library(stringr)

raw <- read_dta(url("http://hdl.handle.net/10079/xpnvx77"))

gotv <- raw %>%
  mutate(age = 2006 - yob,
         n_general = rowSums(select(., g2000, g2002, g2004)),
         n_primary = rowSums(select(., p2000, p2002, p2004))) %>%
  as_factor() %>%
  # fix leading space in treatment levles
  mutate(treatment = fct_relabel(treatment, str_trim)) %>%
  select(treatment, voted, hh_size, age, sex, n_general, n_primary,
         g2000, g2002, g2004, p2000, p2002, p2004)

write_csv(gotv, "data-raw/gotv.csv")
save(gotv, file = "data/gotv.rda", compress = "bzip2")
