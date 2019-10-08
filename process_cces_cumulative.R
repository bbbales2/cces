library(tidyverse)

df = readRDS("cces_cumulative.Rds") %>%
  as_tibble()

df2 = df %>%
  select(economy_retro, approval_pres,
         gender, race, educ, year, birthyr, state, marstat) %>%
  rename(sex = gender) %>%
  drop_na()

df3 = df2 %>%
  mutate(econ_better = economy_retro == 1 | economy_retro == 2,
         approve_pres = approval_pres == 1 | approval_pres == 2) %>%
  select(-economy_retro, -approval_pres) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female"),
         race = factor(sapply(race, function(x) {
           if(x == 3) { return("hispanic") }
           if(x == 1) { return("white") }
           if(x == 2) { return("black") }
           return("other")
           })),
         educ = factor(sapply(educ, function(x) {
           if(x == 1) { return("noHS") }
           if(x == 2) { return("HS") }
           if(x == 3) { return("some_or_assoc") }
           if(x == 4) { return("some_or_assoc") }
           if(x == 5) { return("bach") }
           if(x == 6) { return("bachplus") }
           return("unknown")
         })),
         age = year - birthyr,
         age = factor(sapply(age, function(x) {
           if(x <= 29) { return("18-29") }
           if(x <= 44) { return("30-44") }
           if(x <= 64) { return("45-64") }
           return("65+")
         })),
         marstat = sapply(marstat, function(x) {
           if(x == 1 | x == 6) { return("married") } # married or domestic partnership
           if(x == 5) { return("never married") } # single is never married?
           return("previously married")
         })) %>%
  select(-birthyr) %>%
  mutate(sex = as.character(sex),
         race = as.character(race),
         educ = as.character(educ),
         state = as.character(state),
         age = as.character(age))

write_delim(df3, "cces_regression_cumulative.delim")
