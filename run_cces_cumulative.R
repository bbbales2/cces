library(tidyverse)
library(brms)
library(mapdata)
library(parallel)

# vote 1 means voted for trump, vote 0 means hillary
# sex -0.5 means male, 0.5 female
preprocess = function(df) {
  df %>%
    mutate(male = -0.5 + 1 * (sex == "Male"),
           educ = factor(educ, levels = c("noHS", "HS", "some_or_assoc", "bach", "bachplus")),
           age = factor(age, levels = c("18-29", "30-44", "45-64", "65+"))) %>%
    select(-sex)
}

cces = read_delim("cces_regression_cumulative.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  preprocess

pums = read_delim("poststrat_table.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  preprocess

for(name in c("male", "race", "educ", "state", "age", "marstat")) {
  unique_cces = unique(cces %>% pull(name))
  unique_pums = unique(pums %>% pull(name))
  
  if(length(setdiff(unique_cces, unique_pums)) > 0) {
    stop(paste("Column", name, "does not have the same elements in the cces and pums data"))
  }
}

#vote ~ sex +
#(1 | state) + (1 | race) + (1 | educ) + (1 | age) + (1 | marstat) +
#  (1 | marstat:age) + (1 | marstat:educ) + (1 | marstat:race) + (1 | marstat:state) + (1 | marstat:sex) + 
#  (1 | state:age) + (1 | state:educ) +(1 | state:race) + (1 | state:sex) + 
#  (1 | race:age) + (1 | race:educ) + (1 | race:sex) +
#  (1 | educ:age) + (1 | educ:sex) +
#  (1 | age:sex)

#vote ~ sex +
# (1 | state) + (1 | race) + (1 | educ) + (1 | age) +
# (1 | race:state) + (1 | educ:state) + (1 | age:state)

cces_binomial = cces %>%
  group_by(male, state, race, educ, age, marstat, year) %>%
  summarise(N = n(),
            econ_betters = sum(econ_better),
            approve_pres = sum(approve_pres)) %>%
  arrange(-N) %>%
  ungroup()

years = cces_binomial %>%
  pull(year) %>%
  unique

efits = list()
afits = list()

for(i in 1:length(years)) {
  cces_binomial_year = cces_binomial %>%
    filter(year == years[i])
  
  efits[[i]] = brm(econ_betters | trials(N) ~ male +
                     (1 | race) + (1 | educ) + (1 | age) + (1 | marstat) +
                     (1 + male | state) + (male - 1 | race) + (male - 1 | educ) + (male - 1 | age) + (male - 1 | marstat) +
                     (1 | race:educ) + (1 | race:age) + (1 | race:marstat) +
                     (1 | educ:age) + (1 | educ:marstat) +
                     (1 | age:marstat),
                   data = cces_binomial_year,
                   family = "binomial",
                   cores = 8,
                   init = 0,
                   iter = 500,
                   prior = set_prior("normal(0, 1)", class = "b") +
                     set_prior("normal(0, 1)", class = "Intercept") +
                     set_prior("normal(0, 1)", class = "sd"),
                   control = list(adapt_delta = 0.9))
  
  afits[[i]] = brm(approve_pres | trials(N) ~ male +
                     (1 | race) + (1 | educ) + (1 | age) + (1 | marstat) +
                     (1 + male | state) + (male - 1 | race) + (male - 1 | educ) + (male - 1 | age) + (male - 1 | marstat) +
                     (1 | race:educ) + (1 | race:age) + (1 | race:marstat) +
                     (1 | educ:age) + (1 | educ:marstat) +
                     (1 | age:marstat),
                   data = cces_binomial_year,
                   family = "binomial",
                   cores = 8,
                   init = 0,
                   iter = 500,
                   prior = set_prior("normal(0, 1)", class = "b") +
                     set_prior("normal(0, 1)", class = "Intercept") +
                     set_prior("normal(0, 1)", class = "sd"),
                   control = list(adapt_delta = 0.9))
}

predictdf = mclapply(1:length(years), function(i) {
  epredicted_d = fitted(efits[[i]], newdata = pums, allow_new_levels = TRUE, summary = FALSE)
  apredicted_d = fitted(afits[[i]], newdata = pums, allow_new_levels = TRUE, summary = FALSE)
  
  bind_rows(epredicted_d[sample(1:nrow(epredicted_d), 20), ] %>% t %>%
              as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
              bind_cols(pums) %>%
              pivot_longer(starts_with("predicted_"), names_to = c("rep"), names_pattern = "predicted_([0-9]+)", values_to = "predicted") %>%
              mutate(predicted = predicted / N) %>%
              mutate(year = years[i],
                     outcome = "econ_better"),
            apredicted_d[sample(1:nrow(apredicted_d), 20), ] %>% t %>%
              as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
              bind_cols(pums) %>%
              pivot_longer(starts_with("predicted_"), names_to = c("rep"), names_pattern = "predicted_([0-9]+)", values_to = "predicted") %>%
              mutate(predicted = predicted / N) %>%
              mutate(year = years[i],
                     outcome = "approve_pres"))
}, mc.cores = 2) %>% bind_rows

saveRDS(predictdf, "shiny/basic.Rds")
write_delim(predictdf, "shiny/basic.delim")
