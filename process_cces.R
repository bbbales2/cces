library(tidyverse)
library(foreign)

df = read.dta("cces16.dta") %>%
  as_tibble()

df2 = df %>%
  select(CC16_410a, gender, race, educ, birthyr, inputstate, commonweight) %>%
  rename(vote = CC16_410a,
         state = inputstate,
         sex = gender,
         weight = commonweight) %>%
  drop_na()

df3 = df2 %>%
  mutate(age = 2016 - birthyr) %>%
  mutate(educ = factor(sapply(educ, function(x) {
    if(x == "No HS") { return("noHS") }
    if(x == "High school graduate") { return("HS") }
    if(x == "Some college") { return("some") }
    if(x == "2-year") { return("assoc") }
    if(x == "4-year") { return("bach") }
    if(x == "Post-grad") { return("bachp") }
    return("unknown")
  })),
  race = factor(sapply(race, function(x) {
    if(x == "Hispanic") { return("hispanic") }
    if(x == "White") { return("white") }
    if(x == "Black") { return("black") }
    return("other")
  })),
  age = factor(sapply(age, function(x) {
    if(x <= 29) { return("18-29") }
    if(x <= 44) { return("30-44") }
    if(x <= 64) { return("45-64") }
    return("65+")
  }))) %>%
  mutate(vote = fct_recode(vote,
                  trump = "Donald Trump (Republican)",
                  hillary = "Hillary Clinton (Democrat)",
                  gary = "Gary Johnson (Libertarian)",
                  jill = "Jill Stein (Green)",
                  other = "Other",
                  novote = "I didn't vote in this election",
                  notsure = "I'm not sure",
                  evan = "Evan McMullin (Independent)",
                  skipped = "Skipped",
                  notasked = "Not Asked")) %>%
  select(-birthyr) %>%
  mutate(vote = as.character(vote),
         sex = as.character(sex),
         race = as.character(race),
         educ = as.character(educ),
         state = as.character(state),
         age = as.character(age))

write_delim(df3 %>%
              filter(vote %in% c("trump", "hillary")),
            "cces_regression.delim")
  