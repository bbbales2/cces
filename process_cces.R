library(tidyverse)
library(foreign)

df = read.dta("cces16.dta") %>%
  as_tibble()

df2 = df %>%
  select(CC16_410a, gender, race, educ, birthyr, inputstate, marstat, commonweight, CL_E2016GVM) %>%
  rename(vote = CC16_410a,
         state = inputstate,
         sex = gender,
         weight = commonweight,
         vote_validated = CL_E2016GVM)

df2 = df2 %>%
  drop_na() %>%
  mutate(vote_validated = vote_validated != "")
  #filter(votetype != "")

df3 = df2 %>%
  filter(marstat != 8) %>% # Some people skipped reporting marital status
  mutate(age = 2016 - birthyr) %>%
  mutate(educ = factor(sapply(educ, function(x) {
    if(x == "No HS") { return("noHS") }
    if(x == "High school graduate") { return("HS") }
    if(x == "Some college") { return("some_or_assoc") }
    if(x == "2-year") { return("some_or_assoc") }
    if(x == "4-year") { return("bach") }
    if(x == "Post-grad") { return("bachplus") }
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
  })),
  marstat = sapply(marstat, function(x) {
    if(x %in% c("Married", "Domestic partnership")) { return("married") } # married or domestic partnership
    if(x == "Single") { return("never married") } # single is never married?
    return("previously married")
    })) %>%
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

write_delim(df3, "cces_regression.delim")

  
