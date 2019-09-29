library(tidyverse)
library(ggplot2)
library(brms)
library(rstan)

df1 = read_csv("csv_pus/ss16pusa.csv")
df2 = read_csv("csv_pus/ss16pusb.csv")

df1 = df1 %>%
  mutate(ST = as.character(ST))
df2 = df2 %>%
  mutate(ST = as.character(ST))

df = bind_rows(df1, df2)

rm(df1, df2)

# CCES State
state_lookup = list("01" = "Alabama",
                    "02" = "Alaska",
                    "04" = "Arizona",
                    "05" = "Arkansas",
                    "06" = "California",
                    "08" = "Colorado",
                    "09" = "Connecticut",
                    "10" = "Delaware",
                    "11" = "District of Columbia",
                    "12" = "Florida",
                    "13" = "Georgia",
                    "15" = "Hawaii",
                    "16" = "Idaho",
                    "17" = "Illinois",
                    "18" = "Indiana",
                    "19" = "Iowa",
                    "20" = "Kansas",
                    "21" = "Kentucky",
                    "22" = "Louisiana",
                    "23" = "Maine",
                    "24" = "Maryland",
                    "25" = "Massachusetts",
                    "26" = "Michigan",
                    "27" = "Minnesota",
                    "28" = "Mississippi",
                    "29" = "Missouri",
                    "30" = "Montana",
                    "31" = "Nebraska",
                    "32" = "Nevada",
                    "33" = "New Hampshire",
                    "34" = "New Jersey",
                    "35" = "New Mexico",
                    "36" = "New York",
                    "37" = "North Carolina",
                    "38" = "North Dakota",
                    "39" = "Ohio",
                    "40" = "Oklahoma",
                    "41" = "Oregon",
                    "42" = "Pennsylvania",
                    "44" = "Rhode Island",
                    "45" = "South Carolina",
                    "46" = "South Dakota",
                    "47" = "Tennessee",
                    "48" = "Texas",
                    "49" = "Utah",
                    "50" = "Vermont",
                    "51" = "Virginia",
                    "53" = "Washington",
                    "54" = "West Virginia",
                    "55" = "Wisconsin",
                    "56" = "Wyoming",
                    "72" = "Puerto Rico")

# ACS to Race
# White, ACS 1
# Black, ACS 2
# Hispanic, ACS Anyone with HISP != 1
# Other

# RAC1P 1
# Recoded detailed race code
#1 .White alone
#2 .Black or African American alone
#3 .American Indian alone
#4 .Alaska Native alone
#5 .American Indian and Alaska Native tribes specified; or American
#.Indian or Alaska Native, not specified and no other races
#6 .Asian alone
#7 .Native Hawaiian and Other Pacific Islander alone
#8 .Some Other Race alone
#9 .Two or More Races

#MAR 1
#Marital status
#1 .Married
#2 .Widowed
#3 .Divorced
#4 .Separated
#5 .Never married or under 15 years old

df2 = df %>%
  select(SEX, ST, RAC1P, SCHL, AGEP, HISP, MAR, PWGTP) %>%
  rename(sex = SEX,
         state = ST,
         race = RAC1P,
         educ = SCHL,
         age = AGEP,
         hispanic = HISP,
         marstat = MAR,
         weight = PWGTP) %>%
  mutate(educ = as.integer(educ),
         hispanic = as.integer(hispanic),
         age = as.numeric(age),
         weight = as.numeric(weight)) %>%
  filter(age >= 18)

df3 = df2 %>%
  mutate(sex = sapply(sex, function(x) {
    if(x == 1) { return("Male") }
    return("Female")
  }),
    educ = sapply(educ, function(x) {
    if(is.na(x)) { return("unknown") }
    if(x <= 11) { return("noHS") }
    if(x <= 17) { return("HS") }
    if(x <= 20) { return("some_or_assoc") }
    if(x == 21) { return("bach") }
    return("bachplus")
    }),
    race = map2(race, hispanic, function(race, hispanic) {
      if(hispanic != 1) { return("hispanic") }
      if(race == 1) { return("white") }
      if(race == 2) { return("black") }
      return("other")
      }),
    state = sapply(state, function(x) {
      state_lookup[[x]]
    }),
    age = sapply(age, function(x) {
      if(x <= 29) { return("18-29") }
      if(x <= 44) { return("30-44") }
      if(x <= 64) { return("45-64") }
      return("65+")
    }),
    marstat = sapply(marstat, function(x) {
      if(x == 1) { return("married") }
      if(x == 5) { return("never married") }
      return("previously married")
    })) %>%
  filter(state != "Puerto Rico") %>%
  unnest() %>%
  select(-hispanic)

write_delim(df3, "full_pums_data.delim")

write_delim(df3 %>%
              group_by(sex, state, educ, age, race, marstat) %>%
              summarize(N = sum(weight)),
            "poststrat_table.delim")
