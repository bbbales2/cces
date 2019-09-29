library(tidyverse)
library(brms)
library(mapdata)
library(parallel)

usa = map_data("state")

# vote 1 means voted for trump, vote 0 means hillary
# sex -0.5 means male, 0.5 female
preprocess = function(df) {
  state_groupings = list()
  
  state_groupings[c("district of columbia", "hawaii", "vermont", "massachusetts", "rhode island", "connecticut",
                    "new jersey", "delaware", "maryland", "new york", "virginia", "illinois", "new mexico", "oregon",
                    "california", "washington")] = "Red State"
  
  state_groupings[c("maine", "new hampshire", "pennsylvania", "north carolina", "florida", "michigan", "wisconsin", "minnesota",
                    "nebraska", "colorado", "arizona", "nevada")] = "Battleground State"
  
  state_groupings[c("arkansas", "texas", "louisiana", "mississippi", "alabama", "georgia", "south carolina", "tennessee", "alaska",
                    "oklahoma", "kansas", "missouri", "kentucky", "west virginia", "ohio", "indiana", "iowa", "south dakota",
                    "north dakota", "wyoming", "montana", "idaho", "utah")] = "Blue State"
  
  if("vote" %in% colnames(df)) {
    df = df %>%
      mutate(vote = 1 * (vote == "trump"))
  }
  
  df %>%
    mutate(male = -0.5 + 1 * (sex == "Male"),
           educ = factor(educ, levels = c("noHS", "HS", "some_or_assoc", "bach", "bachplus")),
           age = factor(age, levels = c("18-29", "30-44", "45-64", "65+")),
           state_groupings = sapply(state, function(x) state_groupings[[x]])) %>%
    select(-sex)
}

turnout = read_delim("cces_regression.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  mutate(turned_out = vote %in% c("trump", "hillary") & vote_validated) %>%
  preprocess

cces = read_delim("cces_regression.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  filter(vote %in% c("trump", "hillary")) %>%
  filter(vote_validated == TRUE) %>%
  select(-vote_validated) %>%
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
turnout_binomial = turnout %>%
  group_by(male, state, race, educ, age, marstat) %>%
  summarise(N = n(),
            turned_out = sum(turned_out)) %>%
  arrange(-N) %>%
  ungroup()# %>%
  #top_n(1000, N)

cces_binomial = cces %>%
  group_by(male, state, race, educ, age, marstat) %>%
  summarise(N = n(),
            votes = sum(vote)) %>%
  arrange(-N) %>%
  ungroup()# %>%
  #top_n(1000, N)

tfit = brm(turned_out | trials(N) ~ male +
            (1 | state) + (1 | race) + (1 | educ) + (1 | age) + (1 | marstat) +
            (male - 1 | state) + (male - 1 | race) + (male - 1 | educ) + (male - 1 | age) + (male - 1 | marstat) +
            #(1 | state:age) + (1 | state:educ) + (1 | state:race) + (1 | state:marstat) +
            (1 | race:educ) + (1 | race:age) + (1 | race:marstat) +
            (1 | educ:age) + (1 | educ:marstat) +
            (1 | age:marstat),
          data = turnout_binomial,
          family = "binomial",
          cores = 4,
          prior = set_prior("normal(0, 1)", class = "b") +
            set_prior("normal(0, 1)", class = "Intercept") +
            set_prior("normal(0, 1)", class = "sd"))

tpredicted = fitted(tfit, newdata = pums, allow_new_levels = TRUE)
tpredicted_d = fitted(tfit, newdata = pums, allow_new_levels = TRUE, summary = FALSE)

fit = brm(votes | trials(N) ~ male +
            (1 + male | state) + (1 + male | race) + (1 + male | educ) + (1 + male | age) + (1 + male | marstat) +
            #(1 | state:age) + (1 | state:educ) + (1 | state:race) + (1 | state:marstat) +
            (1 | race:educ) + (1 | race:age) + (1 | race:marstat) +
            (1 | educ:age) + (1 | educ:marstat) +
            (1 | age:marstat),
          data = cces_binomial,
          family = "binomial",
          cores = 4,
          prior = set_prior("normal(0, 1)", class = "b") +
            set_prior("normal(0, 1)", class = "Intercept") +
            set_prior("normal(0, 1)", class = "sd"))

predicted = fitted(fit, newdata = pums, allow_new_levels = TRUE)
predicted_d = fitted(fit, newdata = pums, allow_new_levels = TRUE, summary = FALSE)

pstrat = function(df, predicted, ...) {
  predicted_quo = rlang::enquo(predicted)
  group_vars = rlang::enquos(...)
  
  df %>%
    group_by(!!!group_vars) %>%
    summarize(!!predicted_quo := sum(!!predicted_quo * N / sum(N))) %>%
    ungroup()
}

gender_gap = function(df, outcome, gap_column_name) {
  gap_column_name = rlang::enquo(gap_column_name)
  outcome = rlang::enquo(outcome)
  
  if("male" %in% colnames(df) == FALSE) {
    stop("to compute gender gap you gotta have a column in the dataframe labeled male")
  }
  
  if(length(setdiff(unique(df$male), c(0.5, -0.5))) > 0) {
    stop("male column must be encoded as doubles, 0.5 corresponding to a male, -0.5 to a female")
  }
  
  df %>%
    spread(male, quo_name(outcome)) %>%
    mutate(!!gap_column_name := `0.5` - `-0.5`) %>%
    select(-`0.5`, -`-0.5`)
}

gap_df_educ = mclapply(1:4000, function(i) {
  pums %>%
    mutate(predicted = (predicted_d[i, ] / N)) %>%
    #mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, educ, state_groupings, male) %>%
    gender_gap(predicted, gap) %>%
    mutate(rep = i)
}, mc.cores = 8) %>% bind_rows

gap_df_age = mclapply(1:4000, function(i) {
  pums %>%
    mutate(predicted = (predicted_d[i, ] / N)) %>%
    #mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, age, state_groupings, male) %>%
    gender_gap(predicted, gap) %>%
    mutate(rep = i)
}, mc.cores = 8) %>% bind_rows

gap_df_educ_age = mclapply(1:4000, function(i) {
  pums %>%
    mutate(predicted = (predicted_d[i, ] / N)) %>%
    #mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, educ, age, state_groupings, male) %>%
    gender_gap(predicted, gap) %>%
    mutate(rep = i)
}, mc.cores = 8) %>% bind_rows

gap_df_educ %>%
  select(-rep) %>%
  group_by_at(vars(-gap)) %>%
  summarize(l = quantile(gap, 0.10),
            m = median(gap),
            h = quantile(gap, 0.9)) %>%
  ggplot(aes(educ, m)) +
  geom_point(aes(color = state_groupings), position = position_dodge(0.5)) +
  geom_point(data = cces %>%
               group_by(educ, state_groupings, male) %>%
               summarize(m = sum(vote * weight) / sum(weight)) %>%
               gender_gap(m, m), aes(color = state_groupings),
             shape = "cross",
             position = position_dodge(0.5),
             stroke = 2) +
  geom_errorbar(aes(ymin = l, ymax = h, color = state_groupings), position = position_dodge(0.5)) +
  ylim(-0.005, 0.2) +
  ylab("gap (male prob - female prob)") +
  ggtitle("Gender gap in probability of voting for Trump\n MRP 10<->90 quantiles + medians plotted\n Xs are CCES weight estimates") +
  scale_color_manual(values = c("Blue State" = "blue",
                                "Battleground State" = "black",
                                "Red State" = "red")) +
  theme_grey(base_size = 18)
ggsave("gap_vs_educ.png", width = 9, height = 6, dpi = 300)

gap_df_age %>%
  select(-rep) %>%
  group_by_at(vars(-gap)) %>%
  summarize(l = quantile(gap, 0.10),
            m = median(gap),
            h = quantile(gap, 0.9)) %>%
  ggplot(aes(age, m)) +
  geom_point(aes(color = state_groupings), position = position_dodge(0.5)) +
  geom_point(data = cces %>%
               group_by(age, state_groupings, male) %>%
               summarize(m = sum(vote * weight) / sum(weight)) %>%
               gender_gap(m, m), aes(color = state_groupings),
             shape = "cross",
             position = position_dodge(0.5),
             stroke = 2) +
  geom_errorbar(aes(ymin = l, ymax = h, color = state_groupings),
                position = position_dodge(0.5)) +
  #ylim(-0.3, 0.3) +
  ylab("gap (male prob - female prob)") +
  ggtitle("Gender gap in probability of voting for Trump\n MRP, 10<->90 quantiles + medians plotted\n Xs are CCES weight estimates") +
  scale_color_manual(values = c("Blue State" = "blue",
                                "Battleground State" = "black",
                                "Red State" = "red")) +
  theme_grey(base_size = 18)
ggsave("gap_vs_age.png", width = 9, height = 6, dpi = 300)

gap_df_educ_age %>%
  select(-rep) %>%
  group_by_at(vars(-gap)) %>%
  summarize(l = quantile(gap, 0.10),
            m = median(gap),
            h = quantile(gap, 0.9)) %>%
  ggplot(aes(educ, m)) +
  geom_point(aes(color = state_groupings), position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = l, ymax = h, color = state_groupings),
                position = position_dodge(0.5)) +
  #ylim(-0.005, 0.2) +
  ylab("gap (male prob - female prob)") +
  facet_grid(~ age) +
  ggtitle("Gender gap in probability of voting for Trump\n 10<->90 quantiles + medians plotted") +
  scale_color_manual(values = c("Blue State" = "blue",
                                "Battleground State" = "black",
                                "Red State" = "red")) +
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("gap_vs_educ_age.png", width = 9, height = 6, dpi = 300)

detail_names = c("18-29" = "18-29",
                 "30-44" = "30-44",
                 "45-64" = "45-64",
                 "65+" = "65+",
                 "assoc" = "2 years",
                 "bach" = "4 years",
                 "bachp" = "4 years+",
                 "HS" = "High school",
                 "noHS" = "No High school",
                 "some" = "Some college")

pums %>%
  mutate(predicted = tpredicted[, "Estimate"] / N) %>%
  pstrat(predicted, state, age, educ) %>%
  #gender_gap(predicted, gap) %>%
  rename(region = state) %>%
  full_join(usa, by = "region") %>%
  ggplot() +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = predicted), color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n Gender gap (male - female) of mean probability of voting Trump")
ggsave("gap_map.png", width = 9, height = 6, dpi = 300)

pums %>%
  mutate(predicted = tpredicted[, "Estimate"] / N) %>%
  pstrat(predicted, state, age, educ, male) %>%
  gender_gap(predicted, gap) %>%
  rename(region = state) %>%
  full_join(usa, by = "region") %>%
  ggplot() +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = gap), color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n Gender gap (male - female) of mean probability of voting Trump")

bind_rows(pums %>%
            mutate(predicted = (predicted[, "Estimate"] / N) * (tpredicted[, "Estimate"] / N)) %>%
            pstrat(predicted, age, state_groupings, male) %>%
            gender_gap(predicted, gap) %>%
            mutate(includes_turnout = TRUE),
          pums %>%
            mutate(predicted = predicted[, "Estimate"] / N) %>%
            pstrat(predicted, age, state_groupings, male) %>%
            gender_gap(predicted, gap) %>%
            mutate(includes_turnout = FALSE)) %>%
  ggplot(aes(age, gap)) +
  geom_point(aes(color = includes_turnout))

apply(tpredicted_d, 2, max)

bind_rows(mclapply(sample(1:nrow(predicted_d), 400), function(i) {
    pums %>%
      mutate(predicted = (predicted_d[i, ] / N) * (tpredicted_d[sample(1:nrow(tpredicted_d), 1), ] / N)) %>%
      #mutate(predicted = predicted_d[i, ]) %>%
      pstrat(predicted, educ, age, male) %>%
      gender_gap(predicted, gap) %>%
      mutate(rep = i)
  }, mc.cores = 8) %>% bind_rows %>%
    mutate(which = "p(trump|turned_out)p(turned_out)"),
    mclapply(sample(1:nrow(predicted_d), 400), function(i) {
      pums %>%
        mutate(predicted = predicted_d[i, ] / N) %>%
        pstrat(predicted, educ, age, male) %>%
        gender_gap(predicted, gap) %>%
        mutate(rep = i)
    }, mc.cores = 8) %>% bind_rows %>%
      mutate(which = "p(trump|turned_out)")) %>%
  select(-rep) %>%
  group_by_at(vars(-gap)) %>%
  summarize(l = quantile(gap, 0.10),
            m = median(gap),
            h = quantile(gap, 0.9)) %>%
  ggplot(aes(educ, m)) +
  geom_point(aes(color = which), position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = l, ymax = h, color = which),
                position = position_dodge(0.5)) +
  #ylim(-0.005, 0.2) +
  ylab("gap (male prob - female prob)") +
  facet_grid(~ age) +
  ggtitle("Gender gap in probability of voting for Trump\n 10<->90 quantiles + medians plotted") +
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

save.image(file = "positive_gender_gap.RData")
load("positive_gender_gap.RData")
