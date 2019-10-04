library(tidyverse)
library(brms)
library(mapdata)
library(parallel)
library(gganimate)

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

write_delim(cces_binomial, "votes_df.delim")

tfit = brm(turned_out | trials(N) ~ male +
             (1 | race) + (1 | educ) + (1 | age) + (1 | marstat) +
             (1 + male | state) + (male - 1 | race) + (male - 1 | educ) + (male - 1 | age) + (male - 1 | marstat) +
             #(1 | state:age) + (1 | state:educ) + (1 | state:race) + (1 | state:marstat) +
             (1 | race:educ) + (1 | race:age) + (1 | race:marstat) +
             (1 | educ:age) + (1 | educ:marstat) +
             (1 | age:marstat),
           data = turnout_binomial,
           family = "binomial",
           cores = 4,
           prior = set_prior("normal(0, 1)", class = "b") +
             set_prior("normal(0, 1)", class = "Intercept") +
             set_prior("normal(0, 1)", class = "sd"),
           control = list(adapt_delta = 0.9))

tpr2 = predict(tfit, summary = FALSE)

turnout_binomial %>%
  summarize(p = sum(turned_out) / sum(N))

tibble(p = rowSums(tpr2) / sum(turnout_binomial$N)) %>%
  ggplot(aes(p)) +
  geom_histogram() +
  geom_vline(data = tibble(p = sum(turnout_binomial$turned_out) / sum(turnout_binomial$N)), aes(xintercept = p), color = "red")

tpredicted = fitted(tfit, newdata = pums, allow_new_levels = TRUE)
tpredicted_d = fitted(tfit, newdata = pums, allow_new_levels = TRUE, summary = FALSE)

fit = brm(votes | trials(N) ~ male +
            (1 | race) + (1 | educ) + (1 | age) + (1 | marstat) +
            (1 + male | state) + (male - 1 | race) + (male - 1 | educ) + (male - 1 | age) + (male - 1 | marstat) +
            #(1 | state:age) + (1 | state:educ) + (1 | state:race) + (1 | state:marstat) +
            (1 | race:educ) + (1 | race:age) + (1 | race:marstat) +
            (1 | educ:age) + (1 | educ:marstat) +
            (1 | age:marstat),
          data = cces_binomial,
          family = "binomial",
          cores = 4,
          prior = set_prior("normal(0, 1)", class = "b") +
            set_prior("normal(0, 1)", class = "Intercept") +
            set_prior("normal(0, 1)", class = "sd"),
          control = list(adapt_delta = 0.9))

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

map_tibble = us_map() %>%
  mutate(long = x,
         lat = y,
         region = fips) %>%
  as_tibble()

write_delim(pums %>%
              mutate(predicted = tpredicted[, "Estimate"] / N), "pums_turnout_predictions.delim")

write_delim(tpredicted_d[sample(1:nrow(tpredicted_d), 20), ] %>% t %>%
  as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
  bind_cols(pums) %>%
  pivot_longer(starts_with("predicted_"), names_to = c("rep"), names_pattern = "predicted_([0-9]+)", values_to = "predicted") %>%
  mutate(predicted = predicted / N), "pums_turnout_predictions.delim")

p = tpredicted_d[sample(1:nrow(tpredicted_d), 25), ] %>% t %>%
  as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
  bind_cols(pums) %>%
  pivot_longer(starts_with("predicted_"), names_to = c("rep"), names_pattern = "predicted_([0-9]+)", values_to = "predicted") %>%
  mutate(predicted = predicted / N) %>%
  pstrat(predicted, state, male, educ, age, rep) %>%
  gender_gap(predicted, gap) %>%
  mutate(region = fips(state)) %>%
  ggplot() +
  geom_map(map = map_tibble,
           aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
  coord_equal() +
  expand_limits(map_tibble) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  theme_grey(base_size = 18) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n 25 samples of gender gap of p(turned_out)") +
  transition_manual(rep)

animate(p, fps = 5, height = 1080, width = 1920)
anim_save("gender_gap_map_turnout.gif")

p = predicted_d[sample(1:nrow(predicted_d), 25), ] %>% t %>%
  as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
  bind_cols(pums) %>%
  pivot_longer(starts_with("predicted_"), names_to = c("rep"), names_pattern = "predicted_([0-9]+)", values_to = "predicted") %>%
  mutate(predicted = predicted / N) %>%
  pstrat(predicted, state, male, educ, age, rep) %>%
  gender_gap(predicted, gap) %>%
  mutate(region = fips(state)) %>%
  ggplot() +
  geom_map(map = map_tibble,
           aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
  coord_equal() +
  expand_limits(map_tibble) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  theme_grey(base_size = 18) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n 25 samples of gender gap of p(trump | turned_out)") +
  transition_manual(rep)

animate(p, fps = 5, height = 1080, width = 1920)
anim_save("gender_gap_map_voting.gif")

p = left_join(predicted_d[sample(1:nrow(predicted_d), 25), ] %>% t %>%
            as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
            bind_cols(pums) %>%
            pivot_longer(starts_with("predicted_"),
                         names_to = c("rep"),
                         names_pattern = "predicted_([0-9]+)",
                         values_to = "predicted_vote"),
          tpredicted_d[sample(1:nrow(tpredicted_d), 25), ] %>% t %>%
            as_tibble(.name_repair = function(cols) { paste0("predicted_", 1:length(cols)) }) %>%
            bind_cols(pums) %>%
            pivot_longer(starts_with("predicted_"),
                         names_to = c("rep"),
                         names_pattern = "predicted_([0-9]+)",
                         values_to = "predicted_turnout")) %>%
  mutate(predicted = predicted_vote / N * predicted_turnout / N) %>%
  pstrat(predicted, state, male, educ, age, rep) %>%
  gender_gap(predicted, gap) %>%
  mutate(region = fips(state)) %>%
  ggplot() +
  geom_map(map = map_tibble,
           aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
  coord_equal() +
  expand_limits(map_tibble) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  theme_grey(base_size = 18) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n 25 samples of gender gap of p(trump | turned_out) * p(turned_out)") +
  transition_manual(rep)

animate(p, fps = 5, height = 1080, width = 1920)
anim_save("gender_gap_map_combined.gif")

pums %>%
  mutate(predicted = predicted[, "Estimate"] / N) %>%
  pstrat(predicted, state, male, educ, age) %>%
  gender_gap(predicted, gap) %>%
  mutate(region = fips(state)) %>%
  ggplot() +
  geom_map(map = map_tibble,
           aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
  coord_equal() +
  expand_limits(map_tibble) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  theme_grey(base_size = 18) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n Gender gap of E[p(trump|turned_out)]")
ggsave("gender_gap_map_voting.png", width = 12, height = 6, dpi = 300)

pums %>%
  mutate(predicted = tpredicted[, "Estimate"] / N) %>%
  pstrat(predicted, state, male, educ, age) %>%
  gender_gap(predicted, gap) %>%
  mutate(region = fips(state)) %>%
  ggplot() +
  geom_map(map = map_tibble,
           aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
  coord_equal() +
  expand_limits(map_tibble) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  theme_grey(base_size = 18) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n Gender gap of E[p(turned_out)]")
ggsave("gender_gap_map_turnout.png", width = 12, height = 6, dpi = 300)

pums %>%
  mutate(predicted = predicted[, "Estimate"] * tpredicted[, "Estimate"] / N^2) %>%
  pstrat(predicted, state, male, educ, age) %>%
  gender_gap(predicted, gap) %>%
  mutate(region = fips(state)) %>%
  ggplot() +
  geom_map(map = map_tibble,
           aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
  coord_equal() +
  expand_limits(map_tibble) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  theme_grey(base_size = 18) +
  facet_grid(educ ~ age) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Using CCES 2016 MRP'd on ACS 2016\n Gender gap of E[p(trump|turned_out)] * E[p(turned_out)]")
ggsave("gender_gap_map_combined.png", width = 12, height = 6, dpi = 300)


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
                position = position_dodge(0.5), width = 0.0) +
  #ylim(-0.005, 0.2) +
  ylab("gap (male prob - female prob)") +
  facet_grid(~ age) +
  ggtitle("Gender gap in probability of voting for Trump\n 10<->90 quantiles + medians plotted") +
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("gender_gap_age_educ_turnout.png", width = 12, height = 6, dpi = 300)

bind_rows(mclapply(sample(1:nrow(predicted_d), 400), function(i) {
  pums %>%
    mutate(predicted = (predicted_d[i, ] / N) * (tpredicted_d[sample(1:nrow(tpredicted_d), 1), ] / N)) %>%
    #mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, age, state_groupings, male) %>%
    gender_gap(predicted, gap) %>%
    mutate(rep = i)
}, mc.cores = 8) %>% bind_rows %>%
  mutate(which = "p(trump|turned_out)p(turned_out)"),
mclapply(sample(1:nrow(predicted_d), 400), function(i) {
  pums %>%
    mutate(predicted = predicted_d[i, ] / N) %>%
    pstrat(predicted, age, state_groupings, male) %>%
    gender_gap(predicted, gap) %>%
    mutate(rep = i)
}, mc.cores = 8) %>% bind_rows %>%
  mutate(which = "p(trump|turned_out)")) %>%
  select(-rep) %>%
  group_by_at(vars(-gap)) %>%
  summarize(l = quantile(gap, 0.10),
            m = median(gap),
            h = quantile(gap, 0.9)) %>%
  ggplot(aes(age, m)) +
  geom_point(aes(color = state_groupings), position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = l, ymax = h, color = state_groupings),
                position = position_dodge(0.5), width = 0.0) +
  #ylim(-0.005, 0.2) +
  facet_grid(~ which) +
  scale_color_manual(values = c("Blue State" = "blue",
                                "Battleground State" = "black",
                                "Red State" = "red")) +
  ylab("gap (male prob - female prob)") +
  ggtitle("Gender gap in probability of voting for Trump\n 10<->90 quantiles + medians plotted") +
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggsave("gender_gap_age_turnout.png", width = 12, height = 6, dpi = 300)

save.image(file = "positive_gender_gap.RData")
load("positive_gender_gap.RData")
