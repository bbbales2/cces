library(tidyverse)
library(brms)
library(mapdata)

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
    mutate(sex = -0.5 + 1 * (sex == "Female"),
           educ = factor(educ, levels = c("noHS", "HS", "some", "assoc", "bach", "bachp")),
           age = factor(age, levels = c("18-29", "30-44", "45-64", "65+")),
           state_groupings = sapply(state, function(x) state_groupings[[x]]))
}

cces = read_delim("cces_regression.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  preprocess
pums = read_delim("poststrat_table.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  preprocess

pums2 = read_delim("poststrat_table.inc.delim", delim = " ") %>%
  mutate(state = str_to_lower(state)) %>%
  preprocess

for(name in c("sex", "race", "educ", "state", "age")) {
  unique_cces = unique(cces %>% pull(name))
  unique_pums = unique(pums %>% pull(name))
  
  if(length(setdiff(unique_cces, unique_pums)) > 0) {
    stop(paste("Column", name, "does not have the same elements in the cces and pums data"))
  }
}

fit = brm(vote ~ sex +
            (1 | state) + (1 | race) + (1 | educ) + (1 | age) +
            (1 | race:state) + (1 | educ:state) + (1 | age:state),
          data = cces,
          family = "bernoulli",
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
    left_join(df %>% group_by(!!!group_vars) %>% summarize(Ntotal = sum(N)), by = sapply(group_vars, rlang::as_name)) %>%
    group_by(!!!group_vars) %>%
    mutate(weight = N / Ntotal) %>%
    summarize(!!predicted_quo := sum(!!predicted_quo * weight)) %>%
    ungroup()
}

gender_gap = function(df, outcome, gap_column_name) {
  gap_column_name = rlang::enquo(gap_column_name)
  outcome = rlang::enquo(outcome)
  
  if("sex" %in% colnames(df) == FALSE) {
    stop("to compute gender gap you gotta have a column in the dataframe labeled sex")
  }
  
  if(length(setdiff(unique(df$sex), c(0.5, -0.5))) > 0) {
    stop("sex column must be encoded as doubles, 0.5 corresponding to a female, -0.5 to a male")
  }
  
  df %>%
    spread(sex, quo_name(outcome)) %>%
    mutate(!!gap_column_name := `-0.5` - `0.5`) %>%
    select(-`0.5`, -`-0.5`)
}

gap_df_educ = lapply(1:4000, function(i) {
  pums %>%
    mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, educ, state_groupings, sex) %>%
    gender_gap(gap) %>%
    mutate(rep = i)
}) %>% bind_rows

gap_df_age = lapply(1:4000, function(i) {
  pums %>%
    mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, age, state_groupings, sex) %>%
    gender_gap(gap) %>%
    mutate(rep = i)
}) %>% bind_rows

gap_df_educ_age = lapply(1:4000, function(i) {
  pums %>%
    mutate(predicted = predicted_d[i, ]) %>%
    pstrat(predicted, educ, age, state_groupings, sex) %>%
    gender_gap(gap) %>%
    mutate(rep = i)
}) %>% bind_rows

gap_df_educ %>%
  select(-rep) %>%
  group_by_at(vars(-gap)) %>%
  summarize(l = quantile(gap, 0.10),
            m = median(gap),
            h = quantile(gap, 0.9)) %>%
  ggplot(aes(educ, m)) +
  geom_point(aes(color = state_groupings), position = position_dodge(0.5)) +
  geom_point(data = cces %>%
               group_by(educ, state_groupings, sex) %>%
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
               group_by(age, state_groupings, sex) %>%
               summarize(m = sum(vote * weight) / sum(weight)) %>%
               gender_gap(m, m), aes(color = state_groupings),
             shape = "cross",
             position = position_dodge(0.5),
             stroke = 2) +
  geom_errorbar(aes(ymin = l, ymax = h, color = state_groupings),
                position = position_dodge(0.5)) +
  ylim(-0.005, 0.2) +
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
  ylim(-0.005, 0.2) +
  ylab("gap (male prob - female prob)") +
  facet_grid(~ age) +
  ggtitle("Gender gap in probability of voting for Trump\n 10<->90 quantiles + medians plotted") +
  scale_color_manual(values = c("Blue State" = "blue",
                                "Battleground State" = "black",
                                "Red State" = "red")) +
  theme_grey(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("gap_vs_educ_age.png", width = 9, height = 6, dpi = 300)

pums %>%
  mutate(predicted = predicted[, "Estimate"]) %>%
  pstrat(predicted, state, age, educ, sex) %>%
  gender_gap(predicted, gap) %>%
  rename(region = state) %>%
  full_join(usa, by = "region") %>%
  ggplot() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = gap), color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  facet_grid(educ ~ age, labeller = as_labeller(detail_names)) +
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
