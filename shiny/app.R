#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(usmap)

map_tibble = us_map() %>%
    mutate(long = x,
           lat = y,
           region = fips) %>%
    as_tibble()

prediction_df = read_delim("basic.delim", delim = " ") %>%
    mutate(sex = sapply(male, function(x) if(x == 0.5) "male" else "female")) %>%
    mutate(educ = factor(educ, levels = c("noHS", "HS", "some_or_assoc", "bach", "bachplus")))

pstrat = function(df, predicted, ...) {
    predicted_quo = rlang::enquo(predicted)
    group_vars = rlang::enquos(...)
    
    df %>%
        group_by(!!!group_vars) %>%
        summarize(!!predicted_quo := sum(!!predicted_quo * N / sum(N))) %>%
        ungroup()
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('outcome', 'Outcome', c("Retrospective Economy", "President")),
            selectInput('year', 'Plot Year', c("All", sort(unique(prediction_df$year)))),
            selectInput('factor1', 'Group by', c("Education", "Age", "Sex", "Race", "Marital Status", "State")),
            selectInput('factor2', 'Group by', c("Ignore", "Age", "Education", "Sex", "Race", "Marital Status", "State")),
            selectInput('factor3', 'Group by', c("Ignore", "Education", "Age", "Sex", "Race", "Marital Status", "State")),
            checkboxInput("summarize", label = "Summarize", value = TRUE),
            checkboxInput("use_maps", label = "Use Maps", value = TRUE),
            checkboxInput("free_y", label = "Freely scale y", value = FALSE),
            actionButton("action", label = "Sample (reqs Summarize == False)"),
            width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot", height = "800px"), width = 10)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$distPlot <- renderPlot({
        input$action
        
        prediction_df_outcome_filtered = prediction_df %>%
            filter(outcome == (if(input$outcome == "Retrospective Economy") "econ_better" else "approve_pres"))
        
        to_sym = function(fact) {
            if(fact == "Education") {
                return(rlang::sym("educ"))
            } else if(fact == "Age") {
                return(rlang::sym("age"))
            } else if(fact == "Sex") {
                return(rlang::sym("sex"))
            } else if(fact == "Race") {
                return(rlang::sym("race"))
            } else if(fact == "Marital Status") {
                return(rlang::sym("marstat"))
            } else if(fact == "State") {
                return(rlang::sym("state"))
            } else if(fact == "Year") {
                return(rlang::sym("year"))
            }
        }
        
        if(input$year == 'All') {
            do_time_series_plot = TRUE
            prediction_df_time_filtered = prediction_df_outcome_filtered
        } else {
            if(as.numeric(input$year) %in% unique(prediction_df_outcome_filtered$year)) {
                do_time_series_plot = FALSE
                    prediction_df_time_filtered = prediction_df_outcome_filtered %>%
                    filter(year == as.numeric(input$year))
            } else {
                stop("Invalid year specified (report this error)")
            }
        }
        
        selected_factors = setdiff(unique(c(input$factor1, input$factor2, input$factor3)), c("Ignore"))
        do_state_plot = ("State" %in% selected_factors) & (do_time_series_plot == FALSE)
        if(do_state_plot == TRUE) {
            selected_factors = c("State", setdiff(selected_factors, c("State")))
        } else if(do_time_series_plot == TRUE) {
            selected_factors = c("Year", selected_factors)
        }
        
        factors = lapply(selected_factors, to_sym)
        
        which_rep = sample(unique(prediction_df_time_filtered$rep), 1)
        
        scale_limits = prediction_df_time_filtered %>%
            pstrat(predicted, !!!factors, rep) %>%
            summarize(min = min(predicted),
                      max = max(predicted)) %>%
            unlist()
        
        qdf = prediction_df_time_filtered %>%
            pstrat(predicted, !!!factors, rep) %>%
            group_by(!!!factors) %>%
            summarize(median = median(predicted),
                      uq = quantile(predicted, 0.9),
                      lq = quantile(predicted, 0.1))
        
        sdf = prediction_df_time_filtered %>%
            filter(rep == which_rep) %>%
            pstrat(predicted, !!!factors)
        
        if(do_state_plot == TRUE & input$use_maps == TRUE) {
            if(input$summarize == FALSE) {
                pdf = sdf %>%
                    rename(median = predicted)
            } else {
                pdf = qdf
            }
            
            p = pdf %>%
                mutate(region = fips(state)) %>%
                ggplot() +
                geom_map(map = map_tibble,
                         aes(map_id = region, fill = median), color = "#000000", size = 0.15) +
                coord_equal() +
                expand_limits(map_tibble) +
                scale_fill_continuous(limits = scale_limits) +
                theme(panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())
            
            if(length(factors) == 2) {
                p = p + facet_grid(rows = vars(!!factors[[2]]), labeller = "label_both")
            } else if(length(factors) == 3) {
                p = p + facet_grid(rows = vars(!!factors[[2]]),
                                   cols = vars(!!factors[[3]]), labeller = "label_both")
            }
        } else {
            p = qdf %>%
                ggplot() +
                #ylim(scale_limits) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                ylab("Predicted")

            if(do_time_series_plot == FALSE) {
                p = p + geom_errorbar(aes(!!factors[[1]], ymin = lq, ymax = uq, width = 0.0), position = position_dodge(width = 0.25)) +
                    geom_point(aes(!!factors[[1]], median), position = position_dodge(width = 0.25))
            } else {
                p = p + geom_ribbon(aes(!!factors[[1]], ymin = lq, ymax = uq), alpha = 0.25, size = 0.0) +
                    geom_point(aes(!!factors[[1]], median)) + 
                    scale_x_continuous(breaks = qdf %>% pull(!!factors[[1]]) %>% unique)
            }

            if(input$summarize == FALSE) {
                if(do_time_series_plot == FALSE) {
                    p = p + geom_point(data = sdf,
                                       aes(!!factors[[1]], predicted), shape = 4, position = position_dodge(width = 0.25))
                } else {
                    p = p + geom_point(data = sdf,
                                       aes(!!factors[[1]], predicted), shape = 4)
                }
            }

            scale_type = if(input$free_y) "free_y" else "fixed"
            if(length(factors) == 2) {
                p = p + aes(color = !!factors[[2]], fill = !!factors[[2]])
            } else if(length(factors) == 3) {
                p = p + aes(color = !!factors[[2]], fill = !!factors[[2]]) +
                    facet_wrap(vars(!!factors[[3]]),
                               nrow = qdf %>% pull(!!factors[[3]]) %>% unique %>% length,
                               labeller = "label_both",
                               scales = scale_type)
            } else if(length(factors) == 4) {
                p = p + aes(color = !!factors[[2]], fill = !!factors[[2]]) +
                    facet_wrap(vars(!!!factors[3:4]),
                               nrow = qdf %>% pull(!!factors[[3]]) %>% unique %>% length,
                               ncol = qdf %>% pull(!!factors[[4]]) %>% unique %>% length,
                               labeller = "label_both",
                               scales = scale_type)
            }
        }

        p + theme(text = element_text(size = 20))
    })
}

    # pstrat(predicted, state, male, educ, age, rep) %>%
    # gender_gap(predicted, gap) %>%
    # mutate(region = fips(state)) %>%
    # ggplot() +
    # geom_map(map = map_tibble,
    #          aes(map_id = region, fill = gap), color = "#000000", size = 0.15) +
    # coord_equal() +
    # expand_limits(map_tibble) +
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.0) +
    # theme_grey(base_size = 18) +
    # facet_grid(educ ~ age) +
    # theme(panel.grid.major = element_blank(),
    #       panel.background = element_blank(),
    #       axis.title.x = element_blank(),
    #       axis.text.x = element_blank(),
    #       axis.ticks.x = element_blank(),
    #       axis.title.y = element_blank(),
    #       axis.text.y = element_blank(),
    #       axis.ticks.y = element_blank()) +
    # ggtitle("Using CCES 2016 MRP'd on ACS 2016\n 25 samples of gender gap of p(turned_out)")# +
    # #transition_manual(rep)

# Run the application 
shinyApp(ui = ui, server = server)
