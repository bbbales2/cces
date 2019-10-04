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

turnout_df = read_delim("pums_turnout_predictions.delim", delim = " ") %>%
    mutate(sex = sapply(male, function(x) if(x == 0.5) "male" else "female"))

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
            selectInput('factor1', 'Group by', c("Education", "Age", "Sex", "Race", "Marital Status", "State")),
            selectInput('factor2', 'Group by', c("Ignore", "Education", "Age", "Sex", "Race", "Marital Status", "State")),
            selectInput('factor3', 'Group by', c("Ignore", "Education", "Age", "Sex", "Race", "Marital Status", "State")),
            checkboxInput("summarize", label = "Summarize", value = TRUE),
            actionButton("action", label = "Sample (reqs Summarize == False)")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot", height = "800px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$distPlot <- renderPlot({
        input$action
        
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
            }
        }
        
        factors = setdiff(unique(c(input$factor1, input$factor2, input$factor3)), c("Ignore"))
        state_factor = "State" %in% factors
        if(state_factor == TRUE) {
            factors = setdiff(factors, c("State"))
            factor1 = to_sym("State")
        } else {
            factor1 = to_sym(factors[1])
            factors = as.vector(na.omit(factors[2:3]))
        }
        
        num_facet_factors = length(factors)
        
        which_rep = sample(unique(turnout_df$rep), 1)
        
        if(num_facet_factors == 0) {
            scale_limits = turnout_df %>%
                pstrat(predicted, !!factor1, rep) %>%
                summarize(min = min(predicted),
                          max = max(predicted)) %>%
                unlist()
            
            qdf = turnout_df %>%
                pstrat(predicted, !!factor1, rep) %>%
                group_by(!!factor1) %>%
                summarize(median = median(predicted),
                          uq = quantile(predicted, 0.9),
                          lq = quantile(predicted, 0.1))
            
            sdf = turnout_df %>%
                filter(rep == which_rep) %>%
                pstrat(predicted, !!factor1)
        } else if(num_facet_factors == 1) {
            factor2 = to_sym(factors[1])
            
            scale_limits = turnout_df %>%
                pstrat(predicted, !!factor1, !!factor2, rep) %>%
                summarize(min = min(predicted),
                          max = max(predicted)) %>%
                unlist()
            
            qdf = turnout_df %>%
                pstrat(predicted, !!factor1, !!factor2, rep) %>%
                group_by(!!factor1, !!factor2) %>%
                summarize(median = median(predicted),
                          uq = quantile(predicted, 0.9),
                          lq = quantile(predicted, 0.1))
            
            sdf = turnout_df %>%
                filter(rep == which_rep) %>%
                pstrat(predicted, !!factor1, !!factor2)
        } else if(num_facet_factors == 2) {
            factor2 = to_sym(factors[1])
            factor3 = to_sym(factors[2])
            
            scale_limits = turnout_df %>%
                pstrat(predicted, !!factor1, !!factor2, !!factor3, rep) %>%
                summarize(min = min(predicted),
                          max = max(predicted)) %>%
                unlist()
            
            qdf = turnout_df %>%
                pstrat(predicted, !!factor1, !!factor2, !!factor3, rep) %>%
                group_by(!!factor1, !!factor2, !!factor3) %>%
                summarize(median = median(predicted),
                          uq = quantile(predicted, 0.9),
                          lq = quantile(predicted, 0.1))
            
            sdf = turnout_df %>%
                filter(rep == which_rep) %>%
                pstrat(predicted, !!factor1, !!factor2, !!factor3)
        }
        
        if(state_factor == TRUE) {
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
        } else {
            p = qdf %>%
                ggplot() +
                geom_errorbar(aes(!!factor1, ymin = lq, ymax = uq, width = 0.0)) +
                geom_point(aes(!!factor1, median)) +
                ylim(scale_limits) +
                theme(axis.text.x = element_text(angle=90, hjust=1)) +
                ylab("Predicted")
            
            if(input$summarize == FALSE) {
                p = p + geom_point(data = sdf,
                                   aes(!!factor1, predicted), color = "red")
            }
        }
        
        if(num_facet_factors == 1) {
            p = p + facet_grid(cols = vars(!!factor2))
        } else if(num_facet_factors == 2) {
            p = p + facet_grid(cols = vars(!!factor2),
                               rows = vars(!!factor3))
        }
        
        p + theme(text = element_text(size=20))
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
