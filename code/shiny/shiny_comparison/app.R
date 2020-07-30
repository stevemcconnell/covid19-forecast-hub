library("drake")
library("tidyverse")
library("shiny")
library("DT")
library("shinyWidgets")

options(DT.options = list(pageLength = 50))

source("code/processing-fxns/get_next_saturday.R")

fourweek_date = get_next_saturday(Sys.Date() + 3*7)
loadd(truth)
truth_sources = unique(truth$source)
loadd(latest_locations)
loadd(latest_targets)
loadd(latest_quantiles)
loadd(latest_quantiles_summary)
loadd(latest_plot_data)


ui <- navbarPage(
  "Explore:",
  
  tabPanel("Latest locations", 
           DT::DTOutput("latest_locations")),
  
  tabPanel("Latest quantiles", 
           h3("Quantiles collapsed over targets"),
           h5("all_full: the full set of 23 quantiles exists in all targets"),
           h5("any_full: the full set of 23 quantiles exists in at least one target"),
           h5("all_min: the minimum set of 9 quantiles exists in all targets"),
           h5("any_min: the minimum set of 9 quantiles exists in at least one target"),
           DT::DTOutput("latest_quantiles_summary"), 
           h3("Quantiles by target"),
           DT::DTOutput("latest_quantiles")),
  
  tabPanel("Latest Viz by Location",
           sidebarLayout(
             sidebarPanel (
               selectInput("loc_state", "State", sort(unique(latest_plot_data$abbreviation))),
               selectInput("loc_county", "County", sort(unique(latest_plot_data$location_name))),
               selectInput("loc_target",     "Target", sort(unique(latest_plot_data$simple_target))),
               selectInput("loc_sources", "Truth sources", truth_sources, selected = "JHU-CSSE", multiple = TRUE),
               selectInput("loc_team_model",         "Team_Model", sort(unique(latest_plot_data$team_model)),
                           selected =c("UMass-MechBayes","LANL-GrowthRate","YYG-ParamSearch","UCLA-SuEIR"), multiple = TRUE),
               dateRangeInput("loc_dates", "Date range", start = "2020-03-01", end = fourweek_date)
             ),
             mainPanel(
               plotOutput("latest_plot_by_location")
             )
           )
  ),
  # tabPanel("All",              
  #          DT::DTOutput("all_data")),
  
  tabPanel("Help",
           h3("Explore tabs"),
           # h5("All: contains all of the processed data including those with missing required fields"),
           h5("Latest targets: summarizes `Latest` to see which targets are included"),
           h5("Latest locations: summarizes `Latest` to see which locations are included"),
           h5("Latest quantiles: summarizes `Latest` to see which quantiles are included"),
           h5("Latest Viz by Location: compares forecast visualization for a selected location for selected models"),
           h3("Usage"),
           h4("Each table has the capability to be searched and filtered")
  ),
  
  selected = "Latest Viz by Location"
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$latest_locations <- DT::renderDT(latest_locations, filter = "top")
  output$latest_quantiles <- DT::renderDT(latest_quantiles, filter = "top")
  output$latest_quantiles_summary <- DT::renderDT(latest_quantiles_summary, filter = "top")
  
  
  #############################################################################
  # Latest viz by Location: Filter data based on user input
  latest_loc_l <- reactive({ latest_plot_data    %>% filter(abbreviation    == input$loc_state) })
  latest_loc_lc <- reactive({ latest_loc_l()     %>% filter(location_name == input$loc_county) })
  latest_loc_ltc  <- reactive({ latest_loc_lc()     %>% filter(simple_target == input$loc_target) })
  latest_loc_ltct    <- reactive({ latest_loc_ltc() %>% filter(team_model     %in% input$loc_team_model) })
  
  observe({
    counties <- sort(unique(latest_loc_l()$location_name))
    updateSelectInput(session, "loc_county", choices = counties, selected = counties[1])
  })
  
  observe({
    targets <- sort(unique(latest_loc_lc()$simple_target))
    updateSelectInput(session, "loc_target", choices = targets, 
                      selected = ifelse("wk ahead cum death" %in% targets, 
                                        "wk ahead cum death", 
                                        targets[1]))
  })
  
  # fix filter
  observe({
    team_models <- sort(unique(latest_loc_ltc()$team_model))
    updateSelectInput(session, "loc_team_model", choices = team_models, 
                      selected = ifelse(c("UMass-MechBayes","LANL-GrowthRate","YYG-ParamSearch","UCLA-SuEIR") %in% 
                                          team_models,c("UMass-MechBayes","LANL-GrowthRate","YYG-ParamSearch","UCLA-SuEIR"),team_models[1]))
  })
  
  

  truth_loc_plot_data <- reactive({ 
    input_simple_target <- unique(paste(
      latest_loc_ltct()$unit, "ahead", latest_loc_ltct()$inc_cum, latest_loc_ltct()$death_cases))
    
    tmp = truth %>% 
      filter(abbreviation == input$loc_state,
             location_name == input$loc_county,
             grepl(input_simple_target, simple_target),
             source %in% input$loc_sources)
  })
  
  output$latest_plot_by_location      <- shiny::renderPlot({
    d    <- latest_loc_ltct()
    team_model <- unique(d$team_model)
    forecast_date <- unique(d$forecast_date)
  
    ggplot(d, aes(x = target_end_date)) + 
      geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`, fill = "95%")) +
      geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`, fill = "50%")) +
      scale_fill_manual(name = "", values = c("95%" = "lightgray", "50%" = "gray")) +
        
      geom_point(aes(y=`0.5`, color = "median")) + geom_line( aes(y=`0.5`, color = "median")) + 
      geom_point(aes(y=point, color = "point")) + geom_line( aes(y=point, color = "point")) + 
        
      scale_color_manual(name = "", values = c("median" = "slategray", "point" = "black")) +
        
      ggnewscale::new_scale_color() +
      geom_line(data = truth_loc_plot_data(),
                aes(x = date, y = value, 
                    linetype = source, color = source, group = source)) +
      scale_color_manual(values = c("JHU-CSSE" = "green",
                                    "USAFacts" = "seagreen",
                                    "NYTimes"  = "darkgreen")) +
      
      scale_linetype_manual(values = c("JHU-CSSE" = 1,
                                       "USAFacts" = 2,
                                       "NYTimes"  = 3)) +
      xlim(input$loc_dates) + 
      facet_wrap(~team_model,ncol = 3,labeller = label_wrap_gen(multi_line=FALSE))+
      labs(x = "Date", y="Number", title = paste("Forecast date:", forecast_date)) +
      theme_bw() +
      theme(strip.text.x = element_text(size = 8),plot.title = element_text(color = ifelse(Sys.Date() - forecast_date > 6, "red", "black")))
        
    
    
  },height ="auto")

}

shinyApp(ui = ui, server = server)