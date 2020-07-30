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
 
  tabPanel("Latest Viz",
           sidebarLayout(
             sidebarPanel(
               selectInput("team_model","Team_Model", sort(unique(latest_plot_data$team_model )), 
                           shiny::getShinyOption("default_team_model",default = "IHME-CurveFit")),
               selectInput("target","Target", sort(unique(latest_plot_data$simple_target))),
               selectInput("abbreviation","Location", sort(unique(latest_plot_data$abbreviation   ))),
               selectInput("county","County", sort(unique(latest_plot_data$location_name  ))),
               selectInput("sources","Truth sources", truth_sources, selected = "JHU-CSSE", multiple = TRUE),
               dateRangeInput("dates","Date range", start = "2020-03-01", end =  fourweek_date)
             ), 
             mainPanel(
               plotOutput("latest_plot")
             )
           )
  ),
  
  tabPanel("Help",
           h3("Explore tabs"),
           h5("Latest targets: summarizes `Latest` to see which targets are included"),
           h5("Latest locations: summarizes `Latest` to see which locations are included"),
           h5("Latest quantiles: summarizes `Latest` to see which quantiles are included"),
           h5("Latest Viz: shows visualization for forecast for a selected location"),
           h3("Usage"),
           h4("Each table has the capability to be searched and filtered")
  ),
  
  selected = "Latest Viz"
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$latest_locations <- DT::renderDT(latest_locations, filter = "top")
  output$latest_quantiles <- DT::renderDT(latest_quantiles, filter = "top")
  output$latest_quantiles_summary <- DT::renderDT(latest_quantiles_summary, filter = "top")
  
  #############################################################################
  # Latest viz: Filter data based on user input
  
  observe({
    targets <- sort(unique(latest_t()$simple_target))
    updateSelectInput(session, "target", choices = targets, 
                      selected = ifelse("wk ahead cum death" %in% targets, 
                                        "wk ahead cum death", 
                                        targets[1]))
  })
  
  observe({
    abbreviations <- sort(unique(latest_tmt()$abbreviation))
    updateSelectInput(session, "abbreviation", choices = abbreviations, 
                      selected = ifelse("US" %in% abbreviations, 
                                        "US", 
                                        abbreviations[1]))
  })
  
  observe({
    counties <- sort(unique(latest_tmtl()$location_name))
    updateSelectInput(session, "county", choices = counties, selected =counties[1])
  })
  
  
  latest_t    <- reactive({ latest_plot_data %>% filter(team_model          == input$team_model) })
  latest_tmt  <- reactive({ latest_t()      %>% filter(simple_target == input$target) })
  latest_tmtl <- reactive({ latest_tmt()     %>% filter(abbreviation    == input$abbreviation) })
  latest_tmtlc <- reactive({ latest_tmtl()     %>% filter(location_name    == input$county) })

  truth_plot_data <- reactive({ 
    input_simple_target <- unique(paste(
      latest_tmtlc()$unit, "ahead", latest_tmtlc()$inc_cum, latest_tmtlc()$death_cases))
    
    tmp = truth %>% 
      filter(abbreviation == input$abbreviation,
             location_name ==input$county,
             grepl(input_simple_target, simple_target),
             source %in% input$sources)
  })
  
  
  
  
  output$latest_plot      <- shiny::renderPlot({
    d    <- latest_tmtlc()
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
      geom_line(data = truth_plot_data(),
                aes(x = date, y = value, 
                    linetype = source, color = source, group = source)) +
      
      scale_color_manual(values = c("JHU-CSSE" = "green",
                                    "USAFacts" = "seagreen",
                                    "NYTimes"  = "darkgreen")) +
      
      scale_linetype_manual(values = c("JHU-CSSE" = 1,
                                       "USAFacts" = 2,
                                       "NYTimes"  = 3)) +
      
      xlim(input$dates) + 
      
      labs(x = "Date", y="Number", 
           title = paste("Forecast date:", forecast_date)) +
      theme_bw() +
      theme(plot.title = element_text(color = ifelse(Sys.Date() - forecast_date > 6, "red", "black")))
  })
}

shinyApp(ui = ui, server = server)