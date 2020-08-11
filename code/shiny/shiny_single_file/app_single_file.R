# app for a single file 
library("tidyverse")
library("shiny")
library("tidyr")
library("dplyr")
library("data.table")
library("DT")
library("shinyWidgets")
library("scales")
library("reticulate")

# set max upload size to be 30 MB
options(shiny.maxRequestSize=30*1024^2)

R.utils::sourceDirectory("code/shiny/R",modifiedOnly=FALSE)


source("code/processing-fxns/get_next_saturday.R")

#fourweek_date = get_next_saturday(Sys.Date() + 3*7)

# file to plot
#file_path = "data-processed/COVIDhub-baseline/2020-08-03-COVIDhub-baseline.csv"


# run python validation file


# get data for viz 
locations = get_locations(file = "data-locations/locations.csv")

# forecast = read_forecast_file(f = file_path) %>%
#   dplyr::left_join(locations, by = c("location")) %>%
#   tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_cases"),remove = FALSE)
# 
# latest_plot_data = get_latest_plot_data(d = forecast)

# Truth
inc_jhu = get_truth(file = "data-truth/truth-Incident Deaths.csv",                     "inc", "JHU-CSSE")
cum_jhu = get_truth(file = "data-truth/truth-Cumulative Deaths.csv",                   "cum", "JHU-CSSE")
inc_usa = get_truth(file ="data-truth/usafacts/truth_usafacts-Incident Deaths.csv",   "inc", "USAFacts")
cum_usa = get_truth(file = "data-truth/usafacts/truth_usafacts-Cumulative Deaths.csv", "cum", "USAFacts")
inc_nyt = get_truth(file = "data-truth/nytimes/truth_nytimes-Incident Deaths.csv",     "inc", "NYTimes")
cum_nyt = get_truth(file = "data-truth/nytimes/truth_nytimes-Cumulative Deaths.csv",   "cum", "NYTimes")
inc_cases_nyt = get_truth(file = "data-truth/nytimes/truth_nytimes-Incident Cases.csv",     "inc", "NYTimes")
inc_cases_usa = get_truth(file = "data-truth/usafacts/truth_usafacts-Incident Cases.csv",   "inc", "USAFacts")
inc_cases_jhu = get_truth(file = "data-truth/truth-Incident Cases.csv",   "inc", "JHU-CSSE")

truth = combine_truth(inc_jhu, inc_usa, inc_nyt,
                      cum_jhu, cum_usa, cum_nyt,
                      inc_cases_nyt,inc_cases_usa,inc_cases_jhu) %>%
  dplyr::left_join(locations, by = c("location"))%>%
  dplyr::mutate(location_name = coalesce(location_name.x, location_name.y))

truth_sources = unique(truth$source)

# create viz 
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("forecast", "Choose Forecast File",multiple = FALSE,accept = c(".csv")),
      hr(),
      selectInput("target","Target", choices = 'NA'),
      selectInput("abbreviation","Location",choices = 'NA'),
      selectInput("county","County", choices = 'NA'),
      selectInput("sources","Truth sources", truth_sources, selected = "JHU-CSSE", multiple = TRUE),
      dateRangeInput("dates","Date range", start = "2020-03-01", end =  Sys.Date())
      ), 
    
    mainPanel(
      plotOutput("latest_plot")
    )  
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # the modal dialog where the user can upload files.
  validate_modal <- modalDialog(
    title = "Validate Forecast File",
    easyClose = F,
    footer = tagList(
      actionButton("validation","Run Validation")
    )
  )
  
  latest_plot_data = eventReactive(input$forecast,{
    forecast <- read_forecast_file(input$forecast$datapath) %>%
      # put in the right model_abbr
      dplyr::mutate(team_model = sub("(?:[^-\n]+-){3}(.*)\\.csv", "\\1",input$forecast$name)) %>%
      dplyr::left_join(locations, by = c("location")) %>%
      tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_cases"),remove = FALSE)
    
    get_latest_plot_data(d = forecast)
    
  })
  
  observeEvent(latest_plot_data(),{
    showModal(validate_modal)
  })
  
  observeEvent(input$validation, {
    #validate file 
    # call python file
    # shinyFeedback::feedbackWarning
    
  
    updateSelectInput(session, "target",choices = sort(unique(latest_plot_data()$simple_target)), selected =sort(unique(latest_plot_data()$simple_target))[1])
    
    updateDateRangeInput(session, "dates",start = "2020-03-01", end =  max(latest_plot_data()$target_end_date))
    
    
    removeModal()
  })  
  
    
  #############################################################################
  # Latest viz: Filter data based on user input
  
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
   
   
 
   latest_tmt  <- reactive({ latest_plot_data()     %>% filter(simple_target == input$target) })
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
           title = team_model,
           subtitle = paste("Forecast date:", forecast_date)) +
      theme_bw() +
      theme(plot.subtitle  = element_text(color = ifelse(Sys.Date() - forecast_date > 6, "red", "black")))
  })
}

shinyApp(ui = ui, server = server)


