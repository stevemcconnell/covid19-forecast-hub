library("drake")
library("tidyverse")
library("shiny")
library("DT")
library("shinyWidgets")

options(DT.options = list(pageLength = 50))

source("code/processing-fxns/get_next_saturday.R")

# all_data = readRDS("code/shiny/drake_files/all_data.RDS")
fourweek_date = get_next_saturday(Sys.Date() + 3*7)
loadd(truth)
truth_sources = unique(truth$source)
# loadd(latest)
loadd(latest_locations)
loadd(latest_targets)
loadd(latest_quantiles)
loadd(latest_quantiles_summary)
# ensemble=readRDS("code/shiny/drake_files/ensemble.RDS")
# g_ensemble_quantiles=readRDS("code/shiny/drake_files/g_ensemble_quantiles.RDS")
loadd(latest_plot_data)
# latest_plot_data=readRDS("code/shiny/drake_files/latest_plot_data.RDS")

ui <- navbarPage(
  "Explore:",
  
  tabPanel("Latest locations", 
           DT::DTOutput("latest_locations")),
  
  #tabPanel("Latest targets",  
  #          h5("max_n: the farthest ahead forecast for this target (does not guarantee that all earlier targets exist)"),
  #          DT::DTOutput("old_latest_targets")),
  tabPanel("Latest targets",  
           sidebarLayout(
             sidebarPanel(
               shinyWidgets::pickerInput("targets_team_model","Team", sort(unique(latest_targets$team_model)),
                                         selected =sort(unique(latest_targets$team_model)),
                                         options = list(`actions-box` = TRUE), multiple = TRUE),
               selectInput("targets_type","Type", sort(unique(latest_targets$type))),
               selectInput("targets_target","Target", sort(unique(latest_targets$target))),
               dateRangeInput("targets_dates", "Date range", start = "2020-03-15", end = Sys.Date())
             ), 
             mainPanel(
               plotOutput("latest_targets")
             )
           )
  ),
  
  tabPanel("Latest quantiles", 
           h3("Quantiles collapsed over targets"),
           h5("all_full: the full set of 23 quantiles exists in all targets"),
           h5("any_full: the full set of 23 quantiles exists in at least one target"),
           h5("all_min: the minimum set of 9 quantiles exists in all targets"),
           h5("any_min: the minimum set of 9 quantiles exists in at least one target"),
           DT::DTOutput("latest_quantiles_summary"), 
           h3("Quantiles by target"),
           DT::DTOutput("latest_quantiles")),
  
  # tabPanel("Ensemble",           
  #          DT::DTOutput("ensemble"),
  #          # DT::DTOutput("ensemble_quantiles"),
  #          plotOutput("ensemble_quantile_plot")),
  
  # tabPanel("Latest",           
  #          DT::DTOutput("latest")),
  
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
  
  tabPanel("Latest Viz by Location",
           sidebarLayout(
             sidebarPanel (
               selectInput("loc_state", "State", sort(unique(latest_plot_data$abbreviation))),
               selectInput("loc_county", "County", sort(unique(latest_plot_data$location_name))),
               selectInput("loc_target",     "Target", sort(unique(latest_plot_data$simple_target))),
               selectInput("loc_sources", "Truth sources", truth_sources, selected = "JHU-CSSE", multiple = TRUE),
               selectInput("loc_team_model",         "Team_Model", sort(unique(latest_plot_data$team_model)),
                           selected =c("UMass-MechBayes","LANL-GrowthRate","YYG-ParamSearch","UCLA-SuEIR"), multiple = TRUE),
               #selectInput("loc_model",       "Model", sort(unique(latest_plot_data$model        )),selected = c("MechBayes","GrowthRate","ParamSearch","SuEIR"),multiple = TRUE),
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
           h5("Latest targets: summarizes `Latest` to see which targets are included"),
           h5("Latest locations: summarizes `Latest` to see which locations are included"),
           h5("Latest quantiles: summarizes `Latest` to see which quantiles are included"),
           h5("Latest Viz: shows visualization for forecast for a selected location"),
           h5("Latest Viz by Location: compares forecast visualization for a selected location for selected models"),
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
  
  #set_shiny_plot_height <- function(session, output_width_name){
  #  function() { 
  #    session$clientData[[output_width_name]] *2
  #  }
  #}
  
  

  
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

  #############################################################################
  # New Latest Targets: Filter data based on user input
   latest_t_t <- reactive({latest_targets %>% filter( team_model         %in% input$targets_team_model) })
   latest_t_tmty    <- reactive({ latest_t_t() %>% filter( type    %in%  input$targets_type) })
   latest_t_tmtyt    <- reactive({ latest_t_tmty()  %>% filter(target     %in% input$targets_target) })
  
   
   observe({
     types <- sort(unique(latest_t_t()$type))
     updateSelectInput(session, "targets_type", choices = types, selected = types[1])
   })
  
   
   observe({
     targets <- sort(unique(latest_t_tmty()$target))
     updateSelectInput(session, "targets_target", choices = targets, 
                       selected = ifelse("wk ahead cum death" %in% targets, 
                                         "wk ahead cum death", 
                                         targets[1]))
   })
   
   set_shiny_plot_height <- function(session, output_width_name){
     function() { 
       session$clientData[[output_width_name]] 
     }
   }
  output$latest_targets <-shiny::renderPlot({
  
    d = reshape2::melt(latest_t_tmtyt(),id.vars=c("team_model","type","target","max_n")) 
    dates = unique(d$value)
    dates_axis =list(seq(as.Date(min(dates))-1, Sys.Date(),"day"))
    
    d = d %>%
      group_by_all() %>% 
      nest %>% 
      mutate(data = dates_axis) %>%
      unnest (cols = c(data)) %>%
      mutate(color = if_else(as.Date(value) == as.Date(data), 1, 0)) %>%
      group_by(type,target,data,team_model) %>%
      summarise(color = sum(color))
    
    
    ggplot(d,aes(x=as.Date(data), y=team_model,fill = as.factor(color)))+
      geom_tile(colour="black",size=0.25) +
      scale_y_discrete(expand=c(0,0))+
      scale_x_date(expand=c(0,0),breaks = "1 day",date_labels="%m/%d",limits =c(input$targets_dates[1], input$targets_dates[2]))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
      scale_fill_manual(values = c("white", "chartreuse2"),name = "Status", labels = c("not submitted", "sumbitted"))+
      labs(x = "Forecast Dates", y="Team Model")+
      theme(legend.position="bottom")
  },height = set_shiny_plot_height(session, "output_latest_targets_width"))
  
  
  
  output$all_data         <- DT::renderDT(all_data,         filter = "top")
}

shinyApp(ui = ui, server = server)