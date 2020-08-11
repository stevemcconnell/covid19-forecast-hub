# COVID-19 Forecast Hub Shiny App

The shiny app is complementary to the COVID-19 Forecast Hub 
[Dashboard](https://reichlab.io/covid19-forecast-hub/).
These files are a work-in-progress version of the files in [data-processed/](../../data-processed).
For now, the data-processed/ files should be used.

Software requirements: python 3 the following packages:
    
    install.packages(c("tidyverse","data.table","R.utils","shiny",
                       "shinyWidgets","ggnewscale","reshape2","scales","reticulate"))
    
    # install packages at system-level
    pip3 install --upgrade setuptools
    pip3 install -r visualization/requirements.txt

If you want to try out this new version of the shiny app you can use 

    reticulate::use_python(python = "path of your system-level python",required = T) 
    source("code/shiny/shiny_single_file/app_single_file.R")
    shinyApp(ui = ui, server = server) # if it doesn't automatically run
    
from the base folder of the repository.

Note: JHU New York City County truth is different from those from other sources because five boroughs in NYC were aggregated under “New York City.” For more information: please go to https://coronavirus.jhu.edu/us-map-faq

## Background

Originally the app was designed to be an internal tool for the COVID-19 Forecast
Hub team to know what forecast existed within the repository,
aid in the building of an ensemble forecast, 
and (quickly) visualize forecasts (before updating of the Dashboard). 

On May 15, the app was introduced to teams submitting forecasts and therefore
the user base is a lot wider than original intended. 

## Uses

The primary use of the app is for the COVID-19 Forecast Hub team to know what
forecasts are in the repository. 

This app provides a visualization of selected forecast similar to the
Dashboard, but with some slightly different features. It can be used to validate
forecast files locally and visualize forecasts before submitting those forecasts
to the Hub. 

This app only provide visualization for file that passes the validation check. 
