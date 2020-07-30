library("drake")

R.utils::sourceDirectory("code/shiny/R",modifiedOnly=FALSE)

forecast_files        = get_forecast_files()
latest_forecast_files = get_latest_forecast_files(forecast_files)

ids = rlang::syms(lapply(latest_forecast_files, function(f) unlist(strsplit(as.character(f), "/"))[2]))


source("code/shiny/shiny_teams/drake_plan.R")
drake_config(plan, targets = shiny)
#drake_config(plan, targets = all_forecasts)
#drake_config(plan, targets = latest)