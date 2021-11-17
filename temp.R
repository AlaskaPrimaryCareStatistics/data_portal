
# Next: try to attach this to app


rm(list=objects())
{
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  setwd("Rfunctions")
  for (file in list.files()){
    source(file)
  }
  setwd("../rds")
  for (file in list.files()){
    load(file)
  }
  setwd("../")
}


survival_plot(
  data <- data_covid,
  xvar <- "time",
  yvar <- "covid",
  ovar <- "age",
  dr <- c(
    "21-01-01",
    "21/08/01"
  ),
  ylim <- c(0, 1)
)

