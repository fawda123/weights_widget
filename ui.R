library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Weighting example for detiding regression"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    dateInput("refdate", "Center of window (date):",
                   value = "2012-07-01",
                   min   = "2012-01-01", 
                   max   = "2012-12-31"),
    numericInput("reftime", "Center of window (time):", 
                 value = 12, 
                 min = 0, max = 23.5, step = 0.5),
    numericInput("win_1", 
                 "Day window (1-365):", 
                 min=1, 
                 max=365, 
                 value=10),
    numericInput("win_2", 
                 "Hour window (0-24)", 
                 min=0, 
                 max=24, 
                 value=12, step = 0.5),
    numericInput("win_3", 
                 "Tidal height window", 
                 min=0, 
                 max=5, 
                 value=1, step = 0.5),
    dateRangeInput("daterange", "Date range for plot:",
                   start = "2012-06-26",
                   end   = "2012-07-08", 
                   min   = "2012-01-01", 
                   max   = "2012-12-31")

  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
))