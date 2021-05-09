################################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
# SERVER FUNCTION
################################################################################

# install.packages("pacman")
pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo, thematic,
               bslib, plotly)
# devtools::install_github("hadley/emo")

# Today, 17/04/2021, we are going to load the COVID data from Spain. We will 
# work on this data set and we will fit the model until this time. The following
# days will be used to test the data.

# spain <- COVID19::covid19(country = "spain")
# saveRDS(spain, "spain.RDS")

spain <- readRDS("../00_data/spain_clean.RDS")
spain_deaths <- readRDS("../00_data/spain_deaths.RDS")
spain_confirmed <- readRDS("../00_data/spain_confirmed.RDS")

server <- function(input, output){
  thematic::thematic_shiny()
  
  output$dataset <- renderDataTable(
    spain, 
    options = list(
      pageLength = 10
    )
  )
  
  output$numplot <- renderPlotly({
    if (input$numvar == "deaths") {
      spain %>%
        ggplot(aes(x = date, y = deaths)) +
        geom_line() +
        ggtitle("Cumulative deaths") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    } else if (input$numvar == "confirmed") {
      spain %>%
        ggplot(aes(x = date, y = confirmed)) +
        geom_line() +
        ggtitle("Cumulative confirmed cases") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    } else if (input$numvar == "vaccines") {
      spain %>%
        ggplot(aes(x = date, y = vaccines)) +
        geom_line() +
        ggtitle("Cumulative vaccines") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    }
  })
  
  output$catplot <- renderPlotly({
    spain %>%
      ggplot(aes_string(x = "date", y = input$numvar, color = input$catvar)) +
      geom_point() + 
      ggtitle(paste("Cumulative ", input$numvar, " and ", input$catvar)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20))
  })
  
  output$weekly_data1 <- renderDataTable(
    spain_deaths[, input$cols1],
    options = list(
      pageLength = 10
    )
  )
  
  output$weekly_data2 <- renderDataTable(
    spain_confirmed[, input$cols2],
    options = list(
      pageLength = 10
    )
  )
  
}

