################################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
# SERVER FUNCTION
################################################################################

# install.packages("pacman")
pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo, thematic,
               bslib, plotly, tidyquant, mixOmics)
# devtools::install_github("hadley/emo")

# Today, 17/04/2021, we are going to load the COVID data from Spain. We will 
# work on this data set and we will fit the model until this time. The following
# days will be used to test the data.

# spain <- COVID19::covid19(country = "spain")
# saveRDS(spain, "spain.RDS")

spain <- readRDS("../00_data/spain_clean.RDS")
spain_weekly <- readRDS("../00_data/spain_weekly.RDS")
spain_deaths <- readRDS("../00_data/spain_deaths.RDS")
spain_confirmed <- readRDS("../00_data/spain_confirmed.RDS")

workflow_results_deaths_tuned <- readRDS("../02_results/workflow_results_deaths_tuned.RDS")
workflow_results_confirmed_tuned <- readRDS("../02_results/workflow_results_confirmed_tuned.RDS")

best_fit_deaths <- readRDS("../02_results/best_fit_deaths.RDS")
best_fit_confirmed <- readRDS("../02_results/best_fit_confirmed.RDS")
results <- readRDS("../02_results/results.RDS")


server <- function(input, output){
  thematic::thematic_shiny()
  
  output$dataset <- renderDataTable(
    spain[, input$data], 
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
  
  output$metrics1 <- renderPlotly({
    autoplot(workflow_results_deaths_tuned, select_best = T) +
      ggtitle("Results for the deaths model") +
      scale_color_tq() +
      theme_tq()
  })
  
  output$metrics2 <- renderPlotly({
    autoplot(workflow_results_confirmed_tuned, select_best = T) +
      ggtitle("Results for the confirmed model") +
      scale_color_tq() +
      theme_tq()
  })
  
  output$fit <- renderPlot({
    if (input$optFit1 == "Deaths"){
      plotVar(best_fit_deaths$fit$fit$fit)
    } else if (input$optFit1 == "Confirmed"){
      plotVar(best_fit_confirmed$fit$fit$fit)
    }
  })
  
  output$train <- renderPlotly({
    if (input$optFit2 == "Deaths"){
      pred_train_deaths <- best_fit_deaths %>%
        predict(new_data = spain_weekly)
      
      p1 <- spain_weekly %>%
        mutate(predictions = pred_train_deaths$.pred) %>%
        dplyr::select(date, deaths_week, predictions) %>%
        pivot_longer(cols = c("deaths_week", "predictions"), values_to = "value", names_to = "Variable") %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(color = Variable), size = 1) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(
          y = "Deaths per week",
          x = "Date"
        )
      p1 +
        ggtitle("Predicted deaths in the training set") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    } else if (input$optFit2 == "Confirmed") {
      pred_train_confirmed <- best_fit_confirmed %>%
        predict(new_data = spain_weekly)
      
      p2 <- spain_weekly %>%
        mutate(predictions = pred_train_confirmed$.pred) %>%
        dplyr::select(date, confirmed_week, predictions) %>%
        pivot_longer(cols = c("confirmed_week", "predictions"), values_to = "value", names_to = "Variable") %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(color = Variable), size = 1) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(
          y = "Confirmed cases per week",
          x = "Date"
        ) 
      p2 +
        ggtitle("Predicted confirmed cases in the training set") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    }
    
    
  })
  
  output$test <- renderDataTable(
    results,
    options = list(
      searching = FALSE,
      paging = FALSE
    )
  )
}

