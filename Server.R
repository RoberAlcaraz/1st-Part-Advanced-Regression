################################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
# SERVER FUNCTION
################################################################################

# install.packages("pacman")
pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes)
# Today, 17/04/2021, we are going to load the COVID data from Spain. We will 
# work on this data set and we will fit the model until this time. The following
# days will be used to test the data.

# spain <- COVID19::covid19(country = "spain")
# saveRDS(spain, "spain.RDS")

spain <- readRDS("spain.RDS")

server <- function(input, output){
  
}

