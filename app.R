##############################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
#############################################################################

# install.packages("pacman")
pacman::p_load(shiny, tidyverse, tidymodels)

source('UI.R', local = TRUE)
source('Server.R')

shinyApp(ui = UI, server = Server)