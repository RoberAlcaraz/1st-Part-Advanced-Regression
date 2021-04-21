################################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
# UI FUNCTION
################################################################################

pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo)
# devtools::install_github("hadley/emo")

refPanel <- tabPanel(
  "References",
  mainPanel(
    p(tags$button(class="btn btn-default", 
                  `data-toggle`="collapse", 
                  `data-target`="#collapseExample",
                  "References")),
    
    div(class="collapse", id="collapseExample",
        div(class="card card-body",
            includeMarkdown("references.md")
        ))
  )
)

ui <- navbarPage("Advanced Regression And Prediction",
                 theme = shinytheme("sandstone"),
                 refPanel
)





