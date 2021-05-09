################################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
# UI FUNCTION
################################################################################

pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo, shiny.semantic)
# devtools::install_github("hadley/emo")


introPanel <- tabPanel(
  "1. Introduction",
  sidebarLayout(
    # position = "right",
    sidebarPanel(
      h3(
        strong("Description of the variables: ")),
      HTML(paste0("<ul><li>", 
                  code("id"), ": Unique identifier. </li><li>", 
                  code("date"), ": Observation date. </li><li>", 
                  code("deaths"), ": Cumulative number of deaths. </li><li>", 
                  code("test"), ": Cumulative number of test.</li><li>", 
                  code("confirmed"), ": Cumulative number of confirmed cases. </li><li>", 
                  code("vaccines"), ": Cumulative number of doses administered (single dose). </li><li>", 
                  code("hosp"), ": Number of hospitalized patients on date. </li><li>", 
                  code("icu"), ": Number of hospitalized patients in ICUs on date. </li><li>", 
                  code("population"), ": Total population. </li><li>", 
                  code("stay_home_restrictions"), ": Indicates the measures of staying at home. </li><li>", 
                  code("school_closing"), ": Indicates the measures in education. </li><li>", 
                  code("workplace_closing"), ": Indicates the measures of the workplace. </li><li>", 
                  code("transport_closing"), ": Indicates the measures in the public transport. </li><li>", 
                  code("gatherings_restrictions"), ": Indicates the measures of gatherings. </li><li>",
                  code("internal_movement_restrictions"), ": Indicates the measures of the movements between regions.
                  </li></ul>")
           )
      ),
    mainPanel(
      h4(em("Roberto J. Alcaraz Molina")),
      h4(em("09/05/2021")),
      br(),
      h1(strong("Introduction")),
      br(),
      p("The aim of this project is to analyze and try to predict the confirmed 
        cases and deaths due to the coronavirus pandemic (COVID-19) in Spain using
        regression tools. It started the 31st of January of 2020 in La Gomera 
        (Canary Island) and continues until now, having almost 2.5 million confirmed
        cases and around 67k deaths."),
      br(),
      p("All the analysis of this project, from the data cleaning until the model
        results will be done mainly with the", code("tidyverse"), "and ", code("tidymodels"),
        " packages. The first one is well known for R users but the second, even 
        though is still in development, has enough tools for modeling and machine 
        learning.")
      )
  )
)


edaPanel <- tabPanel(
  "2. EDA",
  sidebarLayout(
    # position = "right",
    sidebarPanel(
      
    ),
    mainPanel(
      
    )
  )
)

newPanel <- tabPanel(
  "1. Introduction",
  sidebarLayout(
    # position = "right",
    sidebarPanel(
    ),
    mainPanel(
      
    )
  )
)

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
                 theme = bslib::bs_theme(bootswatch = "litera"),
                 introPanel,
                 refPanel
)





