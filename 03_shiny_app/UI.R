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
                  code("date"), ": Observation date. </li><li>", 
                  code("deaths"), ": Cumulative number of deaths. </li><li>", 
                  code("confirmed"), ": Cumulative number of confirmed cases. </li><li>", 
                  code("vaccines"), ": Cumulative number of doses administered (single dose). </li><li>", 
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
      shiny::selectInput("numvar", label = "Select the numeric variable:",
                         choices = c("Deaths" = "deaths", 
                                     "Confirmed cases" = "confirmed",
                                     "Vaccines" = "vaccines")),
      br(),
      shiny::selectInput("catvar", label = "Select the categorical variable:",
                         choices = c("School closing" = "school_closing",
                                     "Workplace closing" = "workplace_closing", 
                                     "Transport closing" = "transport_closing",
                                     "Gatherings restrictions" = "gatherings_restrictions",
                                     "Internal movement restrictions" = "internal_movement_restrictions"))
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Description", dataTableOutput("dataset")),
        tabPanel("Numeric variables", plotlyOutput("numplot")),
        tabPanel("Categorical variables", plotlyOutput("catplot"))
      )
      
    )
  )
)


prepSteps <- 
  h4(
    strong("Preprocessing steps: ")
    )

fePanel <- tabPanel(
  "3. Feature Engineering",
  shiny::splitLayout(
    prepSteps,
    wellPanel(
      shiny::selectInput("cols", label = "Select the columns: ", 
                         choices = colnames(spain_weekly)),
      p("On the other hand, there is the quantity of vehicles on sale in each state: "),
      dataTableOutput("weekly_data")
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
                 edaPanel,
                 fePanel,
                 refPanel
)





