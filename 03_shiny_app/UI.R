################################################################################
# title: "1st Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "09/05/2021"
# https://mastering-shiny.org/
# UI FUNCTION
################################################################################

pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo, shiny.semantic,
               thematic, bslib, plotly)
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


prepSteps <- wellPanel(
  h4(strong("Preprocessing steps: ")),
  br(),
  p(strong("1."), " Divide the date in month and year and convert it to categorical, because they could"),
  p("be very important for the outcomes."),
  br(),
  p(strong("2."), " We will create some new variables which will be the deaths and confirmed cases 3"), 
  p("weeks lagged, i.e., we will consider in our model what happened in the previous weeks"), 
  p("to be able to predict in a better way the next month."),
  br(),
  p(strong("3."), " We have 12 covariates which the majority of them are categorical, so we must do "),
  p(strong("feature selection"), ". This time we will use recursive feature elimination, selecting"),
  p("the features that are above the 20% of importance using a random forest model."),
  br(),
  p(strong("4."), " Even though we will test our data with the next 3 weeks, we should divide our "),
  p("training data into folds to tune the model parameters and to compare the model results."),
  p("Then, we can divide our training set into different folds by date, which is very useful"),
  p("for time series models."),
  br(),
  p(strong("5."), " For the linear regression models, the recommended preprocessing is to remove the"),
  p("zero variance variables, decorrelate the predictors and create dummy variables for "),
  p("the categorical predictors; and for the partial least squares (PLS), we need to "),
  p("normalize the predictors.")
  
)
  
deathsPanel <- wellPanel(
  shiny::selectInput("cols1", label = "Select the columns: ", 
                     choices = colnames(spain_deaths), multiple = T),
  dataTableOutput("weekly_data1")
)
confPanel <- wellPanel(
  shiny::selectInput("cols2", label = "Select the columns: ", 
                     choices = colnames(spain_confirmed), multiple = T),
  dataTableOutput("weekly_data2")
)


fePanel <- tabPanel(
  "3. Feature Engineering",
  shiny::splitLayout(
    prepSteps,
    tabsetPanel(
      tabPanel("Deaths data set", deathsPanel),
      tabPanel("Confirmed data set", confPanel)
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





