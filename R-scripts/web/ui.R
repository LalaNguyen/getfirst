## app.R ##
library(shinydashboard)
library(DT)

ui <- dashboardPage(skin="black",
  dashboardHeader(title = "GeT-First dashboard"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Homepage", tabName = "dashboard", icon = icon("dashboard")),
      selectizeInput('selectize', label=NULL, choices = NULL, multiple=TRUE, options = list(maxOptions = 1000,placeholder = 'Write keywords'))
    )
    
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # Dash board 1
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                column(width = 12,
                       box(width = NULL, 
                           title = "STEP 1 - CLASSIFICATION",
                           collapsible = TRUE,
                           DT::dataTableOutput('text1'))
              )),
              fluidRow(
                column(width = 12,
                       box(width = NULL, 
                           title = "STEP 2 - FINDING FREQUENT COMPOUND NOUNS",
                           collapsible = TRUE,
                           DT::dataTableOutput('text2'))
                ))
      )
    ))
)