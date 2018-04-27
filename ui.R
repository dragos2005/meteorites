


shinyUI(
  dashboardPage(
    dashboardHeader(title = 'My Dashboard'),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        selectizeInput(inputId = "class",
                       label = "Class",
                       choices = unique(dfm$class),
                       selected = 'Pallasite',
                       multiple = T),
        sliderInput(inputId = "year",
                    label = "Year",
                    min = 1800,
                    max = 2020,
                    value = c(1800, 2020),
                    round = TRUE,
                    sep = '',
                    animate = animationOptions(interval = 500))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(
          tabName = 'map',
          fluidRow(box(leafletOutput("map"), width = 12)),
          fluidRow(box(plotOutput("hist"), width = 12))
        ),
        tabItem(
          tabName = 'data',
          fluidRow(box(DT::dataTableOutput("table"), width = 12))
        )
      )
    )
  )
)