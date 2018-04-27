

# # logifySlider javascript function
# JS.logify =
#   "
# // function to logify a sliderInput
# function logifySlider (sliderId) {
# // regular number style
# $('#'+sliderId).data('ionRangeSlider').update({
# 'prettify': function (num) { return (Math.pow(10, num)); }
# })
# }"
# 
# # call logifySlider for each relevant sliderInput
# JS.onload =
#   "
# // execute upon document loading
# $(document).ready(function() {
#   // wait a few ms to allow other scripts to execute
#   setTimeout(function() {
#     // include call for each slider
#     logifySlider('mass')
#   }, 5)})
# "

shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = 'Meteorites'),
    dashboardSidebar(
      sidebarMenu(
        tags$head(tags$style(HTML('
                                  .checkbox {
                                    font-size: 12px;
                                    margin: 5px;
                                  }
                                  '))),
        # tags$head(tags$script(HTML(JS.logify))),
        # tags$head(tags$script(HTML(JS.onload))),
        id = "tabs",
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Histogram", tabName = "hist", icon = icon("signal")),
        checkboxGroupInput(inputId = "class",
                       label = "Class",
                       choices = "",
                       selected = 'Pallasite'
                       ),
        checkboxGroupInput(inputId = "fall",
                           label = "Fall",
                           choices = c("Seen falling" = 'Fell',"Found on the ground" = 'Found'),
                           selected = c("Fell","Found")),
        sliderInput(inputId = "year",
                    label = "Year",
                    min = 1800,
                    max = 2020,
                    value = c(1800, 2020),
                    round = TRUE,
                    sep = '',
                    animate = animationOptions(interval = 200,
                                               playButton = HTML("<h4>Play</h4>")))
        # sliderInput(inputId = "mass",
        #             label = "Mass",
        #             min = -1,
        #             max = 8,
        #             step = 1,
        #             value = c(1,2))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(
          tabName = 'map',
          fluidRow(box(leafletOutput("map"), width = 12))
        ),
        tabItem(
          tabName = 'hist',
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