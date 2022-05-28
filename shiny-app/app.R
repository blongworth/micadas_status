# Shiny app for looking at MICADAS instrument and run status

library(shiny)
library(DT, warn.conflicts = FALSE)
library(amstools)
library(ggplot2)
library(dplyr)

# Define UI
ui <- shinyUI(
  fluidPage(

    # Application title
    titlePanel("MICADAS Wheel Status"),

    # Sidebar with inputs for selecting wheel and display style
    sidebarLayout(
      sidebarPanel(
        selectInput("magazineSelect",
                    label = h3("Magazine"),""),
        radioButtons("type",
                     label = h3("Sample type"),
                     choices = list("all")),
                     # selected = 1),
        # checkboxInput("box", label = "Boxplot?", value = FALSE),
        # checkboxInput("oxi", label = "Use only OX-I primaries?", value = FALSE),
        # checkboxInput("group", label = "Last group only?", value = FALSE)
      ),


      # Stats and Plots
      mainPanel(
        # htmlOutput("stdData"),
        plotOutput("Plot", height = "1000px")
      )
    ),

    # Data table
    # fluidRow(
    #   dataTableOutput(outputId="table")
    # )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Get list of micadas magazines most recent first
  sqlOutput<- reactive({
    rev(list_magazines())
  })

  observe ({
    updateSelectInput(session, "magazineSelect",
                      choices = sqlOutput()
    )
  })

  mag_df <- reactive({
    get_magazine(input$magazineSelect)
  })

  observe({
    types <- unique(mag_df()$type)
    updateRadioButtons(session, "type",
                       choices = c('all', types))
  })

  mag_sub_df <- reactive({
    df <- mag_df()
    if (input$type != 'all') {
    df <- df %>%
      filter(type == input$type)
    }
    df
  })

  output$Plot <- renderPlot({
    mag_sub_df() %>%
      ggplot(aes(TIMEDAT, RA, color = type)) +
      geom_point()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
