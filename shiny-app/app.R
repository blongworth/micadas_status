# Shiny app for looking at MICADAS instrument and run status

library(shiny)
#library(DT, warn.conflicts = FALSE)
library(amstools)
library(amsdata)
library(ggplot2)
library(dplyr)

norm_magazine <- function(magazine, normstd) {
  stdrat <- c(tank_std = 1.0398, oxa1 = 1.0398, oxa2 = 1.3407)
  magazine <- magazine |>
    mutate(cor1412 = 	RA / BA ^ 2)
  mean_std <- magazine |>
    filter(type == normstd)|>
    pull(cor1412) |>
    mean()
  magazine |>
    mutate(norm_ratio = norm_run(cor1412, mean_std, stdrat[normstd]))
}

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
        radioButtons("norm",
                     label = h3("Normalizing Standard"),
                     choices = list("none")),
        radioButtons("plottype",
                     label = h3("Plot ratio"),
                     choices = list("RA", "cor1412", "norm_ratio")),
        checkboxInput("box", label = "Boxplot?", value = FALSE),
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

server <- function(input, output, session) {

  # Get list of micadas magazines most recent first
  sqlOutput<- reactive({
    rev(list_magazines())
  })

  observeEvent(sqlOutput(), {
    updateSelectInput(session, "magazineSelect",
                      choices = sqlOutput()
    )
  })

  mag_df <- reactive({
    get_magazine(input$magazineSelect) |>
      mutate(postion = as.factor(position)) |>
      norm_magazine(input$norm)
  })

  observeEvent(input$magazineSelect, {
    types <- unique(mag_df()$type)
    updateRadioButtons(session, "type",
                       choices = c('all', types))
    updateRadioButtons(session, "norm",
                       choices = c('none', types[types %in% c("oxa1", "oxa2", "tank_std")]))
  })

  mag_sub_df <- reactive({
    df <- mag_df()
    if (input$type != 'all') {
    df <- df |>
      filter(type == input$type)
    }
    df
  })

  output$Plot <- renderPlot({
    if (input$box) {
      mag_sub_df() |>
        ggplot(aes(position, .data[[input$plottype]], group = position, color = type)) +
        geom_boxplot()

    } else {
      mag_sub_df() |>
        ggplot(aes(TIMEDAT, .data[[input$plottype]], color = type)) +
        geom_point()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
