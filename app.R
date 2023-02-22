library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)

# load the dataset
data <- read.csv("US Police shootings in from 2015-22.csv")

# Define UI
ui <- fluidPage(
  
  # Title
  titlePanel("US Police Shootings 2015-2022"),
  
  # Sidebar with input commands
  sidebarLayout(
    sidebarPanel(
      # Filter by date range
      dateRangeInput("date_range", "Filter by date range:",
                     start = as.Date("2015-01-01"), end = Sys.Date()),
      # Filter by race
      selectInput("race_input", "Filter by race:",
                  choices = c("All", unique(data$race))),
      # Filter by state
      selectInput("state_input", "Filter by state:",
                  choices = c("All", unique(data$state)))
    ),
    
    # Main panel with Leaflet map, datatable, and plotly graphs
    mainPanel(
      # Leaflet map with markers and polygons
      leafletOutput("map"),
      # Datatable with raw data
      dataTableOutput("table"),
      # Plotly graph 1
      plotlyOutput("graph1"),
      # Plotly graph 2
      plotlyOutput("graph2"),
      # Download button for filtered data
      downloadButton("download_data", "Download filtered data")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Filter the data based on input commands
  filtered_data <- reactive({
    data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(ifelse(input$race_input == "All", TRUE, race == input$race_input)) %>%
      filter(ifelse(input$state_input == "All", TRUE, state == input$state_input))
  })
  
  # Leaflet map with markers and polygons
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Markers for each shooting
      addMarkers(data = filtered_data(),
                 clusterOptions = markerClusterOptions()) %>%
      # Polygons for each state
      addPolygons(data = data.frame(state = unique(data$state)),
                  fillColor = "blue", fillOpacity = 0.2, weight = 2,
                  options = list(clickable = FALSE))
  })
  
  # Datatable with raw data
  output$table <- renderDataTable({
    filtered_data()
  })
  
  # Plotly graph 1: Bar chart of number of shootings by race
  output$graph1 <- renderPlotly({
    plot_ly(filtered_data(), x = ~race) %>%
      add_trace(type = "bar") %>%
      layout(title = "Number of shootings by race")
  })
  
  # Plotly graph 2: Pie chart of number of shootings by state
  output$graph2 <- renderPlotly({
    plot_ly(filtered_data(), labels = ~state) %>%
      add_pie(values = ~1) %>%
      layout(title = "Number of shootings by state")
  })
  
  # Download button for filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

# Run the app
shinyApp(ui, server)
