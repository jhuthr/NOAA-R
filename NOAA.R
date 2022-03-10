library(shinydashboard)
library(plotly)
library(shinyjs)
library(shiny)
library(DT)

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidnR0dHQiLCJhIjoiY2tlZGtkb2VwMDM2NTJxcG9hc2Voc2duYyJ9.ATNW9fc-sLH32YumAI9KPQ')

sensors <- read.csv("sensorinfo.csv")
sensors_data <- read.csv("minmaxall5d.csv")

c25 <- c(
  "dodgerblue2","#E31A1C","green4","#6A3D9A","#FF7F00","gold1","skyblue2","#FB9A99","palegreen2","#CAB2D6","#FDBF6F","gray70","khaki2","maroon","orchid1","deeppink1","blue1","steelblue4","darkturquoise","green1","yellow4","yellow3","darkorange4","brown"
)

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(12,"",
         fluidRow(
           column(7,"Fluid 9",
           plotlyOutput("map")),
           
           column(5,"Fluid 3",
            conditionalPanel("output.show", plotlyOutput("histogram"))
           )
         ))),
  conditionalPanel("output.show", sliderInput("Date.Selection",
                  "Dates:",
                  min = as.Date(tail(sensors_data, 1)$Date.Time),
                  max = as.Date(head(sensors_data, 1)$Date.Time),
                  value=c(
                        as.Date(tail(sensors_data, 1)$Date.Time), 
                        as.Date(head(sensors_data, 1)$Date.Time)
                        ),
                  timeFormat="%Y-%m-%d",
                  dragRange=TRUE)),
  conditionalPanel("output.show", plotlyOutput("sensorline")),
  conditionalPanel("output.show", DT::dataTableOutput("mytable")),
  htmlOutput("hoverDataOut"),
  htmlOutput("clickDataOut"),
  
)

server <- function(input, output, session) {
  
  sensorSubset <- reactive(
    subset(sensors_data, 
           sensors_data$id == sensors[unname(clickData()["pointNumber"]) + 1,]$id
           & as.Date(sensors_data$Date.Time) %in% input$Date.Selection[1]:input$Date.Selection[2])
  )
  
  output$map <- renderPlotly({
    layout(
      plot_mapbox(sensors, source = "mapSource") %>%
        config(displayModeBar = FALSE) %>%
        add_markers(
          x = ~lon, 
          y = ~lat, 
          size = 10, 
          color = ~state,
          colors = c25,
          text = ~paste(label),
          hoverinfo = 'text'
        ),
      font = list(color='white'),
      plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
      legend = list(orientation = 'h',
                    font = list(size = 8)),
      margin = list(l = 10, r = 10,
                    b = 25, t = 25,
                    pad = 2), 
      mapbox = list(
        #style = "carto-positron",
        style = "dark",
        zoom =2.5,
        center = list(lon =-95.712889, lat = 37.090194)
      )
    )
  })
  
  output$histogram <- renderPlotly({
    plot_ly(
      data = sensorSubset(),
      alpha=0.6,
      type = "histogram",
      hoverinfo = 'text'
    ) %>%
      add_histogram(
        x = ~max, 
        name = "Water Level Max"
      ) %>%
      add_histogram(
        x = ~min, 
        name = "Water Level Min"
      ) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        legend = list(x = 0.1, y = -0.1, orientation = 'h', itemdoubleclick = FALSE, itemclick=FALSE), 
        barmode = "overlay",
        xaxis = list(title = ''))
  })
  
  hoverData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_hover", source = "mapSource", priority = "event"))
  })
  
  clickData <- reactive({
    currentEventData <- unlist(event_data(event = "plotly_click", source = "mapSource", priority = "event"))
  })
  
  
  output$sensorline <- renderPlotly({
    plot_ly(
      data = sensorSubset()[order(sensorSubset()$Date.Time),],
      x = ~Date.Time,
      y = ~max,
      name = "Max",
      type = "scatter",
      mode = "lines",
      line = list(width = 2, dash = "solid")
      ) %>%
      add_trace(
        y = ~min, 
        name = "Min", 
        type = "scatter",
        mode = "lines", 
        line = list(width = 2, dash = "solid")
      ) %>%
      add_trace(
        y = ~mean, 
        name = "Mean", 
        type = "scatter",
        mode = "lines", 
        line = list(width = 2, dash = "solid")
      ) %>%
      config(displayModeBar = FALSE) %>%
      layout(
        legend = list(x = 0.1, y = 1.1, orientation = 'h', itemdoubleclick = FALSE, itemclick=FALSE),
        #hovermode  = 'x',
        xaxis = list(
          type = 'date',
          tickformat = "%Y"
        )
        )
  })
  
  output$show <- reactive({
    return(!is.null(clickData()))
  })

  outputOptions(output, 'show', suspendWhenHidden = FALSE)

  output$mytable <- DT::renderDataTable({
    DT::datatable(sensorSubset(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
}

shinyApp(ui, server)