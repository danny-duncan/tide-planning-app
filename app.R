# NOAA CO-OPS Tide Prediction Shiny App
# Application for European Green Crab (EGC) field planning purposes

library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(gridExtra)

# Function to query NOAA CO-OPS API for tide predictions
get_tide_predictions <- function(station_id, start_date, end_date, datum = "MLLW") {
  base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  
  # Format dates for API
  begin_date <- format(start_date, "%Y%m%d")
  end_date <- format(end_date, "%Y%m%d")
  
  # API parameters
  params <- list(
    product = "predictions",
    begin_date = begin_date,
    end_date = end_date,
    datum = datum,
    station = station_id,
    time_zone = "lst_ldt",
    units = "english",
    interval = "hilo",
    format = "json"
  )
  
  response <- GET(base_url, query = params)
  
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    if (!is.null(data$predictions)) {
      df <- as.data.frame(data$predictions)
      df$t <- as.POSIXct(df$t, format = "%Y-%m-%d %H:%M", tz = "")
      df$v <- as.numeric(df$v)
      return(df)
    }
  }
  
  return(NULL)
}

# Function to get detailed tide predictions (every 6 minutes)
get_detailed_tide_predictions <- function(station_id, start_date, end_date, datum = "MLLW") {
  base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  
  begin_date <- format(start_date, "%Y%m%d")
  end_date <- format(end_date, "%Y%m%d")
  
  params <- list(
    product = "predictions",
    begin_date = begin_date,
    end_date = end_date,
    datum = datum,
    station = station_id,
    time_zone = "lst_ldt",
    units = "english",
    interval = "6",
    format = "json"
  )
  
  response <- GET(base_url, query = params)
  
  if (status_code(response) == 200) {
    content <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    if (!is.null(data$predictions)) {
      df <- as.data.frame(data$predictions)
      df$t <- as.POSIXct(df$t, format = "%Y-%m-%d %H:%M", tz = "")
      df$v <- as.numeric(df$v)
      return(df)
    }
  }
  
  return(NULL)
}

# Function to find threshold crossings
find_threshold_crossings <- function(tide_data, threshold, direction = "below") {
  if (is.null(tide_data) || nrow(tide_data) == 0) {
    return(NULL)
  }
  
  crossings <- data.frame()
  
  for (i in 2:nrow(tide_data)) {
    prev_val <- tide_data$v[i-1]
    curr_val <- tide_data$v[i]
    
    if (direction == "below") {
      # Crossing from above to below threshold
      if (prev_val >= threshold && curr_val < threshold) {
        crossings <- rbind(crossings, data.frame(
          Time = tide_data$t[i],
          WaterLevel = curr_val,
          Direction = "Entering trapping window"
        ))
      }
    } else if (direction == "above") {
      # Crossing from below to above threshold
      if (prev_val <= threshold && curr_val > threshold) {
        crossings <- rbind(crossings, data.frame(
          Time = tide_data$t[i],
          WaterLevel = curr_val,
          Direction = "Exiting trapping window"
        ))
      }
    }
  }
  
  return(crossings)
}

# UI Definition
ui <- fluidPage(
  titlePanel("NOAA CO-OPS Tide Prediction Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("station_id", "NOAA CO-OPS Station ID:", value = 9449679, step = 1, max = 9999999, min = 0000000),
      dateRangeInput("date_range", "Date Range:",
                     start = (Sys.Date() - wday(Sys.Date() - 1)) + 8,
                     end = (Sys.Date() - wday(Sys.Date() - 1)) + 12,
                     min = Sys.Date(),
                     max = Sys.Date() + 365),
      selectInput("datum", "Datum:", 
                  choices = c("MLLW" = "MLLW", "MLW" = "MLW", "MTL" = "MTL", "MSL" = "MSL", "MHW" = "MHW", "MHHW" = "MHHW"),
                  selected = "MLLW"),
      numericInput("threshold", "Trapping Tide Threshold (feet):", value = 3.0, step = 0.1, max = 10, min = 0.1),
      actionButton("fetch_data", "Fetch Tide Data", class = "btn-primary"),
      hr(),
      
      # Information Panel
      helpText("Enter a NOAA COOPS Station ID, select a date range, and set the tide threshold level."),
      helpText("Common stations:"),
      tags$ul(
        tags$li("Drayton Harbor, WA: 9449679"),
        tags$li("Bellingham, WA: 9449211"),
      ),
      helpText("Trapping Tide Thresholds:"),
      tags$ul(
        tags$li("Drayton Harbor, WA: 3 ft"),
        tags$li("Bellingham, WA: 5 ft"),
      ),
      width = 3
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Tide Predictions", value = "predictions",
                 plotOutput("tide_plot", height = "600px"),
                 hr(),
                 h4("Low Tide Summary"),
                 tableOutput("low_tide_table")
        ),
        tabPanel("Trapping Window", value = "threshold",
                 plotOutput("threshold_plot", height = "600px"),
                 hr(),
                 h4("Times when tide crosses below and above thresholds"),
                 DTOutput("threshold_table")
        )
      ),
      width = 9
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  tide_data <- reactiveVal(NULL)
  hilo_data <- reactiveVal(NULL)
  detailed_data <- reactiveVal(NULL)
  
  # Fetch tide data when button is clicked
  observeEvent(input$fetch_data, {
    req(input$station_id, input$datum, input$date_range)
    
    withProgress(message = 'Fetching tide data...', value = 0, {
      start_date <- input$date_range[1]
      end_date <- input$date_range[2]
      sid <- input$station_id
      
      incProgress(0.3, detail = "Getting high/low tides...")
      hilo <- get_tide_predictions(sid, start_date, end_date, input$datum)
      hilo_data(hilo)
      
      incProgress(0.6, detail = "Getting detailed predictions...")
      detailed <- get_detailed_tide_predictions(sid, start_date, end_date, input$datum)
      detailed_data(detailed)
      
      incProgress(1, detail = "Complete!")
    })
    
    if (is.null(detailed_data())) {
      showNotification("Failed to fetch tide data. Please check the station ID and try again.", type = "error")
    }
  })
  
  # Tab 1: Tide Predictions Plot
  output$tide_plot <- renderPlot({
    req(detailed_data(), hilo_data())
    
    detailed <- detailed_data()
    hilo <- hilo_data()
    
    # Filter for low-low tides only
    low_tides <- hilo %>% 
      filter(type == "L") %>%
      mutate(dtg = format(t, "%Y-%m-%d %H:%M"),
             ymd = format(t, "%Y-%m-%d")) %>%
      group_by(ymd) %>% 
      slice_min(v) %>% 
      select(t, dtg, v)
      
    # Create the plot
    p <- ggplot(detailed, aes(x = t, y = v)) +
      geom_line(color = 'darkblue', size = 1) +
      scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
      labs(caption = paste('Low Tide Predictions for Station', input$station_id, 'between',
                           format(input$date_range[1], "%Y-%m-%d"), 'and',
                           format(input$date_range[2], "%Y-%m-%d"), '(Source: NOAA CO-OPS)'),
           title = paste('NOAA Station ID:', input$station_id),
           x = 'Date',
           y = 'Tide Level (ft)') +
      theme_minimal() +
      theme(plot.caption = element_text(size = 16, hjust = 0.5),
            plot.title = element_text(size = 18),
            legend.position = 'none')
    
    # Add low tide annotations
    if (nrow(low_tides) > 0) {
      p <- p + 
        geom_point(data = low_tides, aes(x = t, y = v), color = '#ff5555', size = 3) +
        geom_text(data = low_tides, aes(x = t, y = v, 
                      label = paste(dtg, "\n", round(v, 2), "ft")),
                  vjust = 0, hjust = -0.3, size = 6, color = '#ff5555')
    }
    
    print(p)
  })
  
  # Tab 1: Low Tide Table
  output$low_tide_table <- renderTable({
    req(hilo_data())
    
    hilo <- hilo_data()
    low_tides <- hilo %>%
      filter(type == "L") %>%
      mutate(
        Date = format(t, "%Y-%m-%d"),
        Time = format(t, "%H:%M"),
        `Water Level (ft)` = round(v, 2)
      ) %>%
      group_by(Date) %>% 
      slice_min(`Water Level (ft)`) %>% 
      select(Date, Time, `Water Level (ft)`)
    
    low_tides
  }, striped = TRUE, hover = TRUE)
  
  # Tab 2: Trapping window plot
  output$threshold_plot <- renderPlot({
    req(detailed_data(), input$threshold)
    
    detailed <- detailed_data()
    
    detailed <- detailed %>% 
      mutate(window <- if_else(v <= input$threshold, "yes","no"))
    
    p1 <- ggplot(detailed, aes(x = t, y = v)) +
      geom_line(color = if_else(detailed$window == "yes",'#8be9fd','#ff5555'), size = 1) +
      geom_hline(yintercept = input$threshold,
                 linetype = "dashed",
                 color = "black") +
      scale_x_datetime(date_breaks = "1 day", date_labels = "%b %d") +
      labs(caption = paste('Tidal Window Predictions for Station', input$station_id, 'between',
                           format(input$date_range[1], "%Y-%m-%d"), 'and',
                           format(input$date_range[2], "%Y-%m-%d"), '(Source: NOAA CO-OPS)'),
           title = paste('NOAA Station ID:', input$station_id),
           x = 'Date',
           y = 'Tide Level (ft)') +
      theme_minimal() +
      theme(plot.caption = element_text(size = 16, hjust = 0.5),
            plot.title = element_text(size = 18),
            legend.position = 'none')

    print(p1)
  })
  
  # Tab 2: Trapping window table
  output$threshold_table <- renderDT({
    req(detailed_data(), input$threshold)
    
    detailed <- detailed_data()
    
    # Find and combine threshold crossings
    all_crossings <- do.call(rbind, lapply(c("below", "above"), function(direction) {
      crossings <- find_threshold_crossings(detailed, input$threshold, direction)
      if (!is.null(crossings)) {
        crossings$Threshold <- paste(direction, input$threshold, "ft")
        return(crossings)
      }
      NULL
    }))
    
    # Return message if no crossings found
    if (is.null(all_crossings) || nrow(all_crossings) == 0) {
      return(data.frame(Message = "No threshold crossings found in the selected period."))
    }
    
    all_crossings <- all_crossings %>%
      mutate(
        Date = format(Time, "%Y-%m-%d"),
        `Time (LST)` = format(Time, "%H:%M"),
        `Water Level (ft)` = round(WaterLevel, 2)
      ) %>%
      select(Date, `Time (LST)`, `Water Level (ft)`, Direction, Threshold) %>%
      arrange(Date, `Time (LST)`)
    
    datatable(all_crossings, options = list(pageLength = 25, scrollX = TRUE))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
