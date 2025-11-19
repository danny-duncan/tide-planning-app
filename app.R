# NOAA CO-OPS Tide Prediction Shiny App
# Application for European Green Crab (EGC) field planning purposes

library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)

# Function to query NOAA CO-OPS API for tide predictions
get_tide_predictions <- function(station_id, start_date, end_date, datum = "MLLW") {
  base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  
  # Format dates for API
  begin_date <- format(start_date, "%Y%m%d")
  end_date <- format(end_date, "%Y%m%d")
  
  # API parameters
  params <- list(
    product = "predictions",
    application = "NOS.COOPS.TAC.WL",
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
    application = "NOS.COOPS.TAC.WL",
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
          Direction = "Falling below threshold"
        ))
      }
    } else if (direction == "above") {
      # Crossing from below to above threshold
      if (prev_val <= threshold && curr_val > threshold) {
        crossings <- rbind(crossings, data.frame(
          Time = tide_data$t[i],
          WaterLevel = curr_val,
          Direction = "Rising above threshold"
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
      textInput("station_id", "NOAA Station ID:", value = "9414290"),
      helpText("Example stations: 9414290 (San Francisco), 8454000 (Providence), 8518750 (The Battery, NY)"),
      dateInput("start_date", "Start Date:", value = Sys.Date()),
      numericInput("days", "Number of Days:", value = 7, min = 1, max = 31),
      selectInput("datum", "Datum:", 
                  choices = c("MLLW" = "MLLW", "MLW" = "MLW", "MTL" = "MTL", "MSL" = "MSL", "MHW" = "MHW", "MHHW" = "MHHW"),
                  selected = "MLLW"),
      actionButton("fetch_data", "Fetch Tide Data", class = "btn-primary"),
      hr(),
      conditionalPanel(
        condition = "input.tabs == 'threshold'",
        numericInput("threshold_low", "Low Threshold (feet):", value = 2.0, step = 0.1),
        numericInput("threshold_high", "High Threshold (feet):", value = 5.0, step = 0.1)
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Tide Predictions", value = "predictions",
                 plotOutput("tide_plot", height = "600px"),
                 hr(),
                 h4("Low Tide Summary"),
                 tableOutput("low_tide_table")
        ),
        tabPanel("Threshold Crossings", value = "threshold",
                 h4("Times when tide crosses below and above thresholds"),
                 DTOutput("threshold_table")
        ),
        tabPanel("Coastal Inundation", value = "inundation",
                 h4("NOAA Coastal Inundation Predictions"),
                 htmlOutput("inundation_info"),
                 uiOutput("inundation_map")
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
    req(input$station_id, input$start_date, input$days)
    
    withProgress(message = 'Fetching tide data...', value = 0, {
      start_date <- input$start_date
      end_date <- start_date + days(input$days)
      
      incProgress(0.3, detail = "Getting high/low tides...")
      hilo <- get_tide_predictions(input$station_id, start_date, end_date, input$datum)
      hilo_data(hilo)
      
      incProgress(0.6, detail = "Getting detailed predictions...")
      detailed <- get_detailed_tide_predictions(input$station_id, start_date, end_date, input$datum)
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
    
    # Filter for low tides only
    low_tides <- hilo %>% filter(type == "L")
    
    # Create the plot
    p <- ggplot(detailed, aes(x = t, y = v)) +
      geom_line(color = "blue", size = 1) +
      labs(
        title = paste("Tide Predictions - Station", input$station_id),
        subtitle = paste("From", format(input$start_date, "%Y-%m-%d"), "for", input$days, "days"),
        x = "Date/Time",
        y = paste("Water Level (feet,", input$datum, ")")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_line(linetype = "dotted")
      )
    
    # Add low tide annotations
    if (nrow(low_tides) > 0) {
      p <- p + 
        geom_point(data = low_tides, aes(x = t, y = v), color = "red", size = 3) +
        geom_text(data = low_tides, 
                  aes(x = t, y = v, 
                      label = paste(format(t, "%m/%d %H:%M"), "\n", round(v, 2), "ft")),
                  vjust = -0.5, hjust = 0.5, size = 3, color = "red")
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
      select(Date, Time, `Water Level (ft)`)
    
    low_tides
  }, striped = TRUE, hover = TRUE)
  
  # Tab 2: Threshold Crossings Table
  output$threshold_table <- renderDT({
    req(detailed_data(), input$threshold_low, input$threshold_high)
    
    detailed <- detailed_data()
    
    # Find crossings below threshold
    crossings_below <- find_threshold_crossings(detailed, input$threshold_low, "below")
    if (!is.null(crossings_below)) {
      crossings_below$Threshold <- paste("Below", input$threshold_low, "ft")
    }
    
    # Find crossings above threshold
    crossings_above <- find_threshold_crossings(detailed, input$threshold_high, "above")
    if (!is.null(crossings_above)) {
      crossings_above$Threshold <- paste("Above", input$threshold_high, "ft")
    }
    
    # Combine results
    all_crossings <- rbind(crossings_below, crossings_above)
    
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
  
  # Tab 3: Coastal Inundation
  output$inundation_info <- renderUI({
    req(input$station_id)
    
    HTML(paste0(
      "<p>The NOAA Coastal Inundation Dashboard provides real-time and forecasted coastal flooding information.</p>",
      "<p><strong>Station ID:</strong> ", input$station_id, "</p>",
      "<p>For more information and interactive maps, visit the official NOAA resources:</p>",
      "<ul>",
      "<li><a href='https://tidesandcurrents.noaa.gov/inundationdb/' target='_blank'>NOAA Inundation Dashboard</a></li>",
      "<li><a href='https://tidesandcurrents.noaa.gov/stationhome.html?id=", input$station_id, "' target='_blank'>Station ", input$station_id, " Information</a></li>",
      "<li><a href='https://coast.noaa.gov/slr/' target='_blank'>NOAA Sea Level Rise Viewer</a></li>",
      "<li><a href='https://water.weather.gov/ahps/' target='_blank'>NWS Advanced Hydrologic Prediction Service</a></li>",
      "</ul>",
      "<p><em>Note: Real-time coastal inundation forecasts are available through the NOAA Inundation Dashboard. ",
      "The dashboard integrates water level observations, tide predictions, and storm surge guidance to provide ",
      "coastal flood forecasts up to 48 hours in advance.</em></p>"
    ))
  })
  
  output$inundation_map <- renderUI({
    req(input$station_id)
    
    # Embed the NOAA inundation map for the station
    tags$div(
      style = "margin-top: 20px;",
      tags$h4("NOAA Inundation Map"),
      tags$iframe(
        src = paste0("https://tidesandcurrents.noaa.gov/inundationdb/?id=", input$station_id),
        width = "100%",
        height = "600px",
        frameborder = "0",
        style = "border: 1px solid #ccc;"
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
