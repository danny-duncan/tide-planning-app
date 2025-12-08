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
library(plotly)

#### FUNCTIONS FOR CODE ####
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
    
    if (! is.null(data$predictions)) {
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

####### UI Definition ######
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
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
        tags$li("Cherry Point, WA: 9449424")
      ),
      helpText("Trapping Tide Thresholds:"),
      tags$ul(
        tags$li("Drayton Harbor, WA: 3 ft"),
        tags$li("Cherry Point, WA: 5 ft")
      ),
      width = 3
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Tide Prediction Graph", value = "predictions",
                 plotlyOutput("threshold_plot", height = "60vh")
        ),
        tabPanel("Tide Prediction Table", value = "threshold",
                 h4("Tide Threshold Times"),
                 DTOutput("threshold_table"),
                 hr(),
                 h4("Low Tide Times"),
                 DTOutput("lowtide_table")
        )
      ),
      width = 9
    )
  )
)

###### Server Logic ######
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

#### Tab 1: Tide Prediction Plotly ####
  output$threshold_plot <- renderPlotly({
    req(detailed_data(), input$threshold)
    
    detailed <- detailed_data()
    
    detailed <- detailed %>% 
      mutate(window = if_else(v <= input$threshold, "yes", "no"))
    
    # Find intersection points where the data crosses the threshold
    intersections <- do.call(rbind, lapply(2:nrow(detailed), function(i) {
      prev_val <- detailed$v[i-1]
      curr_val <- detailed$v[i]
      
      # Check if line crosses threshold
      if ((prev_val <= input$threshold && curr_val > input$threshold) ||
          (prev_val > input$threshold && curr_val <= input$threshold)) {
        
        # Linear interpolation to find exact intersection point
        fraction <- (input$threshold - prev_val) / (curr_val - prev_val)
        t_intersect <- detailed$t[i-1] + fraction * as.numeric(difftime(detailed$t[i], detailed$t[i-1], units = "secs"))
        
        data.frame(
          t = as.POSIXct(t_intersect, origin = "1970-01-01", tz = attr(detailed$t, "tzone")),
          v = input$threshold,
          direction = if_else(curr_val > prev_val, "Rising", "Falling"),
          idx = i
        )
      }
    }))
    
    # Find lowest low tide for each day
    daily_lows <- detailed %>%
      mutate(date = as.Date(t)) %>%
      group_by(date) %>%
      slice_min(v, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    # Create segments by splitting at intersections
    segments <- list()
    segment_idx <- 1
    start_idx <- 1
    
    if (! is.null(intersections) && nrow(intersections) > 0) {
      for (i in 1:nrow(intersections)) {
        # Add segment up to intersection
        end_idx <- intersections$idx[i] - 1
        segment_data <- detailed[start_idx:end_idx, ]
        # Add intersection point to end of segment
        segment_data <- rbind(segment_data, data.frame(
          t = intersections$t[i],
          v = intersections$v[i],
          window = segment_data$window[nrow(segment_data)]
        ))
        segments[[segment_idx]] <- segment_data
        segment_idx <- segment_idx + 1
        
        # Start new segment from intersection
        start_idx <- intersections$idx[i]
      }
      # Add final segment
      segment_data <- detailed[start_idx:nrow(detailed), ]
      # Add intersection point to start of segment
      last_intersection <- intersections[nrow(intersections), ]
      segment_data <- rbind(data.frame(
        t = last_intersection$t,
        v = last_intersection$v,
        window = segment_data$window[1]
      ), segment_data)
      segments[[segment_idx]] <- segment_data
    } else {
      # No intersections, use entire dataset
      segments[[1]] <- detailed
    }
    
    # Create annotations list with the existing subtitle
    annotations_list <- list(
      list(
        text = paste('Tidal Window Predictions for Station', input$station_id, 'between',
                     format(input$date_range[1], "%Y-%m-%d"), 'and',
                     format(input$date_range[2], "%Y-%m-%d"), '(Source: NOAA CO-OPS)'),
        xref = 'paper',
        yref = 'paper',
        x = 0.5,
        y = -0.15,
        xanchor = 'center',
        yanchor = 'top',
        showarrow = FALSE,
        font = list(size = 16)
      )
    )
    
    # Add intersection annotations
    if (!is.null(intersections) && nrow(intersections) > 0) {
      for (i in 1:nrow(intersections)) {
        annotations_list[[length(annotations_list) + 1]] <- list(
          x = intersections$t[i],
          y = intersections$v[i],
          text = paste0(intersections$direction[i], "<br>", 
                        format(intersections$t[i], "%m/%d %H:%M")),
          showarrow = TRUE,
          arrowhead = 2,
          arrowsize = 1,
          arrowwidth = 2,
          arrowcolor = '#ffb86c',
          ax = if_else(intersections$direction[i] == "Rising", 40, -40),
          ay = -40,
          bgcolor = 'rgba(255, 255, 255, 0.9)',
          bordercolor = '#ffb86c',
          borderwidth = 2,
          borderpad = 4,
          font = list(size = 10, color = '#282a36')
        )
      }
    }
    
    # Add daily low tide annotations
    if (nrow(daily_lows) > 0) {
      for (i in 1:nrow(daily_lows)) {
        annotations_list[[length(annotations_list) + 1]] <- list(
          x = daily_lows$t[i],
          y = daily_lows$v[i],
          text = paste0("Low Tide<br>", sprintf("%.2f ft", daily_lows$v[i]), "<br>",
                        format(daily_lows$t[i], "%H:%M")),
          showarrow = TRUE,
          arrowhead = 2,
          arrowsize = 1,
          arrowwidth = 2,
          arrowcolor = '#50fa7b',
          ax = 0,
          ay = 40,
          bgcolor = 'rgba(255, 255, 255, 0.9)',
          bordercolor = '#50fa7b',
          borderwidth = 2,
          borderpad = 4,
          font = list(size = 10, color = '#282a36')
        )
      }
    }
    
    # Create the plotly object
    p1 <- plot_ly()
    
    # Add each segment as a separate trace
    for (seg in segments) {
      if (nrow(seg) > 0) {
        color <- if_else(seg$window[1] == "yes", '#8be9fd', '#ff5555')
        p1 <- p1 %>%
          add_trace(
            data = seg,
            x = ~t, 
            y = ~v,
            type = 'scatter',
            mode = 'lines',
            line = list(color = color, width = 2),
            showlegend = FALSE,
            hovertemplate = paste(
              '<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>',
              '<b>Tide Level:</b> %{y:. 2f} ft<br>',
              '<extra></extra>'
            )
          )
      }
    }
    
    # Add horizontal threshold line
    p1 <- p1 %>%
      add_trace(
        x = range(detailed$t),
        y = c(input$threshold, input$threshold),
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'black', width = 2, dash = 'dash'),
        name = 'Threshold',
        showlegend = FALSE,
        hovertemplate = paste('<b>Threshold:</b>', input$threshold, 'ft<extra></extra>')
      )
    
    # Add intersection points as markers
    if (!is.null(intersections) && nrow(intersections) > 0) {
      p1 <- p1 %>%
        add_trace(
          data = intersections,
          x = ~t,
          y = ~v,
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 10, color = '#ffb86c', 
                        line = list(color = '#282a36', width = 2)),
          name = 'Threshold Crossing',
          showlegend = FALSE,
          hovertemplate = paste(
            '<b>Threshold Crossing</b><br>',
            '<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>',
            '<b>Direction:</b> %{text}<br>',
            '<extra></extra>'
          ),
          text = ~direction
        )
    }
    
    # Add daily low tide markers
    if (nrow(daily_lows) > 0) {
      p1 <- p1 %>%
        add_trace(
          data = daily_lows,
          x = ~t,
          y = ~v,
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 10, color = '#50fa7b', 
                        line = list(color = '#282a36', width = 2)),
          name = 'Daily Low Tide',
          showlegend = FALSE,
          hovertemplate = paste(
            '<b>Daily Low Tide</b><br>',
            '<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>',
            '<b>Tide Level:</b> %{y:.2f} ft<br>',
            '<extra></extra>'
          )
        )
    }
    
    # Apply layout with annotations
    p1 <- p1 %>%
      layout(
        title = list(
          text = paste('NOAA Station ID:', input$station_id),
          font = list(size = 18)
        ),
        xaxis = list(
          title = 'Date',
          tickformat = '%b %d',
          dtick = 86400000  # 1 day in milliseconds
        ),
        yaxis = list(
          title = 'Tide Level (ft)'
        ),
        annotations = annotations_list,
        hovermode = 'closest',
        showlegend = FALSE
      )
    
  })
  
#### Tab 2: Trapping window table ####
  output$threshold_table <- renderDT({
    req(detailed_data(), input$threshold)
    
    detailed <- detailed_data()
    
    # Find and combine threshold crossings
    all_crossings <- do.call(rbind, lapply(c("below", "above"), function(direction) {
      crossings <- find_threshold_crossings(detailed, input$threshold, direction)
      if (! is.null(crossings)) {
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
      select(Date, `Time (LST)`, `Water Level (ft)`, Direction) %>%
      arrange(Date, `Time (LST)`)
    
    datatable(all_crossings, options = list(dom = 'tip'))
    
  })
  
#### Tab 2: Low Tide Table ####
  output$lowtide_table <- renderDT({
    req(hilo_data())
    
    hilo <- hilo_data()
    low_tides <- hilo %>%
      filter(type == "L") %>%
      mutate(
        Date = format(t, "%Y-%m-%d"),
        `Time (LST)` = format(t, "%H:%M"),
        `Water Level (ft)` = round(v, 2)
      ) %>%
      group_by(Date) %>% 
      slice_min(`Water Level (ft)`) %>% 
      select(Date, `Time (LST)`, `Water Level (ft)`)
    
    datatable(
      low_tides,
      options = list(dom = 'tip'),
      rownames = FALSE,
    )
  })
    
}

# Run the application
shinyApp(ui = ui, server = server)