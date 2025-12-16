# NOAA CO-OPS Tide Prediction Shiny App

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(DT)
library(plotly)

#### FUNCTIONS FOR CODE ####

# Fetch tide data from NOAA API
get_tide_data <- function(station_id, start_date, end_date, interval, datum = "MLLW") {
  response <- GET("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter", query = list(
    product = "predictions", 
    begin_date = format(start_date, "%Y%m%d"), 
    end_date = format(end_date, "%Y%m%d"),
    datum = datum, 
    station = station_id, 
    time_zone = "lst_ldt", 
    units = "english", 
    interval = interval, 
    format = "json")
  )
  
  if (status_code(response) != 200) return(NULL)
  
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  if (is.null(data$predictions)) return(NULL)
  
  data$predictions %>%
    as.data.frame() %>%
    mutate(t = as.POSIXct(t, format = "%Y-%m-%d %H:%M", tz = ""), v = as.numeric(v))
}

# Find nearest data points to threshold when crossing occurs
find_crossings <- function(tide_data, threshold) {
  if (is.null(tide_data) || nrow(tide_data) < 2) return(NULL)
  
  do.call(rbind, lapply(2:nrow(tide_data), function(i) {
    prev <- tide_data$v[i-1]
    curr <- tide_data$v[i]
    
    # Check if crossing occurs
    if ((prev - threshold) * (curr - threshold) <= 0 && prev != curr) {
      # Find which point is nearest to threshold
      nearest_idx <- if (abs(prev - threshold) < abs(curr - threshold)) i - 1 else i
      
      data.frame(
        t = tide_data$t[nearest_idx],
        v = tide_data$v[nearest_idx],
        direction = if_else(curr > prev, "Rising", "Falling"),
        idx = nearest_idx
      )
    }
  }))
}

# Split tide data into colored segments at threshold crossings
split_segments <- function(tide_data, crossings, threshold) {
  if (is.null(crossings) || nrow(crossings) == 0) {
    return(list(list(data = tide_data, below = mean(tide_data$v) <= threshold)))
  }
  
  crossing_indices <- sort(unique(crossings$idx))
  n_crossings <- length(crossing_indices)
  
  lapply(seq_len(n_crossings + 1), function(i) {
    # Determine segment boundaries
    start_idx <- if (i == 1) 1 else crossing_indices[i - 1]
    end_idx <- if (i <= n_crossings) crossing_indices[i] else nrow(tide_data)
    
    # Determine color by checking a point away from the crossing boundary
    check_idx <- if (i == 1) max(1, end_idx - 1) else min(nrow(tide_data), start_idx + 1)
    
    list(
      data = tide_data[start_idx:end_idx, ],
      below = tide_data$v[check_idx] <= threshold
    )
  })
}

#### UI ####
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("NOAA CO-OPS Tide Prediction Tool"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("station_id", "Station ID:", value = 9449679, min = 0, max = 9999999),
      dateRangeInput("date_range", "Date Range:",
                     start = (Sys.Date() - wday(Sys.Date() - 1)) + 8,
                     end = (Sys.Date() - wday(Sys.Date() - 1)) + 12,
                     min = Sys.Date(), max = Sys.Date() + 365),
      selectInput("datum", "Datum:", choices = c("MLLW", "MLW", "MTL", "MSL", "MHW", "MHHW"), selected = "MLLW"),
      numericInput("threshold", "Threshold (ft):", value = 3.0, min = 0.1, max = 10, step = 0.1),
      actionButton("fetch_data", "Fetch Data", class = "btn-primary"),
      hr(),
      helpText("Common Stations: "),
      tags$ul(tags$li("Drayton Harbor, WA:  9449679"), tags$li("Cherry Point, WA: 9449424")),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graph", plotlyOutput("tide_plot", height = "60vh")),
        tabPanel("Tables", h4("Threshold Crossings"), DTOutput("crossing_table"), hr(), 
                 h4("Daily Low Tides"), DTOutput("low_table"))
      ),
      width = 9
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {
  
  # Store fetched data
  tide_hilo <- reactiveVal(NULL)
  tide_detailed <- reactiveVal(NULL)
  
  # Fetch data on button click
  observeEvent(input$fetch_data, {
    req(input$station_id, input$date_range)
    
    withProgress(message = 'Fetching...  ', {
      tide_hilo(get_tide_data(input$station_id, input$date_range[1], input$date_range[2], 'hilo', input$datum))
      tide_detailed(get_tide_data(input$station_id, input$date_range[1], input$date_range[2], '6', input$datum))
    })
    
    if (is.null(tide_detailed())) showNotification("Failed to fetch data", type = "error")
  })
  
  # Plot
  output$tide_plot <- renderPlotly({
    req(tide_detailed(), tide_hilo(), input$threshold)
    
    # Prepare data
    data <- tide_detailed()
    crossings <- find_crossings(data, input$threshold)
    daily_lows <- tide_hilo() %>%
      filter(type == "L") %>%
      mutate(date = format(t, "%Y-%m-%d")) %>%
      group_by(date) %>%
      arrange(v) %>%
      slice(1) %>%
      ungroup() %>%
      distinct(date, .keep_all = TRUE)  
    
    segments <- split_segments(data, crossings, input$threshold)
    
    # Build plot
    p <- plot_ly()
    
    # Add colored tide segments
    for (seg in segments) {
      if (nrow(seg$data) > 0) {
        color <- if (seg$below) '#8be9fd' else '#ff5555'
        p <- p %>% add_trace(
          data = seg$data, x = ~t, y = ~v, type = 'scatter', mode = 'lines',
          line = list(color = color, width = 2),
          showlegend = FALSE,
          hovertemplate = '<b>%{x|%Y-%m-%d %H:%M}</b><br>%{y:.2f} ft<extra></extra>'
        )
      }
    }
    
    # Add threshold line
    p <- p %>% add_trace(
      x = range(data$t), y = rep(input$threshold, 2),
      type = 'scatter', mode = 'lines', line = list(color = 'black', width = 2, dash = 'dash'),
      showlegend = FALSE, hovertemplate = paste('Threshold:', input$threshold, 'ft<extra></extra>')
    )
    
    # Add crossing markers
    if (!is.null(crossings) && nrow(crossings) > 0) {
      p <- p %>% add_trace(
        data = crossings, x = ~t, y = ~v, type = 'scatter', mode = 'markers',
        marker = list(size = 10, color = '#ffb86c', line = list(color = '#282a36', width = 2)),
        text = ~direction, showlegend = FALSE,
        hovertemplate = '<b>Crossing:   %{text}</b><br>%{x|%m/%d %H:%M}<br>%{y:.2f} ft<extra></extra>'
      )
    }
    
    # Add low tide markers
    p <- p %>% add_trace(
      data = daily_lows, x = ~t, y = ~v, type = 'scatter', mode = 'markers',
      marker = list(size = 10, color = '#50fa7b', line = list(color = '#282a36', width = 2)),
      showlegend = FALSE,
      hovertemplate = '<b>Low Tide</b><br>%{y:. 2f} ft @ %{x|%H:%M}<extra></extra>'
    )
    
    # Build annotations list
    annotations <- list()
    
    # Add crossing annotations
    if (!is.null(crossings) && nrow(crossings) > 0) {
      for (i in 1:nrow(crossings)) {
        annotations[[length(annotations) + 1]] <- list(
          x = crossings$t[i], 
          y = crossings$v[i],
          text = paste0(crossings$direction[i], "<br>", sprintf("%.2f ft", crossings$v[i]), "<br>", format(crossings$t[i], "%m/%d %H:%M")),
          showarrow = TRUE, 
          arrowhead = 2, 
          arrowsize = 1,
          arrowwidth = 2,
          arrowcolor = '#ffb86c',
          ax = if_else(crossings$direction[i] == "Rising", 40, -40), 
          ay = -40,
          bgcolor = 'white', 
          bordercolor = '#ffb86c', 
          borderwidth = 2,
          borderpad = 4,
          font = list(size = 10, color = '#282a36')
        )
      }
    }
    
    # Add low tide annotations
    for (i in 1:nrow(daily_lows)) {
      annotations[[length(annotations) + 1]] <- list(
        x = daily_lows$t[i],
        y = daily_lows$v[i],
        text = paste0("Low Tide<br>", sprintf("%.2f ft", daily_lows$v[i]), "<br>", format(daily_lows$t[i], "%H:%M")),
        showarrow = TRUE, 
        arrowhead = 2, 
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = '#50fa7b',
        ax = 0, 
        ay = 40,
        bgcolor = 'white', 
        bordercolor = '#50fa7b', 
        borderwidth = 2,
        borderpad = 4,
        font = list(size = 10, color = '#282a36')
      )
    }
    
    # Layout
    p %>% layout(
      title = paste('Station', input$station_id),
      xaxis = list(title = 'Date', tickformat = '%b %d'),
      yaxis = list(title = 'Tide Level (ft)'),
      annotations = annotations,
      hovermode = 'closest'
    )
  })
  
  # Crossing table
  output$crossing_table <- renderDT({
    req(tide_detailed(), input$threshold)
    
    crossings <- find_crossings(tide_detailed(), input$threshold)
    
    if (is.null(crossings) || nrow(crossings) == 0) {
      return(data.frame(Message = "No crossings found"))
    }
    
    crossings %>%
      mutate(Date = format(t, "%Y-%m-%d"), Time = format(t, "%H:%M"), Level = round(v, 2)) %>%
      select(Date, Time, Level, Direction = direction) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # Low tide table
  output$low_table <- renderDT({
    req(tide_hilo())
    
    tide_hilo() %>%
      filter(type == "L") %>%
      mutate(Date = format(t, "%Y-%m-%d"), Time = format(t, "%H:%M"), Level = round(v, 2)) %>%
      group_by(Date) %>%
      slice_min(Level, n = 1, with_ties = FALSE) %>%
      select(Date, Time, Level) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
}

shinyApp(ui, server)