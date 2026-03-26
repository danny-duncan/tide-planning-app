---
editor_options: 
  markdown: 
    wrap: 72
---

# NOAA CO-OPS Tide Planning Shiny App - Technical Documentation

## Table of Contents

1.  [Overview](#overview)
2.  [Architecture](#architecture)
3.  [Code Components](#code-components)
4.  [Dependencies](#dependencies)
5.  [Deployment Guide](#deployment-guide)
6.  [API Integration](#api-integration)
7.  [Troubleshooting](#troubleshooting)

------------------------------------------------------------------------

## Overview {#overview}

The **NOAA CO-OPS Tide Planning Shiny App** is a web-based application
designed for field planning purposes, specifically for European Green
Crab (EGC) management. It provides real-time tide predictions from the
National Oceanic and Atmospheric Administration (NOAA) Center for
Operational Oceanographic Products and Services (CO-OPS) API.

**Key Features:** - Interactive tide prediction visualization -
Threshold crossing detection and analysis - Daily low tide
identification - Customizable vertical datum reference - NOAA station
selection (9,000+ available stations) - Date range filtering - Real-time
data fetching

------------------------------------------------------------------------

## Architecture {#architecture}

### Technology Stack

-   **Frontend Framework:** Shiny (R interactive web framework)
-   **Backend Language:** R (≥ 4.0.0)
-   **Data Source:** NOAA CO-OPS REST API
-   **Visualization:** Plotly (interactive graphs) + DataTables (DT)
-   **Data Processing:** dplyr, lubridate

### Application Flow

```         
┌──────────────────────────┐
│   User Interface (UI)    │
│  - Input Controls        │
│  - Parameter Selection   │
└────────────┬─────────────┘
             │
             ▼
┌──────────────────────────┐
│   Server Logic           │
│  - Event Listeners       │
│  - Data Processing       │
│  - Reactive Values       │
└────────────┬─────────────┘
             │
             ▼
┌──────────────────────────┐
│  NOAA CO-OPS API         │
│  - REST Endpoint         ���
│  - JSON Response         │
└──────────────────────────┘
```

------------------------------------------------------------------------

## Code Components {#code-components}

### 1. **Data Fetching Function: `get_tide_data()`**

**Purpose:** Retrieves tide prediction data from the NOAA CO-OPS API

**Parameters:** - `station_id` - NOAA station identifier (numeric) -
`start_date` - Query start date (Date object) - `end_date` - Query end
date (Date object) - `interval` - Data interval (`"hilo"` for high/low
only, `"6"` for 6-minute intervals) - `datum` - Vertical reference
(default: `"MLLW"`)

**API Request Details:**

``` r
GET("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter", 
    query = list(
      product = "predictions",           # Only predictions, not observations
      begin_date = "%Y%m%d",             # Start date (YYYYMMDD format)
      end_date = "%Y%m%d",               # End date (YYYYMMDD format)
      datum = datum,                      # Vertical datum reference
      station = station_id,               # NOAA station ID
      time_zone = "lst_ldt",              # Local time (auto DST adjustment)
      units = "english",                  # Feet (not meters)
      interval = interval,                # Data granularity
      format = "json"                     # Response format
    )
)
```

**Data Processing:** - Validates HTTP status code (expects 200) - Parses
JSON response - Converts timestamp strings to POSIXct (datetime) -
Converts water level values to numeric - Returns formatted dataframe

**Output Format:**

```         
  t (POSIXct)        v (numeric)    type (character)
  2026-03-23 10:30   2.45           "L"
  2026-03-23 16:15   4.12           "H"
```

### 2. **Threshold Crossing Detection: `find_crossings()`**

**Purpose:** Identifies when tide levels cross a user-defined threshold

**Algorithm:** 1. Iterates through consecutive tide data points 2.
Checks if sign change occurs across threshold 3. Identifies which point
is nearest to threshold value 4. Classifies crossing as "Rising" or
"Falling"

**Key Logic:**

``` r
if ((prev - threshold) * (curr - threshold) <= 0 && prev != curr) {
  # A crossing occurred between these two points
}
```

**Output Format:**

```         
  t (POSIXct)        v (numeric)    direction (character)    idx (integer)
  2026-03-23 10:45   3.00           "Rising"                 45
  2026-03-23 15:30   3.02           "Falling"                92
```

### 3. **Segment Coloring: `split_segments()`**

**Purpose:** Divides tide data into segments colored by whether they're
above/below threshold

**Logic:** - Uses crossing indices as segment boundaries - Colors
segments red (#ff5555) when above threshold - Colors segments cyan
(#8be9fd) when below threshold - Maintains data continuity across
segments

**Output:** List of segments, each containing: - `data` - Dataframe
subset for segment - `below` - Boolean (TRUE if segment is below
threshold)

### 4. **User Interface (UI)**

**Layout:** SidebarLayout with dark theme (Darkly Bootswatch)

**Sidebar Controls:** \| Control \| Type \| Default \| Range \|
\|---------\|------\|---------\|-------\| \| Station ID \| Numeric \|
9449679 (Drayton Harbor, WA) \| 0-9999999 \| \| Date Range \| Date \|
Mon-Fri of next week \| Today to +365 days \| \| Datum \| Select \| MLLW
\| MLLW, MLW, MTL, MSL, MHW, MHHW \| \| Threshold \| Numeric \| 3.0 ft
\| 0.1-10 ft \| \| Show Labels \| Checkbox \| TRUE \| Boolean \| \|
Fetch Data \| Button \| - \| Triggers API call \|

**Main Panel Tabs:** 1. **Graph Tab:** Interactive Plotly visualization
2. **Tables Tab:** Data tables for crossings and low tides

### 5. **Server Logic**

**Reactive Values:** - `tide_hilo` - High/low tide data (used for daily
low identification) - `tide_detailed` - 6-minute interval data (used for
detailed analysis)

**Event Observers:**

``` r
observeEvent(input$fetch_data, {
  # Triggers on button click
  # Fetches both datasets
  # Shows progress indicator
  # Displays error notification on failure
})
```

**Reactive Outputs:** 1. **tide_plot** - Plotly visualization with: -
Colored tide segments - Threshold line (dashed black) - Crossing markers
(orange) - Low tide markers (green) - Optional annotations with
direction/time/level

2.  **crossing_table** - DataTable showing:
    -   Date, Time, Level, Direction for each threshold crossing
3.  **low_table** - DataTable showing:
    -   Daily minimum low tide levels

------------------------------------------------------------------------

## Dependencies {#dependencies}

### R Package Requirements

```         
Package      Version    Purpose
─────────────────────────────────────────────────
shiny        ≥ 1.7.0    Web application framework
httr         ≥ 1.4.0    HTTP requests to NOAA API
jsonlite     ≥ 1.8.0    JSON parsing
ggplot2      ≥ 3.4.0    Not directly used (legacy)
dplyr        ≥ 1.1.0    Data manipulation (pipe, filter, mutate)
lubridate    ≥ 1.9.0    Date/time parsing and formatting
DT           ≥ 0.27     Interactive DataTables
plotly       (implicit) Interactive visualization
bslib        (implicit) Bootstrap theme support
```

**Installation:**

``` r
# Install required packages
packages <- c("shiny", "httr", "jsonlite", "ggplot2", 
              "dplyr", "lubridate", "DT", "plotly", "bslib")
install.packages(packages)
```

------------------------------------------------------------------------

## Deployment Guide {#deployment-guide}

### Option 1: Local Development (Quick Start)

**Requirements:** - R ≥ 4.0.0 - RStudio (recommended) - Internet
connection (for API calls)

**Steps:** 1. Clone the repository:
`bash    git clone https://github.com/danny-duncan/tide-planning-app.git    cd tide-planning-app`

2.  Open `app.R` in RStudio

3.  Install dependencies:

    ``` r
    # In R console:
    source("install_dependencies.R")  # If available
    # OR manually:
    install.packages(c("shiny", "httr", "jsonlite", "ggplot2", 
                       "dplyr", "lubridate", "DT", "plotly", "bslib"))
    ```

4.  Run the application:

    ``` r
    shiny::runApp()
    ```

    The app will open in your default browser at `http://localhost:3838`

### Option 2: Shinyapps.io (Cloud Hosting - Easiest)

**Requirements:** - RStudio account (free: <https://rstudio.cloud>) -
Shinyapps.io account (free tier available)

**Deployment:**

1.  **Set up Shinyapps.io:**

    -   Create account at <https://www.shinyapps.io>
    -   Copy deployment token from dashboard

2.  **Deploy from RStudio:**

    ``` r
    # Install deployment tools
    install.packages('rsconnect')

    # Authenticate
    rsconnect::setAccountInfo(name='<your-account>', 
                              token='<your-token>', 
                              secret='<your-secret>')

    # Deploy application
    rsconnect::deployApp(appDir = '.', appName = 'tide-planning-app')
    ```

3.  **Access:** `https://<your-account>.shinyapps.io/tide-planning-app`

**Limitations (Free Tier):** - 25 active hours per month - 5 concurrent
users - 1 GB data per month - No custom domain

------------------------------------------------------------------------

## API Integration {#api-integration}

### NOAA CO-OPS API Details

**Base URL:**
`https://api.tidesandcurrents.noaa.gov/api/prod/datagetter`

**Products Available:** - `predictions` - Tide level predictions -
`water_level` - Observed water levels - `air_temperature` - Air
temperature observations - `wind` - Wind observations

**Datums (Vertical References):** \| Code \| Name \| Description \|
\|------\|------\|-------------\| \| MLLW \| Mean Lower Low Water \|
Average of lower low water heights (default) \| \| MLW \| Mean Low Water
\| Average of all low water heights \| \| MTL \| Mean Tide Level \|
Average of MHHW and MLLW \| \| MSL \| Mean Sea Level \| Average of all
hourly levels \| \| MHW \| Mean High Water \| Average of all high water
heights \| \| MHHW \| Mean Higher High Water \| Average of higher high
water heights \|

**Intervals:** - `"hilo"` - High/low water times only (\~2-4 data points
per day) - `"6"` - 6-minute intervals (\~240 data points per day) -
`"60"` - 60-minute intervals (\~24 data points per day)

**Finding Station IDs:** - Browse:
<https://tidesandcurrents.noaa.gov/> - API:
`https://api.tidesandcurrents.noaa.gov/api/prod/stations` - Search by
location name or coordinates

**Example Request:**

```         
https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?
  station=9449679&
  begin_date=20260323&
  end_date=20260327&
  product=predictions&
  datum=MLLW&
  units=english&
  time_zone=lst_ldt&
  format=json
```

**Example Response:**

``` json
{
  "predictions": [
    {
      "t": "2026-03-23 10:30",
      "v": "2.453",
      "type": "L"
    },
    {
      "t": "2026-03-23 16:15",
      "v": "4.124",
      "type": "H"
    }
  ]
}
```

### Rate Limiting

-   **Limit:** 10 requests per second per IP
-   **No authentication required** for standard access
-   **API availability:** Generally 99.9% uptime
-   **Data availability:** Current +365 days

------------------------------------------------------------------------

## Troubleshooting {#troubleshooting}

### Common Issues

#### 1. "Failed to fetch tide data" Error

**Causes & Solutions:**

``` r
# Check 1: Verify station ID exists
httr::GET("https://api.tidesandcurrents.noaa.gov/api/prod/stations",
          query = list(format = "json")) %>%
  httr::content() %>%
  head(10)

# Check 2: Verify date range
# NOAA can only predict 365 days into future
current_date <- Sys.Date()
max_future_date <- current_date + 365

# Check 3: Test API connectivity
httr::GET("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter",
          query = list(
            station = 9449679,
            begin_date = "20260323",
            end_date = "20260327",
            product = "predictions",
            format = "json"
          )) %>%
  httr::status_code()  # Should return 200
```

#### 2. Plot Not Rendering

**Causes:** - No data fetched (click "Fetch Data" button) - Date range
selection error - Empty threshold crossing result

**Solution:**

``` r
# Debug: Check what data was fetched
tide_detailed()  # Should not be NULL
nrow(tide_detailed())  # Should be > 0
```

#### 3. Slow Performance

**Optimization:** - Use `interval = "6"` instead of smaller intervals
for large date ranges - Limit date range to ≤ 30 days for faster
queries - Check network connectivity

#### 4. Package Installation Errors

**Common errors:**

``` r
# Missing system libraries
# Ubuntu: sudo apt-get install libcurl4-openssl-dev libssl-dev

# R compilation issues
# Try specifying CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages("httr")
```

### Debug Mode

Enable verbose logging in `app.R`:

``` r
# Add after library imports
options(shiny.trace = TRUE)

# In server function, add:
observeEvent(input$fetch_data, {
  cat("DEBUG: Fetching data for station", input$station_id, "\n")
  cat("DEBUG: Date range:", input$date_range[1], "-", input$date_range[2], "\n")
  
  tide_detailed(get_tide_data(...))
  
  cat("DEBUG: Received", nrow(tide_detailed()), "rows\n")
})
```

------------------------------------------------------------------------

## Performance Considerations

### Memory Usage

-   30-day period with 6-minute intervals: \~8,600 data points (\~1 MB)
-   365-day period: \~104,000 data points (\~12 MB)
-   Typical browser memory: 50-200 MB

### API Response Times

-   Station query: 0.5-2 seconds
-   30-day prediction: 1-3 seconds
-   365-day prediction: 2-5 seconds

### Optimization Tips

1.  **Limit historical data:**

    ``` r
    # Reduce default date range
    end = (Sys.Date() - wday(Sys.Date() - 1)) + 12,  # 1 week
    start = (Sys.Date() - wday(Sys.Date() - 1)) + 8,
    ```

2.  **Cache API responses:**

    ``` r
    # Add caching if fetching same station repeatedly
    cached_data <- reactiveValues(station = NULL, data = NULL)
    ```

3.  **Implement data sampling for display:**

    ``` r
    # Sample points for very large datasets
    if (nrow(tide_detailed()) > 5000) {
      tide_detailed(tide_detailed()[seq(1, nrow(tide_detailed()), 5), ])
    }
    ```

------------------------------------------------------------------------

## License & Attribution

-   **Application:** Created by NWSC Planning Team
-   **Data Source:** NOAA CO-OPS (public domain)
-   **Citation:** National Oceanic and Atmospheric Administration,
    Center for Operational Oceanographic Products and Services

------------------------------------------------------------------------

## Support & Resources

-   **NOAA API Documentation:**
    <https://api.tidesandcurrents.noaa.gov/api/prod/>
-   **Shiny Documentation:** <https://shiny.rstudio.com/>
-   **Station Finder:** <https://tidesandcurrents.noaa.gov/>
-   **Report Issues:** GitHub Issues in repository

------------------------------------------------------------------------

**Document Version:** 1.0\
**Last Updated:** 2026-03-23\
**Maintainer:** Daniel R. Duncan
([d.duncan\@colostate.edu](mailto:d.duncan@colostate.edu){.email})
