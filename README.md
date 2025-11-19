# NOAA CO-OPS Tide Prediction Shiny App

Shiny application for European Green Crab (EGC) field planning purposes. Application provides tide prediction information from NOAA CO-OPS in a streamlined visual format, along with coastal inundation information.

## Features

### Tab 1: Tide Predictions
- Interactive line plot showing tide predictions for a user-specified time period (default: 7 days)
- Annotated low tide markers showing time and water level for each low tide
- Summary table of all low tides with date, time, and water level

### Tab 2: Threshold Crossings
- User-definable low and high tide thresholds
- Table showing all times when tides cross below the low threshold
- Table showing all times when tides cross above the high threshold
- Sortable and searchable table for easy planning

### Tab 3: Coastal Inundation
- Links to NOAA Coastal Inundation Dashboard
- Embedded NOAA inundation map for the selected station
- Links to additional NOAA resources (Sea Level Rise Viewer, station information)

## Installation

### Prerequisites
- R (>= 4.0.0)
- Required R packages (will be installed automatically when running the app)

### Required R Packages
```r
install.packages(c("shiny", "httr", "jsonlite", "ggplot2", "dplyr", "lubridate", "DT"))
```

## Usage

### Running the Application

1. Clone this repository:
```bash
git clone https://github.com/danny-duncan/nwsc-planning-app.git
cd nwsc-planning-app
```

2. Open R or RStudio and run:
```r
shiny::runApp()
```

Or from the command line:
```bash
Rscript -e "shiny::runApp('app.R')"
```

### Using the Application

1. **Select a Station**: Enter a NOAA station ID (e.g., 9414290 for San Francisco)
   - Find station IDs at: https://tidesandcurrents.noaa.gov/
   
2. **Set Date Range**: Choose a start date and number of days (1-31)

3. **Select Datum**: Choose the vertical datum reference (default: MLLW - Mean Lower Low Water)

4. **Fetch Data**: Click "Fetch Tide Data" to retrieve predictions from NOAA

5. **View Results**:
   - **Tide Predictions Tab**: View the tide prediction graph with low tide annotations
   - **Threshold Crossings Tab**: Set custom thresholds and view crossing times
   - **Coastal Inundation Tab**: Access NOAA inundation forecasts and maps

## Example Station IDs

- **9414290**: San Francisco, CA
- **8454000**: Providence, RI
- **8518750**: The Battery, New York, NY
- **9447130**: Seattle, WA
- **8729108**: Panama City, FL

For a complete list of stations, visit: https://tidesandcurrents.noaa.gov/map/

## API Reference

This application uses the NOAA CO-OPS API:
- **API Documentation**: https://api.tidesandcurrents.noaa.gov/api/prod/
- **Product**: Tide Predictions (predictions)
- **Units**: English (feet)
- **Time Zone**: Local Standard Time/Local Daylight Time (LST/LDT)

## Data Explanations

### Datums
- **MLLW** (Mean Lower Low Water): Average of lower low water heights
- **MLW** (Mean Low Water): Average of all low water heights
- **MTL** (Mean Tide Level): Average of mean high water and mean low water
- **MSL** (Mean Sea Level): Average of all hourly water levels
- **MHW** (Mean High Water): Average of all high water heights
- **MHHW** (Mean Higher High Water): Average of higher high water heights

### Tide Types
- **H**: High tide
- **L**: Low tide

## Field Planning Tips

1. **Low Tide Access**: Use the low tide annotations to plan field work during optimal water levels
2. **Threshold Planning**: Set thresholds based on your access requirements and view when conditions are suitable
3. **Multi-Day Planning**: View up to 31 days of predictions to plan multiple field trips
4. **Inundation Awareness**: Check coastal inundation forecasts before field work for safety

## Troubleshooting

### "Failed to fetch tide data"
- Verify the station ID is correct
- Check your internet connection
- Ensure the date range is not too far in the past or future
- NOAA API may be temporarily unavailable

### Plot not showing
- Ensure data has been fetched successfully
- Try a different station ID
- Check that the date range is valid

## License

MIT License

## Acknowledgments

- NOAA CO-OPS for providing the tide prediction API
- European Green Crab field planning team
