# NOAA CO-OPS Tide Prediction Shiny App

## Overview

Shiny application for European Green Crab (EGC) field planning purposes. Application provides tide prediction information from NOAA CO-OPS in a streamlined visual format. 

## Features

### Sidebar: User Defined Parameters
- NOAA CO-OPS Station ID input (Default: Drayton Harbor, WA)
- Date range for data retrieval (Default: Monday-Friday of week + 1)
- Vertical Datum Reference selector (Default: MLLW)
- Water level threshold input (Default: 3 ft)

### Tab 1: Tide Prediction Plot
- Interactive plot for threshold tide analysis
- Annotated low-low tide markers
- Annotated threshold-crossing markers

### Tab 2: Tide Prediction Tables
- Table showing all times when tides crosses water level threshold
- Table of all low-low tides with date, time, and water level

## Usage

### Using the Application

1. **Select a Station**: Enter a NOAA station ID (e.g., 9449679 for Drayton Harbor, WA)
   - Find station IDs at: https://tidesandcurrents.noaa.gov/
   
2. **Set Date Range**: Choose a start date and end date

3. **Select Datum**: Choose the vertical datum reference (default: MLLW - Mean Lower Low Water)

4. **Fetch Data**: Click "Fetch Tide Data" to retrieve predictions from NOAA

5. **View Results**:
   - **Tide Predictions Tab**: View the tide prediction graph with low-low tide annotations
   - **Threshold Crossings Tab**: Set custom thresholds and view trapping window times

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

## Acknowledgments

- NOAA CO-OPS for providing the tide prediction API
