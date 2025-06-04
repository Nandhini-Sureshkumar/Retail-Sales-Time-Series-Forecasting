# 🛍️ Retail Sales – Time Series Forecasting

This project performs time series analysis and forecasting on U.S. retail sales data using ARIMA, ETS, and other techniques.

## 📁 Structure

- `data/`: Contains retail sales and inflation datasets
- `scripts/`: R scripts for cleaning, modeling, and plotting
- `reports/`: RMarkdown and HTML reports

## 🔧 Methods

- STL Decomposition
- ETS
- ARIMA
- TSLM

## 📈 Output Report

Final forecasts are saved as HTML reports in `reports/`.

## 🔗 Data Source

Data retrieved from [FRED](https://fred.stlouisfed.org).

## 📄 Key Files and Scripts

### 🔹 Scripts (`/scripts/`)
- [`Retail_Sales_Time_series_forecasting.R`](scripts/Retail_Sales_Time_series_forecasting.R): Main R script for model training and forecasting
- [`Retail_Sales_Time_series_forecasting.Rmd`](scripts/Retail_Sales_Time_series_forecasting.Rmd): R Markdown version with narrative + plots
- [`Retail_Sales_Time_series_forecasting - Log Transformed.Rmd`](scripts/Retail_Sales_Time_series_forecasting%20-%20Log%20Transformed.Rmd): Log-transformed version for variance stabilization
- [`Retail_Sales_Time_series_forecasting_Outlook_Report.Rmd`](scripts/Retail_Sales_Time_series_forecasting_Outlook_Report.Rmd): Outlook-friendly RMD for sharing results

### 📁 Other Folders
- `data/`: Contains input data files for Google Trend
- `reports/`: Rendered `.html` outputs and visual reports
