# US inflation: unit roots, AR models and forecasting

Time series homework for the M1 *Time Series* course at the Toulouse School of Economics (March 2026, R).

## What it does

- Loads the monthly US Consumer Price Index (FRED series `CPIAUCSL`, 1947 onwards) and annual US inflation (FRED `FPCPITOTLZGUSA`).
- Studies the log price level: trend, ACF/PACF, unit root, then first differences to reach stationarity.
- Selects the order of an AR(p) model by AIC and compares candidate models in a summary table.
- Writes a small forecasting function and produces out-of-sample forecasts.

Each step comes with a short written interpretation in the code comments and in the report.

## Data

Both series are public on [FRED](https://fred.stlouisfed.org/). Download `CPIAUCSL.csv` and `FPCPITOTLZGUSA.csv` and place them next to the scripts.

## Files

- `main.R`: the full analysis.
- `report.Rmd`: the written report (R Markdown).

**Stack:** R, tidyverse, tseries, forecast.
