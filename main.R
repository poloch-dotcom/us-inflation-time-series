
library(tidyverse)
library(readr)
library(tseries)
library(forecast)

df1 <- read_csv("CPIAUCSL.csv", col_types = cols(observation_date = col_date(format = "%Y-%m-%d"), 
                                                         CPIAUCSL = col_number()))

df1$CPIAUCSL <- na.interp(df1$CPIAUCSL)
serie_prix <- ts(df1$CPIAUCSL, start = c(1947, 1), frequency = 12)

log_prix <- log(serie_prix) 

plot(log_prix, main = "Log Price Index", col = "blue", ylab = "Log(Prix)")


summary(log_prix)
sd(log_prix) 

## The log_prix series shows a consistent, non-linear upward trend from the late 1940s through 2020, with a particularly sharp increase between 1970 and 1985. Statistically, the mean of 4.486 and median of 4.698 are quite close, suggesting the data is relatively well-centered within its range of 3.067 to 5.789. With a standard deviation of 0.879 and a clear time-dependent mean, the plot strongly suggests the series is non-stationary and will likely need to be differenced before conducting further econometric analysis.

par(mfrow = c(1, 2)) 
Acf(log_prix, main = "ACF Log-Prix")
Pacf(log_prix, main = "PACF Log-Prix")
## The ACF shows a very slow linear decay while the PACF has a single dominant spike at the first lag, both of which are classic indicators of a non-stationary series. These results strongly suggest the presence of a unit root, meaning the data should be differenced before proceeding with any ARMA modeling.

diff_log_prix <- diff(log_prix)


plot(diff_log_prix, main = "First Difference (Log-Prix)", col = "red")


summary(diff_log_prix)
sd(diff_log_prix, na.rm = TRUE)
## After first-differencing, the series now oscillates around a constant mean of 0.0028, effectively removing the previous upward trend. The plot shows a much more stationary pattern, although the spikes in the early years and around 2008 suggest some periods of higher volatility. This transformation likely makes the data suitable for ARMA modeling.

par(mfrow = c(1, 2))
Acf(diff_log_prix, main = "ACF First Diff")
Pacf(diff_log_prix, main = "PACF First Diff")

## The ACF and PACF plots for the differenced series show a significant drop-off compared to the original data, though many lags still exceed the confidence intervals. The ACF shows a slow decay while the PACF has a large spike at lag 1 followed by another significant spike at lag 12. This suggests that while first-differencing helped, there is still strong autocorrelation and a clear seasonal.



df2 <- read_csv("FPCPITOTLZGUSA.csv")


df2$FPCPITOTLZGUSA <- na.interp(df2$FPCPITOTLZGUSA)
serie_inflation <- ts(df2$FPCPITOTLZGUSA, start = c(1960, 1), frequency = 12)



plot(serie_inflation, main = "Inflation", col = "blue", ylab = "Inflation")


summary(serie_inflation)
sd(serie_inflation) 
## The inflation series shows high volatility and several distinct peaks between 1960 and 1965, with values ranging from -0.35 to a maximum of 13.55. The mean of 3.75 and standard deviation of 2.74 highlight significant fluctuations rather than a steady trend. While the series appears to return toward a central level, the irregular spikes suggest it may still be non-stationary or subject to structural shifts during this period.

par(mfrow = c(1, 2)) 
Acf(serie_inflation, main = "ACF Inflation")
Pacf(serie_inflation, main = "PACF Inflation")
## The ACF for inflation shows an exponential decay with some alternating values, while the PACF has two significant spikes at the first few lags. This combination suggests that the series is likely stationary and could be modeled using an AR(p) process, probably an AR(2). There are no strong seasonal spikes at lag 12 or 24, indicating that a standard ARMA approach should be sufficient for this specific timeframe.

diff_serie_inflation <- diff(serie_inflation)


plot(diff_serie_inflation, main = "First Difference", col = "red")


summary(diff_serie_inflation)
sd(diff_serie_inflation, na.rm = TRUE)

## The first-differenced inflation series is now clearly stationary, centered around a mean of 0.023 with a reduced standard deviation of 1.79. The plot shows the series oscillating rapidly around zero, which effectively removes the earlier level shifts and provides a stable foundation for ARMA modeling.

par(mfrow = c(1, 2))
Acf(diff_serie_inflation, main = "ACF First Diff")
Pacf(diff_serie_inflation, main = "PACF First Diff")

## The ACF and PACF plots for the first-differenced inflation series show that most autocorrelations now fall within the confidence intervals. There is a single significant negative spike at lag 2 in both plots. Overall, the transformation has successfully achieved stationarity.





df3 <- read_csv("GDPC1.csv")

df3$GDPC1 <- na.interp(df3$GDPC1)
serie_gdp <- ts(df3$GDPC1, start = c(1947, 1), frequency = 12)

log_gdp <- log(serie_gdp) 

plot(log_gdp, main = "Log GDP", col = "blue", ylab = "Log(GDP)")


summary(log_gdp)
sd(log_gdp) 
## The log_gdp series shows a very strong and steady upward linear trend from 1950 through the early 1970s. With a mean of 9.02 and a standard deviation of 0.70, the data moves predictably from a minimum of 7.68 to a maximum of 10.09. This clear time-dependency and lack of mean reversion confirm that the series is non-stationary, likely containing a unit root that will require first-differencing to stabilize for modeling.

par(mfrow = c(1, 2)) 
Acf(log_gdp, main = "ACF Log-GDP")
Pacf(log_gdp, main = "PACF Log-GDP")
## The ACF for log-GDP exhibits a very slow, linear decay with all shown lags remaining significantly above the confidence threshold. Coupled with a PACF that shows a single dominant spike at lag 1 and virtually nothing thereafter, this is a textbook signature of a non-stationary series containing a unit root. Differencing the data is necessary to stabilize the mean before any further analysis.

diff_log_gdp <- diff(log_gdp)


plot(diff_log_gdp, main = "First Difference (Log-GDP)", col = "red")


summary(diff_log_gdp)
sd(diff_log_gdp, na.rm = TRUE)


## First-differencing has successfully removed the trend, leaving a stationary series that oscillates around a constant mean of 0.0076. While the data is generally stable, there is a very sharp, significant spike in volatility around 1971. This transformation represents the growth rate of GDP and is now much better suited for time series modeling, though that outlier may require specific attention.
par(mfrow = c(1, 2))
Acf(diff_log_gdp, main = "ACF First Diff")
Pacf(diff_log_gdp, main = "PACF First Diff")
## The ACF and PACF plots for the differenced GDP series show that most autocorrelations now fall within the confidence intervals. There are significant spikes at lag 1 in both plots, which suggests that a first-order autoregressive (AR(1)) or moving average (MA(1)) component might be present. Since the autocorrelations at higher lags are mostly negligible, the series is now considered stationary and ready for ARMA model selection.



df4 <- read_csv("FEDFUNDS.csv")


df4$FEDFUNDS <- na.interp(df4$FEDFUNDS)
serie_interest <- ts(df4$FEDFUNDS, start = c(1954, 7), frequency = 12)


plot(serie_interest, main = "Interest", col = "blue", ylab = "Interest")


summary(serie_interest)
sd(serie_interest) 
## The serie_interest plot shows significant long-term fluctuations, peaking sharply around 1980 before trending downward into a low-interest environment by the 2010s. With a wide range from 0.05 to 19.10 and a standard deviation of 3.54, the series displays high volatility and a non-constant mean. This lack of mean reversion over the decades suggests the series is non-stationary, likely requiring differencing to stabilize it for statistical modeling.

par(mfrow = c(1, 2)) 
Acf(serie_interest, main = "ACF Interest")
Pacf(serie_interest, main = "PACF Interest")
## The ACF for interest rates decays very slowly, while the PACF shows a sharp initial spike followed by a significant negative spike at lag 2. This pattern is characteristic of a non-stationary series that is highly persistent. The slow decline in the ACF strongly suggests the presence of a unit root, confirming that the interest rate data needs to be differenced to reach stationarity before further modeling.

diff_serie_interest <- diff(serie_interest)


plot(diff_serie_interest, main = "First Difference", col = "red")


summary(diff_serie_interest)
sd(diff_serie_interest, na.rm = TRUE)
## First-differencing has stabilized the interest rate series, which now oscillates around a mean of 0.003. The plot shows a clear return to zero, indicating that the non-stationary trend has been removed. However, there is a massive spike in volatility around 1980 and 1981, where the difference reached a minimum of -6.63. Despite these historical shocks, the series is now statistically stationary and ready for ARMA modeling.
par(mfrow = c(1, 2))
Acf(diff_serie_interest, main = "ACF First Diff")
Pacf(diff_serie_interest, main = "PACF First Diff")

## The ACF and PACF for the differenced interest rates show several significant spikes, particularly at the first lag and around lag 9. The significant spike at lag 1 in both plots suggests a first-order AR or MA process is present. While the series is now stationary, the remaining autocorrelations at later lags indicate that a simple ARMA model may need a few more parameters or seasonal adjustments to fully capture the dynamics of the data.




df5 <- read_delim("consommation-quotidienne-brute.csv", 
                  delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                  trim_ws = TRUE)

df5 <- df5 %>%
  group_by(Date) %>%
  filter(!any(is.na(`Consommation brute électricité (MW) - RTE`))) %>%

  summarise(Consommation_Journaliere = sum(`Consommation brute électricité (MW) - RTE`)
  )

df5$log_Consommation_Journaliere <- log(df5$Consommation_Journaliere)
serie_log_consumption <- ts(df5$log_Consommation_Journaliere, start = c(2012, 1, 1), frequency = 365.25)


plot(serie_log_consumption, main = "Log Electricity Consumption (MW)", col = "blue", ylab = "Log MW")


summary(serie_log_consumption)
sd(serie_log_consumption) 
## The log electricity consumption series exhibits a very clear and strong seasonal pattern between 2012 and 2024. With a tight range from 14.34 to 15.32 and a low standard deviation of 0.195, the data appears remarkably stable in its fluctuations. While there is no obvious long-term upward or downward trend, the repetitive seasonal spikes suggest that the series is non-stationary in its mean due to these periodic shifts.

par(mfrow = c(1, 2)) 
Acf(serie_log_consumption, main = "ACF Interest")
Pacf(serie_log_consumption, main = "PACF Interest")
## The ACF plot displays a very strong, repeating sinusoidal pattern with peaks at lag 365.25, which is a definitive indicator of annual seasonality in daily data. The PACF shows significant initial spikes that quickly taper off, though some seasonal echoes remain. This structure confirms that while the series is highly predictable on a seasonal basis, it is non-stationary and will require seasonal differencing (lag 365) to stabilize the mean.

diff_serie_log_consumption <- diff(serie_log_consumption)


plot(diff_serie_log_consumption, main = "First Difference", col = "red")


summary(diff_serie_log_consumption)
sd(diff_serie_log_consumption, na.rm = TRUE)
## The first-differenced log consumption series is now centered around a mean of zero, effectively removing any deterministic trend.
par(mfrow = c(1, 2))
Acf(diff_serie_log_consumption, main = "ACF First Diff")
Pacf(diff_serie_log_consumption, main = "PACF First Diff")
## The ACF and PACF plots for the first-differenced series confirm that seasonality remains the dominant feature. The ACF shows persistent, high-amplitude spikes that repeat at the 365-day interval, indicating that a simple first difference was insufficient to handle the annual cycle. The PACF also displays a large spike at the first seasonal lag, suggesting that the series is still non-stationary in its seasonal component and requires a seasonal difference to be properly stabilized.

#Question2 

modele_best1 <- ar.ols(log_prix, aic = TRUE, order.max = 20, demean = TRUE)
print(modele_best1)

modele_best2 <- ar.ols(serie_inflation, aic = TRUE, order.max = 20, demean = TRUE)
print(modele_best2)

modele_best3 <- ar.ols(log_gdp, aic = TRUE, order.max = 20, demean = TRUE)
print(modele_best3)

modele_best4 <- ar.ols(serie_interest, aic = TRUE, order.max = 20, demean = TRUE)
print(modele_best4)

modele_best5 <- ar.ols(serie_log_consumption, aic = TRUE, order.max = 20, demean = TRUE)
print(modele_best5)

## The results from the AR(OLS) models show that the AIC is selecting the maximum limit of 20 lags for the Log Price Index, Inflation, and Log Consumption series. This generally suggests these variables have a very "long memory" or that the models are still trying to account for the strong seasonal and non-stationary patterns we saw in the initial plots. The Log GDP is the most straightforward, settling on a much simpler AR(3) model, while Interest Rates fall in between with 17 lags. Overall, the high order selections across most of these series indicate they are quite persistent and might benefit from further differencing or seasonal adjustments to simplify the model structure.

#Question3 



forecast_1_step <- predict(modele_best1, n.ahead = 1)
forecast_3_step <- predict(modele_best1, n.ahead = 3)

print(forecast_1_step$pred)

print(forecast_3_step$pred)

## The model predicts a continued, steady increase in the price index. The 3-step forecast moves from 5.790 to 5.795, showing very low volatility in the short-term projections. This reflects the high persistence and strong trend captured by the 20-lag AR model.


forecast_1_step <- predict(modele_best2, n.ahead = 1)
forecast_3_step <- predict(modele_best2, n.ahead = 3)

print(forecast_1_step$pred)


print(forecast_3_step$pred)

## In contrast to the price index, the inflation forecast shows significant movement. It starts at 1.93 for June 1965 but drops sharply to 1.09 in July before rebounding to 1.60. This "zigzag" behavior is typical of an AR(20) model trying to capture the high historical volatility of the inflation series.

forecast_1_step <- predict(modele_best3, n.ahead = 1)
forecast_3_step <- predict(modele_best3, n.ahead = 3)


print(forecast_1_step$pred)


print(forecast_3_step$pred)

## The forecast for log_gdp using the AR(3) model shows a steady linear growth path for the mid-1973 period. The index is projected to move from 10.096 in May to 10.106 by July, maintaining the strong upward momentum observed in the historical data. Because this model uses a low order (3 lags), the forecast isn't distracted by short-term noise and instead provides a smooth extension of the primary economic trend.

forecast_1_step <- predict(modele_best4, n.ahead = 1)
forecast_3_step <- predict(modele_best4, n.ahead = 3)

print(forecast_1_step$pred)


print(forecast_3_step$pred)

## The model predicts a slight upward drift for interest rates in early 2026, rising from 3.65 to 3.69. The 17-lag structure allows for a more nuanced adjustment than a simple trend, but the overall change remains relatively modest in the 3-step horizon.

forecast_1_step <- predict(modele_best5, n.ahead = 1)
forecast_3_step <- predict(modele_best5, n.ahead = 3)


print(forecast_1_step$pred)


print(forecast_3_step$pred)

## For electricity consumption, the 3-step forecast shows a slight decline from 14.99 to 14.94. Given the daily frequency and high seasonality of this data, these small shifts likely reflect the model's attempt to follow the specific seasonal cycle identified by the 20-lag selection.