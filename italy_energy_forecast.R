# Italy Energy Consumption Forecasting in R

cat("Starting Italy Energy Analysis (R)...\n\n")

# Install packages if needed
required_packages <- c("tidyverse", "forecast")
new_packages <- required_packages[!(required_packages %in% rownames(installed.packages()))]
if(length(new_packages)) {
    install.packages(new_packages, quiet = TRUE)
}

library(tidyverse)
library(forecast)

# Download data
cat("Loading energy data...\n")
url <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
df <- read.csv(url)

# Filter for Italy
italy <- df %>% 
    filter(country == "Italy") %>% 
    arrange(year)

cat(sprintf("Data loaded: %d rows, %d columns\n\n", nrow(italy), ncol(italy)))

# Use oil consumption
italy_clean <- italy %>% 
    select(year, oil_consumption) %>% 
    drop_na() %>% 
    arrange(year)

cat(sprintf("Using column: oil_consumption\n"))
cat(sprintf("Years: %d to %d\n", min(italy_clean$year), max(italy_clean$year)))
cat(sprintf("Data points: %d\n\n", nrow(italy_clean)))

# Summary statistics
values <- italy_clean$oil_consumption
cat("=== HISTORICAL STATISTICS ===\n")
cat(sprintf("Mean: %.2f TWh\n", mean(values, na.rm=TRUE)))
cat(sprintf("Std Dev: %.2f TWh\n", sd(values, na.rm=TRUE)))
cat(sprintf("Min: %.2f TWh\n", min(values, na.rm=TRUE)))
cat(sprintf("Max: %.2f TWh\n\n", max(values, na.rm=TRUE)))

# Linear regression
x <- 0:(length(values)-1)
model <- lm(values ~ x)
slope <- coef(model)[2]
intercept <- coef(model)[1]

cat("=== LINEAR REGRESSION MODEL ===\n")
cat(sprintf("Slope: %.6f TWh/year\n", slope))
cat(sprintf("Intercept: %.2f TWh\n", intercept))
cat(sprintf("Trend: %s\n\n", if(slope < 0) "DECREASING" else "INCREASING"))

# Generate 10-year forecast
future_x <- length(values):(length(values) + 9)
forecast_values <- intercept + slope * future_x
future_years <- (max(italy_clean$year) + 1):(max(italy_clean$year) + 10)

cat("=== 10-YEAR FORECAST FOR ITALY (OIL CONSUMPTION) ===\n")
cat("Year | Forecast (TWh)\n")
for(i in 1:10) {
    cat(sprintf("%d  | %.2f\n", future_years[i], forecast_values[i]))
}

# Create visualization
png("italy_energy_forecast_R.png", width=1200, height=600, res=100)

plot(italy_clean$year, values, 
     type="o", pch=16, cex=1.2, lwd=2.5,
     col="#1f77b4",
     xlab="Year", ylab="Oil Consumption (TWh)",
     main="Italy Oil Consumption: Historical & 10-Year Forecast (2026-2035)",
     cex.lab=1.2, cex.main=1.3, font.main=2,
     xlim=c(min(italy_clean$year)-2, max(future_years)+1),
     ylim=c(min(c(values, forecast_values))-1, max(c(values, forecast_values))+1))

# Add forecast line
lines(future_years, forecast_values, type="o", pch=15, lwd=2.5, col="#d62728", lty=2)

# Add separation line
abline(v=max(italy_clean$year) + 0.5, lty=":", col="gray", lwd=2, alpha=0.4)

# Add legend
legend("topright", 
       c("Historical Data", "10-Year Forecast"),
       col=c("#1f77b4", "#d62728"),
       pch=c(16, 15),
       lty=c(1, 2),
       cex=1.1)

# Add grid
grid(NA, NA, col="gray", lty="--", alpha=0.3)

dev.off()

cat("\n✓ Plot saved: italy_energy_forecast_R.png\n")
cat("\nAnalysis complete!\n")
