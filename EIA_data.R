rm(list=ls())
setwd("D:/Energia")
library(readr)
library(jsonlite)
j <- fromJSON("SeriesExport-02-10-2026-16-28-43.json", flatten = TRUE)
View(j)
names(j)
str(j, max.level = 2)
j$data[[1]][1]

## date da millisecondi a normali date con UTC 
for (i in seq_along(j$data)) {
  j$data[[i]][,1] <- as.POSIXct(
    j$data[[i]][,1] / 1000,
    origin = "1970-01-01",
    tz = "UTC"
  )
}
unique(substr(j$series_id,1,7))
##6 serie: giusto

library(dplyr)
library(stringr)
library(purrr)

# variabile
target_vars <- c("INTL.53", 
                 "INTL.55", 
                 "INTL.56", 
                 "INTL.57", 
                 "INTL.58", 
                 "INTL.59")

# Identificatore delle serie che appartengono a queste variabili
idx <- substr(j$series_id, 1, 7) %in% target_vars

# panel
panel_j <- map_dfr(
  which(idx),
  function(i) {
    # estrazione Paese country (fra i due hyphens/trattini)
    country <- str_extract(j$series_id[i], "(?<=-)[A-Z]{3}(?=-)")
###(?<=-) == prendi solo quello che viene dopo il trattino
### [A-Z]{3} prendi solo tre lettere maiuscole
### (?=-) prendi solo quello che è seguito dal trattino, ovvero fermati
### prima dell'ultimo trattino escludendolo
    # descrizione fino alla prima virgola
    description <- str_split(j$name[i], ",")[[1]][1]
### la divisione è in tre pezzetti perché sono due le virgole  
    # estrazione date e valori
    dates <- j$data[[i]][,1]
    values <- j$data[[i]][,2]
    
    # costruzione data frame per questa serie
    data.frame(
      variable = substr(j$series_id[i], 1, 7),
      country = country,
      description = description,
      date = dates,
      value = values
    )
  }
)
panel_j$month_year <- format(panel_j$date, "%b%Y")
full_seq <- seq(
  from = as.Date("1973-01-01"),
  to   = as.Date("2025-10-01"),
  by   = "month"
)
full_seq_fmt <- format(full_seq, "%b%Y")
library(dplyr)
library(tidyr)

panel_j_expanded <- panel_j %>%
  mutate(month_year = format(date, "%b%Y")) %>%
  select(variable, country, description, month_year, value) %>%
  
  # Espansione delle date
  complete(
    variable,
    country,
    month_year = full_seq_fmt,
    fill = list(value = NA)
  ) %>%
  
  # Riempimento di description per gruppo
  group_by(variable, country) %>%
  fill(description, .direction = "downup") %>%
  ungroup() %>%
  
  arrange(variable, country, month_year)

panel_j_expanded$month_year <- factor(
  panel_j_expanded$month_year,
  levels = full_seq_fmt,
  ordered = TRUE
)
panel_j_expanded$date_real <- as.Date(
  paste0("01", panel_j_expanded$month_year),
  format = "%d%b%Y"
)
library(zoo)
panel_j_expanded$month_year <- as.yearmon(panel_j_expanded$date_real)
format(panel_j_expanded$month_year, "%Ym%m")

panel_j_expanded <- panel_j_expanded %>%
  arrange(variable, country, date_real)
panel_j_expanded <- panel_j_expanded %>%
  select(country, month_year, date_real, variable, description, value)
write.csv(panel_j_expanded,"clean_US_EIA.csv")



