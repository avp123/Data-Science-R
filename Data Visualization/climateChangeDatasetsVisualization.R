# Load in data/libraries
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Different ways to find latest year carbon emissions were reported in temp_carbon data frame
# First way
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

# Second way
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

#Third way
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

# Different ways to find earliest year carbon emissions were reported in temp_carbon
# First way
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

# Second way
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()

#Third way
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

# Finding carbon_emissions for earliest reported year (1751) and latest reported year (2014) and comparing them
carbon1 <- temp_carbon %>%
  filter(year == 1751) %>%
  .$carbon_emissions

carbon2 <- temp_carbon %>%
  filter(year == 2014) %>%
  .$carbon_emissions

carbon_increase <- carbon2/carbon1
carbon_increase

# Finding global temperature anomaly between the earliest recorded temperature anomaly and the most recently recorded temperature anomaly in temp_carbon
# Global temperature anomaly in 1880
temp1 <- temp_carbon %>%
  filter(year == "1880") %>%
  .$temp_anomaly

# Global temperature anomaly in 2018
temp2 <- temp_carbon %>%
  filter(year == "2018") %>%
  .$temp_anomaly

# Difference in global temperature anomaly
temp_increase <- temp2 - temp1
temp_increase

# Time series plot showing temperature anomaly through the years
temp_anomaly_plot <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()
temp_anomaly_plot

# Adding blue horizontal line showing mean temperature in 20th century
temp_anomaly_plot <- temp_anomaly_plot + geom_hline(aes(yintercept = 0), color = "blue")
temp_anomaly_plot

# Adding labels to our plot, axes, and horizontal line showing mean temperature anomaly
temp_anomaly_plot <- temp_anomaly_plot + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
temp_anomaly_plot

# Finding first year temperature anomaly exceeded 20th century mean of 0.0 degrees C
temp_carbon %>% 
  filter(temp_anomaly > 0) %>% 
  select(year) %>% 
  min()

# Finding last year temperature anomaly was below 20th century mean of 0.0 degrees C
temp_carbon %>%
  filter(temp_anomaly < 0) %>%
  select(year) %>%
  max()

# Finding first year temperature anomaly exceeded .5 degrees C
temp_carbon %>%
  filter(temp_anomaly > .5) %>%
  select(year) %>%
  min()

# Same as earlier time series plot called temp_anomaly_plot, but includes other columns from temp_carbon, specifically ocean_anomaly and land_anomaly. Plots them the same as temp_anomaly was plotted
land_ocean_temp_anomaly_plot <- temp_carbon %>% filter(!is.na(temp_anomaly) & !is.na(land_anomaly) & !is.na(ocean_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly on land and ocean")
land_ocean_temp_anomaly_plot

# Using greenhouse_gases data frame, created a line plot showing concentration of CH4, CO2, and N2O throughout the years
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") + 
  geom_vline(aes(xintercept = 1850), color = "blue") + 
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# Time series plot of carbon_emissions from temp_Carbon data frame
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  ylab("Carbon emissions (metric tons)") +
  ggtitle("Annual global carbon emissions, 1751-2014")

# Line plot of co2 concentration from Mauna Loa using historic_co2 data frame. 
co2_time <- historic_co2 %>%
  filter(!is.na(co2)) %>%
  ggplot(aes(year, co2, color = source)) +
  geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)")
co2_time

# Looking at different ranges of years and co2 concentrations by setting limits to the x-axis
# X axis from -800,000 to -775,000
co2_time +
  xlim(-800000, -775000)

# X axis from -375,000 to -330,000
co2_time +
  xlim(-375000, -330000)

# X axis from -140,000 to -120,000
co2_time +
  xlim(-140000, -120000)

# X axis from -3000 to 2018
co2_time +
  xlim(-3000, 2018)