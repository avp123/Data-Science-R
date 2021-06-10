# Load in data/libraries
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# Different ways to find latest year carbon emissions were reported in temp_carbon
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