# Load in data/libraries and set sig figs to 3
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

# Mean and Standard Deviation of magnitude in dataset
mean(stars$magnitude)
sd(stars$magnitude)

# Density plot of star magnitude
stars %>% 
  ggplot(aes(magnitude)) + 
  geom_density(fill = "sky blue")

# Density plot of star temperature
stars %>% 
  ggplot(aes(temp)) + 
  geom_density(fill = "sky blue")

# Scatter plot of star temperature vs star magnitude
stars %>% 
  ggplot(aes(temp, magnitude)) + 
  geom_point()

# Same scatter plot shown above, but changed x-axis scale to log10 and reversed both axes
stars %>% 
  ggplot(aes(log10(temp), magnitude)) + 
  geom_point() + 
  scale_x_reverse() + 
  scale_y_reverse()

# Same scatter plot shown above, but also plotted the names of every star
stars %>% 
  ggplot(aes(log10(temp), magnitude)) + 
  geom_point() + 
  geom_text(aes(label = star)) + 
  scale_x_reverse() + 
  scale_y_reverse()

# Same scatterplot shown above, but star names were removed and color was added based on the type of star
stars %>% 
  ggplot(aes(log10(temp), magnitude, color = type)) + 
  geom_point() + 
  scale_x_reverse() + 
  scale_y_reverse()