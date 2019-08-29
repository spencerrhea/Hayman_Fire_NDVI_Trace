library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

#Reading in the data. 
ndvi <- read_csv('data/hayman_ndvi.csv') %>%
  rename(burned=2,unburned=3) %>%
  filter(!is.na(burned),
         !is.na(unburned))

# Converting from wide to long data
ndvi_long <- gather(ndvi,key='site',value='NDVI',-DateTime)

# Plotting all the data
ggplot(ndvi_long,aes(x=DateTime,y=NDVI,color=site)) +
  geom_point(shape=1) + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.3,0.3))

# Summarizing the data by year
ndvi_annual <- ndvi_long %>%
  mutate(year=year(DateTime)) %>%
  mutate(month=month(DateTime)) %>%
  filter(month %in% c(5,6,7,8,9)) %>%
  group_by(site,year) %>%
  summarize(mean_NDVI=mean(NDVI))

#Here making an annual plot
ggplot(ndvi_annual,aes(x=year,y=mean_NDVI,color=site)) +
  geom_point(shape=1) + 
  geom_line() +
  theme_few() + 
  scale_color_few() + 
  theme(legend.position=c(0.3,0.3))




