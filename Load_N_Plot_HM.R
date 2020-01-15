# This R script  was written by Matthew C. Morriss on 1/14/20 to visualize the elevation of the Great Salt Lake
# as a heat map and visualize the flow  of the  Bear River, one of the major conduits of water supply to the GSL
# as a  heat map  as well

library(tidyverse)
library(readxl)
library(lubridate)
library(dataRetrieval)

# Load Data
BR <- read_xlsx('BR_cfs.xlsx')
data <- read.csv('GSL_data.csv')

data$Datetime <- parse_date_time(data$Datetime, orders = 'ymd')
data %>%
  drop_na() -> datan

# rename columns for data
colnames(datan) = c('Agency','Station  ID','Datetime','elevation  (ft)')

# Add day of year and year columns
datan %>%
  mutate(doy = yday(Datetime),
         yr = year(Datetime)) -> datan
BR %>%
  mutate(doy = yday(datetime),
         yr = year(datetime)) -> BR

# Write function to calculate the water year
wtr_yr <- function(dates, start_month=9) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

#Calculate the water day for each dataset
datan <- datan %>%
  mutate(water_yr = wtr_yr(datan$Datetime,start_month = 10))

datan <- datan %>%
  group_by(water_yr) %>%
  mutate(wtr_day = as.integer(difftime(Datetime,ymd(paste0(water_yr-1,'-09-30')),units = 'days')))

BR <- BR %>%
  mutate(water_yr = wtr_yr(BR$datetime,start_month = 10))

BR <- BR %>%
  group_by(water_yr) %>%
  mutate(wtr_day = as.integer(difftime(datetime,ymd(paste0(water_yr-1,'-09-30')),units = 'days')))

#################################### Heat Map of GSL ####################################

library(RColorBrewer)
datan$Datetime <- ymd(datan$Datetime)
hm.palette <- colorRampPalette((brewer.pal(11, 'Spectral')), space='Lab')

datan  %>%
  filter(.,yr > 1990) %>%
  ggplot(aes(x = wtr_day, y = water_yr, fill  = `elevation  (ft)`))+
  geom_tile()+
  # geom_density_2d()+
  scale_fill_gradientn(colors = hm.palette(100))+
  theme_light()+
  labs(x = "Day of Water Year",y = "Water Year", title = "Elevation of the GSL (ft)",
       fill = "Elevation (ft)")+
  scale_x_continuous(sec.axis = sec_axis(~. ))

#################################### Heat Map of Bear River ####################################

hm.palette <- colorRampPalette((brewer.pal(11, 'Spectral')), space='Lab')
my_breaks = c(2, 10, 50, 250, 1250, 6000)
B  <- BR %>%
  filter(.,yr > 1965) %>%
  ggplot(aes(x = wtr_day, y = water_yr, fill  = (discharge)))+
  geom_tile()+
  scale_fill_gradientn(colours = hm.palette(100),
                       trans = "log",
                       name = 'Discharge (cfs)',
                       breaks = my_breaks,
                       labels  = my_breaks)+
  theme_light()+
  labs(x = "Water Day",y = "Water Year", title = "Bear River Discharge",
       fill = "CFS")
B