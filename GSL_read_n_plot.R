#################################### SECTION TITLE ####################################
library(tidyverse)
library(readxl)
library(lubridate)
library(dataRetrieval)
library(gridExtra)
library(tsibble)
rm(list = ls())
setwd('/Users/matthew/Documents/GitHub/GSL')

data <- read.csv('GSL_data.csv')
JRMain <- read_xlsx('JR_main.xlsx')
JRdiv <- read_xlsx('JR_surplus.xlsx')
BR <- read_xlsx('BR_cfs.xlsx')
# data19th = as.data.frame(read_xlsx('1800_data.xlsx'))
# data20th = as.data.frame(read_xlsx('1900_data.xlsx'))

data$Datetime <- parse_date_time(data$Datetime, orders = 'ymd')
data %>%
  drop_na() -> datan


colnames(datan) = c('Agency','Station  ID','Datetime','elevation  (ft)')

datan %>%
  mutate(doy = yday(Datetime),
         yr = year(Datetime)) -> datan
BR %>%
  mutate(doy = yday(datetime),
         yr = year(datetime)) -> BR

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
  
# #################################### Tidy Data ####################################
# data19th$Datetime <- ymd(data19th$Datetime)
# data20th$Datetime <- ymd(data20th$Datetime)
# 
# data <- rbind(data19th,data20th)
# 
# write_csv(data,'GSL_data.csv')
#################################### SECTION TITLE ####################################
# data$%Datetime <- ymd(data$Datetime)

#################################### Plot ####################################
datan %>%
  # filter(.,Datetime < )  %>%
  ggplot(aes(x = Datetime,
             y = `elevation  (ft)`,
             group = 1))+
  geom_line()+
  theme_light()+
  labs(x = "year", y = 'Lake Surface Elevation (m)', title = 'GSL Elevation')

#################################### SECTION TITLE ####################################

baseline <- datan %>%
              filter(Datetime < as.Date('1947-01-01')) %>%
              summarize(centMean = mean(`elevation  (ft)`,na.rm = TRUE))

adata <- datan %>%
  mutate(anomaly = `elevation  (ft)` -  baseline[[1]])

adata  %>%
  ggplot(aes(x = Datetime,  y = anomaly))+
  geom_line()+
  geom_smooth()+
  labs(x = "Date",
       y = "Lake Level Elevation Anomaly")+
  theme_light()+
  geom_hline(yintercept=0,color='dodgerblue',linetype=2,size=2)

#################################### Plots of input rivers ####################################
# rm(p1,p2,p3)

JRMain %>%
  filter(datetime > "1960-01-01") %>%
ggplot(aes(x = (datetime),
             y = discharge))+ 
  geom_line()+
  theme_light()+
  geom_smooth()+
  labs(x = 'Year',
       y = 'Discharge (cfs)',
       title = "Main  Jordan  River") -> p1

JRdiv %>%
  filter(datetime >"1960-01-01") %>%
ggplot( aes(x = datetime,
                        y = discharge))+
  geom_line()+
  theme_light()+
  geom_smooth()+
  labs(x = 'Year',
       y = 'Discharge (cfs)',
       title = "Jordan  River Diversion") -> p2
  # xlim(1960,2020)

BR %>%
  filter(datetime > "1960-01-01") %>%
ggplot(aes (x = datetime,
                     y = discharge))+
      geom_line()+
      theme_light()+
      geom_smooth()+
      labs(x = 'Year',
           y = 'Discharge (cfs)',
           title = "Bear River") -> p3

datan %>%
  filter(Datetime > "1960-01-01") %>%
  ggplot(aes(x = Datetime,
             y = `elevation  (ft)`))+
  geom_line()+
  theme_light()+
  # geom_smooth()+
  labs(x = "Year",
       y = "Lake Elevation (ft)",
       title = "GSL Elevation") -> p4


grid.arrange(p1,p2,p3,p4,ncol = 1)



#################################### Move data to Tsibble ####################################
JRMain$datetime <- ymd(JRMain$datetime)
JRMainT <- as_tsibble(JRMain,index = datetime)

JRMain_monthly <- JRMainT %>%
  index_by(Month = ~month(.)) %>%
  summarise(
    meanDischarge = mean(discharge,na.rm = TRUE),
    maxDischarge = mean(discharge,na.rm = TRUE)  + sd(discharge,  na.rm = TRUE),
    minDischarge = mean(discharge,na.rm = TRUE) - sd(discharge, na.rm = TRUE),
    max = max(discharge,na.rm = TRUE),
    min = min(discharge,na.rm = TRUE)
  )


months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  ggplot(JRMain_monthly,aes(x = Month,
             y = meanDischarge))+
  geom_pointrange(data = JRMain_monthly,
                  aes(x = Month,
                      ymin = minDischarge,
                      ymax = maxDischarge
                      ))+
    # geom_line(data = JRMain_monthly,
    #           aes(x = Month,
    #               y = max))+
    # geom_line(data = JRMain_monthly,
    #           aes(x = Month,
    #               y = min))+
  scale_x_continuous('Month',breaks=1:12,labels=months)+
  theme_light()
#################################### GSL as a Tissible ####################################
  datan$Datetime <- ymd(datan$Datetime)
  GSLT <- as_tsibble(datan,index = Datetime)
  GSLE  <- filter(GSLT, Datetime <= "1934-01-01")
  GSLL <- filter(GSLT,Datetime > "1934-01-01")
  
  GSLE_monthly <- GSLE %>%
      index_by(Month = ~month(.)) %>%
      summarise(meanElev = mean(`elevation  (ft)`,na.rm = TRUE),
                maxElev  = mean(`elevation  (ft)`, na.rm = TRUE) + sd(`elevation  (ft)`, na.rm = TRUE),
                minElev = mean(`elevation  (ft)`, na.rm = TRUE) - sd(`elevation  (ft)`, na.rm = TRUE))
  
  GSLL_monthly <- GSLL %>%
      index_by(Month = ~month(.)) %>%
      summarise(meanElev = mean(`elevation  (ft)`,na.rm = TRUE),
                maxElev  = mean(`elevation  (ft)`, na.rm = TRUE) + sd(`elevation  (ft)`, na.rm = TRUE),
                minElev = mean(`elevation  (ft)`, na.rm = TRUE) - sd(`elevation  (ft)`, na.rm = TRUE))
    
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  ggplot(GSLE_monthly, aes(x = Month,
                          y = meanElev))+
  geom_pointrange(data = GSLE_monthly,
                  aes(x = Month,
                      y = meanElev,
                      ymin = minElev,
                      ymax = maxElev))+
  geom_pointrange(data = GSLL_monthly,
                    aes(x = Month,
                        y = meanElev,
                        ymin = minElev,
                        ymax = maxElev,
                        color =  'red'))+
    scale_x_continuous('Month',breaks=1:12,labels=months)+
    theme_light()
  #################################### Heatmap of lake elevation ####################################
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
  
  #################################### Hm of  Bear River ####################################
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
  
#################################### SECTION TITLE ####################################
gsl <- datan %>%
    filter(.,water_yr >1965) %>%
    ggplot(aes(x = `elevation  (ft)`,  y = water_yr))+
    geom_smooth()
  
grid.arrange(B, gsl, nrow = 1)