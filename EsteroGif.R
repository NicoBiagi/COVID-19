# remove all the variables from the global environment
rm(list = ls())

library(ggplot2)
library(gapminder)
library(gganimate)
library(gifski)
library("tidyr")
library("tidyverse")

# turn this off if you want scientific notation
options(scipen = 999)

con <- url( "https://covid.ourworldindata.org/data/ecdc/full_data.csv")

data <- read.csv(con)
# change the data format
data$date <- as.Date(as.character(data$date))


# get the dataset for today
data2 <- data[which(data$date >= max(data$date)),]

# get the countries that, as today, have had fewer than 100 deaths
data3 <- data2[which(data2$total_deaths<1000),]

# get the countries names
remove_countries <- levels(droplevels(data3$location))

# remove the countries from the dataset
dataset <- data[ ! data$location %in% remove_countries, ]

dataset <- dataset[which(dataset$location!="World"),]

# p<-ggplot(dataset, aes( x = total_cases, y = total_deaths))+
#   geom_point(alpha = 0.7, show.legend = FALSE, aes( color = location))+
#   geom_text(aes(label=location),hjust=0, vjust=0)+
#   theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
#         # remove the background grid
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         # change the background colour to white
#         panel.background = element_rect(fill = 'white', color = 'black')) +
#   scale_x_log10()+
#   # Here comes the gganimate specific bits
#   labs(title = paste('Date: {frame_time}; Total deaths=', sum(dataset$total_deaths), sep =""), x = '', y = 'total deaths') +
#   transition_time(date)
# animate(p, duration = 10, fps = 10, width = 1200, height = 800, renderer = gifski_renderer())
# anim_save("COVID-19-dot.gif")

p1<- ggplot(dataset, aes(reorder(location,-total_deaths)))+
  geom_col(aes(y = total_deaths, fill = reorder(location,-total_deaths)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black')) +
  xlab("") + ylab("Number of Deaths")+
  ggtitle('Date: {frame_time}')+
  transition_time(date)
animate(p1, duration = 14, fps = 10, width = 1200, height = 800, renderer = gifski_renderer())
anim_save("COVID-19-bar.gif")

