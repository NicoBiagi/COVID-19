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

dates <- unique(dataset$date)
dates <- sort(dates)

for (i in 1:length(dates)){
  DATA <- dates[i]
  subset <- dataset[which(dataset$date== DATA),]
  p<- ggplot(subset, aes( x = total_cases, y = total_deaths))+
    geom_point(alpha = 0.7, show.legend = FALSE, aes( color = location))+
    geom_text(aes(label=location),hjust=0, vjust=0, check_overlap =  TRUE)+
    theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
          # remove the background grid
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          # change the background colour to white
          panel.background = element_rect(fill = 'white', color = 'black')) +
    xlab("Number of Cases") + ylab("Number of Deaths")+
    ggtitle(paste('Date: ', DATA, '; Total deaths= ', sum(subset$total_deaths), sep =""))+
    xlim(0,1000000)+ylim(0,100000)
    
  # ggsave(paste('COVID-19-', DATA,'-dot.jpeg',sep = ""), plot = p, device = 'jpeg' )
  
  
  p1<- ggplot(subset, aes(reorder(location,total_deaths)))+
    geom_col(aes(y = total_deaths, fill = location), show.legend = FALSE)+
    theme(axis.text.x = element_blank(),axis.ticks = element_blank(),
          # remove the background grid
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          # change the background colour to white
          panel.background = element_rect(fill = 'white', color = 'black')) +
    geom_text(aes((x = reorder(location,-total_deaths)), y = total_deaths, label = total_deaths), size = 3)+
    xlab("") + ylab("Number of Deaths")+ylim(0,100000)+
    ggtitle(paste('Date: ', DATA, '; Total deaths= ', sum(subset$total_deaths), sep =""))+
    coord_flip()
  
  ggsave(paste('COVID-19-', DATA,'-bar.jpeg',sep = ""), plot = p1, device = 'jpeg' )
    
  
}