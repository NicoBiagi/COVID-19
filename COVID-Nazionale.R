
# remove all the variables from the global environment
rm(list = ls())

# load the yarrr package and the tidyverse package
library("tidyr")
library("tidyverse")
library("ggplot2")
library("scale")

# turn this off if you want scientific notation
options(scipen = 999)

# set the wd to the folder where the csv file is
# WINDOWS
if (Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/zj903545/Documents/GitHub/COVID-19/")
} else{
  # MAC
  setwd("/Users/nico/Documents/GitHub/COVID-19/")
}

# load the csv file
data <- read_csv("dati-andamento-nazionale//dpc-covid19-ita-andamento-nazionale.csv", col_names = TRUE)

# change the data format
data$data <- as.Date(as.character(data$data))

# get the nunmber of new deaths per day
new_deaths <- vector()
y<-1
for (x in 2:nrow(data)){
  new_deaths[y] <- data$deceduti[x]- data$deceduti[x-1]
  y<-y+1
}

new_deaths <- append(data$deceduti[1], new_deaths)

# add it to the main dataframe
data <- cbind(data, new_deaths)

# get a dataframe just for today
today <- data[which(data$data== max(data$data)),]

# graph for the cumulative number of deaths/ geom_point + geom_line
g1<-ggplot(data = data, aes(x= data, y = deceduti))+
  geom_line()+
  geom_point()
  g1+ scale_y_continuous(breaks = seq(0, max(data$deceduti)+1000, 1000))+
  scale_x_date(breaks = unique(data$data))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Cumulative deaths,as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data$new_deaths),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  xlab("")+ylab("")

# graph for the cumulative number of deaths/ geom_col
ggplot(data = data, aes(x= data, y = deceduti))+
geom_col(fill = "red")+
  scale_y_continuous(breaks = seq(0, max(data$deceduti)+250, 250))+
  scale_x_date(breaks = unique(data$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Cumulative deaths,as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data$new_deaths),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes(x= data, y=deceduti, label = deceduti), size = 3, vjust =-0.5)+
  xlab("")+ylab("")


# graph for the cumulative number of new cases/geom_point + geom_line
ggplot(data = data, aes(x= data, y = totale_attualmente_positivi))+
  geom_line()+
  geom_point()+
  scale_y_continuous(breaks = seq(0, max(data$totale_attualmente_positivi)+2000, 2000))+
  scale_x_date(breaks = unique(data$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Cumulative positive patients,as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data$nuovi_attualmente_positivi),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes(x= data, y=totale_attualmente_positivi, label = totale_attualmente_positivi), size = 2.4, vjust =-0.5, fontface = "bold")+
  xlab("")+ylab("")

# graph for the cumulative number of new cases/geom_col
ggplot(data = data, aes(x= data, y = totale_attualmente_positivi))+
  geom_col(fill = "red")+
  scale_y_continuous(breaks = seq(0, max(data$totale_attualmente_positivi)+2000, 2000))+
  scale_x_date(breaks = unique(data$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Cumulative positive patients,as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data$nuovi_attualmente_positivi),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes(x= data, y=totale_attualmente_positivi, label = totale_attualmente_positivi), size = 2.4, vjust =-0.5, fontface = "bold")+
  xlab("")+ylab("")

# graph for the daily number of new cases
ggplot(data =  data, aes( x= data, y= nuovi_attualmente_positivi))+
  geom_col(fill="red")+
  scale_y_continuous(breaks = seq(0, max(data$totale_attualmente_positivi)+2000, 2000))+
  scale_x_date(breaks = unique(data$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Daily new cases, as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data$nuovi_attualmente_positivi),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes(x= data, y=nuovi_attualmente_positivi, label = nuovi_attualmente_positivi), size = 3, vjust =-0.5, fontface = "bold")+
  xlab("")+ylab("")

# graph for the daily number of deaths
ggplot(data =  data, aes( x= data, y= new_deaths))+
  geom_col(fill="red")+
  scale_y_continuous(breaks = seq(0, max(data$new_deaths)+50, 50))+
  scale_x_date(breaks = unique(data$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Daily deaths, as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data$new_deaths),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes(x= data, y=new_deaths, label = new_deaths), size = 3, vjust =-0.5, fontface = "bold",check_overlap = TRUE)+
  xlab("")+ylab("")
