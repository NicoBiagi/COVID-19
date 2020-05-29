
# remove all the variables from the global environment
rm(list = ls())

# load the yarrr package and the tidyverse package
library("tidyr")
library("tidyverse")
library("ggplot2")
library("scales")

# turn this off if you want scientific notation
options(scipen = 999)

# set the wd to the folder where the csv file is
# WINDOWS
if (Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/zj903545/Documents/GitHub/COVID-19/dati-andamento-nazionale")
} else{
  # MAC
  setwd("/Users/nico/Documents/GitHub/COVID-19/")
}

# load the csv for the whole nation
data_nazione <- read_csv("dati-andamento-nazionale//dpc-covid19-ita-andamento-nazionale.csv", col_names = TRUE)

# change the data format
data_nazione$data <- as.Date(as.character(data_nazione$data))

# get the number of peopl that day every day in the whole nation
deceduti_nazione <- vector()
y=1
for (x in 2:nrow(data_nazione)){
  deceduti_nazione[y] <- data_nazione$deceduti[x] - data_nazione$deceduti[x-1]
  y<-y+1
}
# add the number of people that died on the first day
deceduti_nazione <- append(data_nazione$deceduti[1], deceduti_nazione, after = 1)

data_nazione <- cbind(data_nazione, deceduti_nazione)

# load the csv file for all the regions
data_regioni <- read_csv("dati-regioni//dpc-covid19-ita-regioni.csv", col_names = TRUE)

# change the data format
data_regioni$data <- as.Date(as.character(data_regioni$data))

# get the dataset for Lombardia
lombardia <- data_regioni[which(data_regioni$denominazione_regione=="Lombardia"),c("data", "nuovi_attualmente_positivi", "deceduti")]

deceduti_regione <- vector()
y=1
for (x in 2:nrow(lombardia)){
  deceduti_regione[y] <- lombardia$deceduti[x] - lombardia$deceduti[x-1]
  y<-y+1
}

deceduti_regione <- append(lombardia$deceduti[1], deceduti_regione, after = 1)

nuovi_regione <- lombardia$nuovi_attualmente_positivi

data_nazione <- cbind(data_nazione, deceduti_regione,nuovi_regione)

# load the csv file for all the regions
data_province <- read_csv("dati-province//dpc-covid19-ita-province.csv", col_names = TRUE)

# change the data format
data_province$data <- as.Date(as.character(data_province$data))

# create a dataset just for Monza
monza <- data_province[which(data_province$sigla_provincia=="MB"),c("data", "totale_casi")]

nuovi_monza <- vector()
y=1
for (x in 2:nrow(monza)){
  nuovi_monza[y] <- monza$totale_casi[x] - monza$totale_casi[x-1]
  y<-y+1
}

nuovi_monza <- append(monza$totale_casi[1], nuovi_monza)

data_nazione <- cbind(data_nazione, nuovi_monza)

# plots of deaths per day
ggplot(data = data_nazione, aes(x = data, y = deceduti_nazione))+
  geom_point()+
  geom_line(aes(color ="Italia"))+
  geom_point(aes(y = deceduti_regione), color = "black")+
  geom_line(aes(y = deceduti_regione, color = "Lombardia"))+ 
  scale_x_date(breaks = unique(data_nazione$data), labels = date_format("%d %b"))+
  # change the position of the tick markers on the y-axis
  scale_y_continuous(name = "number of deaths", breaks = seq(-100, max(data_nazione$deceduti_nazione)+50, 50))+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 90, vjust = 0.5),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black'))+
  theme(legend.position = c(0.1,0.91), legend.title = element_blank())+
  labs(x ="")+ggtitle("Plot of deaths per day")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  scale_colour_manual(values = c("red", "green"))

# plots of new cases per day
ggplot(data = data_nazione, aes(x = data, y = nuovi_attualmente_positivi))+
  geom_point()+
  geom_line(aes(color ="Italia"))+
  geom_point(aes(y = nuovi_regione), color = "black")+
  geom_line(aes(y = nuovi_regione, color = "Lombardia"))+
  geom_point(aes(y= nuovi_monza), color = "black")+
  geom_line(aes(y= nuovi_monza, color = "Monza"))+
  scale_x_date(breaks = unique(data_nazione$data), labels = date_format("%d %b"))+
  # change the position of the tick markers on the y-axis
  scale_y_continuous(name = "number of new cases", breaks = seq(-250, 5000, 250), limits = c(-250,5000))+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 90, vjust = 0.5),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black'))+
  theme(legend.position = c(0.1,0.9), legend.title = element_blank())+
  labs(x ="")+ggtitle("Plot of new cases per day")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  scale_colour_manual(values = c("red", "green", "blue"))


# get the testing rate
testing <- vector()
y=1
for (x in 2:nrow(data_nazione)){
  testing[y]<- data_nazione$tamponi[x] - data_nazione$tamponi[x-1]
  y<- y+1
}

testing <- append(data_nazione$tamponi[1], testing)

data_nazione <- cbind(data_nazione, testing)

ggplot(data = data_nazione, aes(x = data, y = nuovi_attualmente_positivi))+
  geom_point()+
  geom_line(aes(color ="Number of New Cases per day"))+
  geom_point(aes(y=testing), color = "black")+
  geom_line(aes(y=testing, color = "Number of New People Tested per day"))+
  scale_x_date(breaks = unique(data_nazione$data), labels = date_format("%d %b"))+
  # change the position of the tick markers on the y-axis
  scale_y_continuous(breaks = seq(0, 30000, 2500), limits = c(0,30000))+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 90, vjust = 0.5),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black'))+
  theme(legend.position = c(0.25,0.9), legend.title = element_blank())+
  scale_colour_manual(values = c("red", "blue"), breaks = c("Number of New People Tested per day","Number of New Cases per day"))+
  labs(x ="", y="")
  
  
