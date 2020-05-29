
# remove all the variables from the global environment
rm(list = ls())

# load the yarrr package and the tidyverse package
library("tidyr")
library("tidyverse")
library("ggplot2")

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
dataset <- read_csv("dati-province//dpc-covid19-ita-province.csv", col_names = TRUE)

# change the data format
dataset$data <- as.Date(as.character(dataset$data))

Lombardia <- dataset[which(dataset$denominazione_regione=="Lombardia"),]

start <- length(unique(Lombardia$denominazione_provincia))+1

nuovi<- vector()
y=1
for (x in start:nrow(Lombardia)){
  nuovi[y]<-Lombardia$totale_casi[x]-Lombardia$totale_casi[x-start+1]
  y<- y+1
}

nuovi <- append(Lombardia$totale_casi[1:start-1],nuovi)

Lombardia <- cbind(Lombardia, nuovi)

Lombardia <- Lombardia[which(Lombardia$denominazione_provincia != "In fase di definizione/aggiornamento"), ]

today <- Lombardia[which(Lombardia$data== max(Lombardia$data)),]

# graph for the number of new cases in a specific date in Lombardia
ggplot(data = today, aes(reorder(denominazione_provincia,-nuovi)))+
  geom_col(aes(y = nuovi, fill = reorder(denominazione_provincia,-nuovi)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  ggtitle(paste("Daily new cases as the ",format(unique(today$data), "%d %b"),", total= ", sum(today$nuovi),sep=""))+
  scale_y_continuous(breaks = seq(0, max(today$nuovi)+100, 100))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  # change the x-axis label
  xlab("")+ylab("")+
  geom_text(aes((x = reorder(denominazione_provincia,-nuovi)), y = nuovi, label = nuovi), size = 3.5, vjust=-0.5)

# graph for the cumulative number of cases in Lombardia (up to a specific date)
ggplot(data = today, aes(reorder(denominazione_provincia,-totale_casi)))+
  geom_col(aes(y = totale_casi, fill = reorder(denominazione_provincia,-totale_casi)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  ggtitle(paste("Cumulative cases as the ",format(unique(today$data), "%d %b"),", total= ", sum(today$totale_casi),sep=""))+
  scale_y_continuous(breaks = seq(0, max(today$totale_casi)+500, 500))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  # change the x-axis label
  xlab("")+ylab("")+
  geom_text(aes((x = reorder(denominazione_provincia,-totale_casi)), y = totale_casi, label = totale_casi), size = 3.5, vjust=-0.5)


# create a dataset just for Monza
monza <- Lombardia[which(Lombardia$sigla_provincia=="MB"),]

# graph for the cumulative number of new cases in Monza (up to a specific date)
ggplot(monza, aes(x = data, y = totale_casi)) +
  # set the dots colour to red
  geom_point(color = "red")+
  geom_line(color = "red")+
  # change the position of the tick markers on the y-axis
  scale_y_continuous(breaks = seq(0, max(monza$totale_casi)+100, 100))+
  scale_x_date(breaks = unique(monza$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  xlab("")+ylab("")+
  ggtitle(paste("Cumulative cases as the ",format(max(monza$data), "%d %b")," in Monza, total= ", sum(monza$nuovi),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))


# graph for the daily new cases in Monza
ggplot(monza, aes(x = data, y = nuovi)) +
  # set the dots colour to red
  geom_col(fill = "red")+
  # change the position of the tick markers on the y-axis
  scale_y_continuous(breaks = seq(0, max(monza$nuovi)+50, 25))+
  scale_x_date(breaks = unique(monza$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  xlab("")+ylab("")+
  ggtitle(paste("Daily new cases as the ",format(max(monza$data), "%d %b")," in Monza, total= ", sum(monza$nuovi),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))+
  geom_text(aes(x = data , y = nuovi, label = nuovi), size = 3.5, vjust=-0.5)
