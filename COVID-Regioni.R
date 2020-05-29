
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
  setwd("C:/Users/zj903545/Documents/GitHub/COVID-19/")
} else{
  # MAC
  setwd("/Users/nico/Documents/GitHub/COVID-19/")
}
 
# load the csv file
data_regioni <- read_csv("dati-regioni//dpc-covid19-ita-regioni.csv", col_names = TRUE)

# change the data format
data_regioni$data <- as.Date(as.character(data_regioni$data))

# get the how many regions are in the data_regioni. We need to remove the first day, since it is not possible to calculate the "new deaths" on day 1
start <- length(unique(data_regioni$denominazione_regione))+1

# get the nunmber of new deaths per day
new_deaths <- vector()
y<-1
for (x in start:nrow(data_regioni)){
  new_deaths[y] <- data_regioni$deceduti[x]- data_regioni$deceduti[x-start+1]
  y<-y+1
}

# add the number of deats from the first day
new_deaths <- append(data_regioni$deceduti[1:(start-1)], new_deaths)

# add 
data_regioni <- cbind(data_regioni, new_deaths)

today <- data_regioni[which(data_regioni$data== max(data_regioni$data)),]

# graph for cumulative deaths for all the regions (up to date)
ggplot(data_regioni, aes(x= data, y = deceduti, color = denominazione_regione))+
  geom_point(show.legend = FALSE)+
  geom_line(aes(group = denominazione_regione))+
  scale_y_continuous(breaks = seq(0, max(data_regioni$deceduti)+250, 250))+
  scale_x_date(breaks = unique(data_regioni$data), labels = date_format("%d %b"))+
  # change the font of the tick markers on the x-axis and rotate the ticks 90 degrees
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  # change the x-axis label
  ggtitle(paste("Cumulative deaths,as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data_regioni$new_deaths),sep=""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.25,0.65),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "Regioni")+
  xlab("")+ylab("")

# graphs for daily deaths in all the regions
ggplot(data=data_regioni, aes(x= data, y = new_deaths, color = denominazione_regione))+
  geom_point(show.legend = FALSE)+
  geom_line(aes(group = denominazione_regione))+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  ggtitle(paste("Daily deaths,as the ", format(unique(today$data), "%d %b"),"; total = ", sum(data_regioni$new_deaths),sep=""))+
  scale_y_continuous(breaks = seq(0, max(data_regioni$new_deaths)+50, 50))+
  scale_x_date(breaks = unique(data_regioni$data), labels = date_format("%d %b"))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.25,0.65),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "Regioni")+
  # change the x-axis label
  xlab("")+ylab("")

# graph for cumulative deaths for a specific date in all the region
ggplot(data = today, aes(reorder(denominazione_regione,-deceduti)))+
  geom_col(aes(y = deceduti, fill = reorder(denominazione_regione,-deceduti)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  ggtitle(paste("Cumulative deaths as the ",format(unique(today$data), "%d %b"),"; total = ", sum(today$deceduti),sep=""))+
  scale_y_continuous(breaks = seq(0, max(today$deceduti)+500, 500))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  # change the x-axis label
  xlab("")+ylab("")+
  geom_text(aes((x = reorder(denominazione_regione,-deceduti)), y = deceduti, label = deceduti), size = 3.5, vjust=-0.5)


# graph for daily deaths for a specific date in all the region
ggplot(data = today, aes(reorder(denominazione_regione,-new_deaths)))+
  geom_col(aes(y = new_deaths, fill = reorder(denominazione_regione,-new_deaths)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  ggtitle(paste("Daily deaths on the ",format(unique(today$data), "%d %b"),"; total = ", sum(today$new_deaths),sep=""))+
  scale_y_continuous(breaks = seq(0, max(today$new_deaths)+50, 50))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  # change the x-axis label
  xlab("")+ylab("")+
  geom_text(aes((x = reorder(denominazione_regione,-new_deaths)), y = new_deaths, label = new_deaths), size = 3.5, vjust=-0.5)

# graph for daily deaths for a specific date in all the region
ggplot(data = today, aes(reorder(denominazione_regione,-totale_attualmente_positivi)))+
  geom_col(aes(y = totale_attualmente_positivi, fill = reorder(denominazione_regione,-totale_attualmente_positivi)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  ggtitle(paste("Positve patients on the ",format(unique(today$data), "%d %b"),"; total = ", sum(today$totale_attualmente_positivi),sep=""))+
  scale_y_continuous(breaks = seq(0, max(today$totale_attualmente_positivi)+1000, 1000))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  # change the x-axis label
  xlab("")+ylab("")+
  geom_text(aes((x = reorder(denominazione_regione,-totale_attualmente_positivi)), y = totale_attualmente_positivi, label = totale_attualmente_positivi), size = 3.5, vjust=-0.5)