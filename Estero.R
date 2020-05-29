
# remove all the variables from the global environment
rm(list = ls())

# load the yarrr package and the tidyverse package
library("tidyr")
library("tidyverse")
library("ggplot2")

# turn this off if you want scientific notation
options(scipen = 999)

con <- url( "https://covid.ourworldindata.org/data/ecdc/full_data.csv")

data <- read.csv(con)

# change the data format
data$date <- as.Date(as.character(data$date))

# include only dates after 27 Feb
data2 <- data[which(data$date >= "2020-03-01"),]

# create a dataset for a single state
single_state <- data2[which(data2$location== "Latvia"),]

# create a dataset for several countries
paesi <- data2[which(data2$location == "Italy" | data2$location == "United Kingdom" | data2$location == "Spain" | data2$location == "Turkey" |data2$location == "Latvia"),]

# create a dataset for a specific date
today <- data2[which(data2$date == max(data2$date)),]
today_single <- single_state[which(single_state$date == max(data2$date)),]
today_multiple <- paesi[which(paesi$date == max(data2$date)),]

# create a bar plot for the daily new cases of several countries on a specific date
ggplot(data = today_multiple, aes(reorder(location,-new_cases)))+
  geom_col(aes(y = new_cases, fill = reorder(location,-new_cases)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_continuous(breaks = seq(0, max(today_multiple$new_cases)+1000, 1000))+
  ggtitle(paste("Daily new cases on ", format(unique(today_multiple$date), "%d %b"),"; total = ", sum(today_multiple$new_cases),sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes((x = reorder(location,-new_cases)), y = new_cases, label = new_cases), size = 3.5, vjust=-0.5)+
  xlab("") + ylab("")

# create a graph for the cumulative number of new cases
ggplot(data = paesi, aes(x= date, y = new_cases))+
  geom_point(aes( color = location))+
  geom_line(aes( color = location))+
  scale_x_date(breaks = unique(paesi$date))+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_continuous(breaks = seq(0, max(paesi$new_cases)+1000, 1000))+
  ggtitle(paste("Number of new cases per day, total = ", sum(paesi$total_cases), sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.15,0.8),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "List of Countries")+
  xlab("") + ylab("")
  
# create a bar plot for the daily new deaths for several countries on a specific date
ggplot(data = today_multiple, aes(reorder(location,-new_deaths)))+
  geom_col(aes(y = new_deaths, fill = reorder(location,-new_deaths)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_continuous(breaks = seq(0, max(today_multiple$new_deaths)+50, 50))+
  ggtitle(paste("Daily new deaths on ", format(unique(today_multiple$date), "%d %b"),"; total = ", sum(today_multiple$new_deaths),sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes((x = reorder(location,-new_deaths)), y = new_deaths, label = new_deaths), size = 3.5, vjust=-0.5)+
  xlab("") + ylab("")

# create a graph for the cumulative number of new deaths
ggplot(data = paesi, aes(x= date, y = new_deaths))+
  geom_point(aes( color = location))+
  geom_line(aes( color = location))+
  scale_x_date(breaks = unique(paesi$date))+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_continuous(breaks = seq(0, max(paesi$new_deaths)+100, 100))+
  ggtitle(paste("Number of new deaths per day, total = ", sum(paesi$total_deaths), sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.15,0.8),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "List of Countries")+
  xlab("") + ylab("")

# graph for the total cases on a specific date
ggplot(data = today_multiple, aes(reorder(location,-total_cases)))+
  geom_col(aes(y = total_cases, fill = reorder(location,-total_cases)), show.legend = FALSE)+
  theme(axis.text.x = element_text(size=8, family='Arial', angle = 45, vjust = 1, hjust = 1),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_continuous(breaks = seq(0, max(today_multiple$total_cases)+20000, 20000))+
  ggtitle(paste("Total cases on ", format(unique(today_multiple$date), "%d %b"),"; total = ", sum(today_multiple$total_cases),sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  geom_text(aes((x = reorder(location,-total_cases)), y = total_cases, label = total_cases), size = 3.5, vjust=-0.5)+
  xlab("") + ylab("")

# include only rows where at least 1 person is infected
data_new <- data[which(data$total_cases >= 1),]

# create a dataset for several countries
paesi_new <- data_new[which(data_new$location == "Italy" | data_new$location == "United Kingdom" | data_new$location == "Spain" | data_new$location == "Turkey" |data_new$location == "Latvia"),]

# add a column at the end of the dataframe composed of just 'NA'
paesi_new[,(ncol(paesi_new)+1)]<-NA

# rename the the last column of the dataframe as start
colnames(paesi_new)[ncol(paesi_new)]<- "start"

# get the names of the countries
loc <- unique(paesi_new$location)

# create an empty variable
f= NULL

# create a loop that creates a dataframe for each contry and counts the number of rows in that dataframe, At the end we re-merge all the datasets together
for (x in 1:length(loc)){
  
  # select a specific country
  LOC <- loc[x]
  
  # select the rows that belong to the specifc country
  dataset <- paesi_new[which(paesi_new$location == LOC),]
  
  # give each  row a number (i.e., day since case 1)
  dataset$start <- 1:nrow(dataset)
  
  # create a new dataset that includes the day numbe since case 1
  f <- rbind(f, dataset)
}

# create a graph where the infection curve for each country is shown
ggplot(data = f, aes(x = start, y= total_cases))+
  geom_point(aes( color = location))+
  geom_line(aes( color = location))+
  scale_x_continuous(breaks = seq(0, max(f$start),5))+
  theme(axis.text.x = element_text(size=8, family='Arial'),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_continuous(breaks = seq(0, max(f$total_cases)+10000, 10000))+
  ggtitle(paste("Infection curves, total infected wolrd-wide = ", sum(data2$new_cases), sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.15,0.8),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "List of Countries")+
  xlab("Day after case 1 was reported") + ylab("")

ggplot(data = f, aes(x = start, y= total_cases))+
  geom_point(aes( color = location))+
  geom_line(aes( color = location))+
  scale_x_continuous(breaks = seq(0, max(f$start),5))+
  theme(axis.text.x = element_text(size=8, family='Arial'),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_log10()+
  ggtitle(paste("Infection curves, total infected wolrd-wide = ", sum(data2$new_cases), sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.15,0.8),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "List of Countries")+
  xlab("Day after case 1 was reported") + ylab("")

ggplot(data = f, aes(x = start, y= new_deaths))+
  geom_point(aes( color = location))+
  geom_line(aes( color = location))+
  scale_x_continuous(breaks = seq(0, max(f$start),5))+
  theme(axis.text.x = element_text(size=8, family='Arial'),
        # remove the background grid
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        # change the background colour to white
        panel.background = element_rect(fill = 'white', color = 'black') )+
  scale_y_log10()+
  ggtitle(paste("Infection curves, deaths per day = ", sum(data2$new_deaths), sep = ""))+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5)) +
  theme(legend.position = c(0.15,0.8),legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+labs(color = "List of Countries")+
  xlab("Day after case 1 was reported") + ylab("")
