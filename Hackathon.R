install.packages("readxl")

library(tidyverse)
library(readxl)

#Load the dataset from the website
url1 <- "https://query.data.world/s/tw3oaknxjlqods27xzzbpa3do4rmfr"
p1f <- tempfile()
download.file(url1, p1f, mode="wb")
happy_data <- read_excel(path = p1f)

#View the dataset
View(happy_data)

str(happy_data)

##TIDYING THE DATA

#Rename the columns
colnames(happy_data) <- c("WP5_Country", "country.name", "Year", "Happiness", "GDP", "Soc_Support", "Life_Exp", "Choice_Freedom", "Generosity", "Corrupt_Percept", "Pos_Aff", "Neg_Aff", "Gov_Confident", "Democratic_Qual", "Delivery_Qual", "HappySD", "HappySD_Mean", "Bank_Index", "Bank_Index_Past", "Income", "Trust", "Trust81", "Trust89", "Trust94", "Trust99", "Trust05", "Trust10")
    
#Adding Continents 
install.packages("countrycode")  
library(countrycode)

#Create a blank column
happy_data$continent <- NA

#set the columns as characters
happy_data$county.name <- as.character(happy_data$country.name)
happy_data$continent <- as.character(happy_data$continent)

#Create a Continents column
happy_data <- happy_data %>% 
  mutate(continent = countrycode(sourcevar = country.name, origin = "country.name", destination = "continent")) %>% 
  select(continent, everything())

#View
View(happy_data)

##VISUALISING THE DATA

#Filter the data to only include cases with a continent
happy_data_cont <- happy_data %>% 
  filter(!is.na(continent))

#Plot happiness against year, split by continent, colour by country (Scatter)
ggplot(happy_data_cont, aes (x = Year, y = Happiness, colour = continent)) + 
  geom_point(alpha = .5, show.legend = TRUE) + 
  facet_wrap(~continent)


#Plot happiness against year, split by continent, colour by country (Line)
ggplot(happy_data_cont, aes (x = Year, y = Happiness, colour = country.name)) + 
  geom_line() + 
  facet_wrap(~continent) + 
  theme(legend.position="none")

##(Legend was removed to be able to see graph)


##ANIMATED GRAPH

#Install gganimate
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)

#Animate happiness by year
ggplot(happy_data_cont, aes (x = country.name, y = Happiness, fill = continent)) + 
  geom_bar(stat = "identity", width = 2.0) +
  facet_wrap(~continent) +
  theme(legend.position="none") +
  geom_text(aes(label = country.name), angle = 90) +
  transition_time(Year)


##OVERLY AMBITIOUS CIRCL-Y GRAPH :D

install.packages("ggplot2")
library(ggplot2)


#filter to just one year (2016)
happy_2016 <- happy_data %>% filter(Year == "2016")

View(happy_2016)

empty_bar = 10
to_add = matrix(NA, empty_bar, ncol(happy_2016))
colnames(to_add) = colnames(happy_2016)
happy_2016 = rbind(happy_2016, to_add)
happy_2016$id = seq(1, nrow(happy_2016))

View(happy_2016)

label_data <- happy_2016
number_of_bar <- nrow(label_data)
angle = 90-360*(label_data$id-0.5)/number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

ggplot(happy_2016, aes(x=as.factor(id), y = Happiness)) +
  geom_bar(stat="identity", fill=alpha("green", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  coord_polar(start = 0) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")) +
  geom_text(data=label_data, aes(x=id, y=Happiness, label=country.name), color="black", fontface="bold", alpha=0.6, size=2.5, angle=label_data$angle, hjust=label_data$hjust)
