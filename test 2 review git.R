#Clean Global Enviroment
rm(list=ls())

#importing data from WB
#I used two different code chunks two practice putting them together
library(WDI)
emmisions=WDI(country="all",
              indicator="EN.ATM.CO2E.PC",
              start = 2013,end = 2013,
              extra=FALSE,cache=NULL)

gdp_per_capita=WDI(country="all",
                   indicator= "NY.GDP.PCAP.CD",
                   start = 2013,end = 2013,
                   extra=FALSE,cache=NULL)

#checking the class of the year
class(emmisions$year)
class(gdp_per_capita$year)
summary(gdp_per_capita)
summary(emmisions)


#changing year to numeric class
emmisions$year=as.numeric(emmisions$year)
gdp_per_capita$year=as.numeric(gdp_per_capita$year)

#getting iso3 labels for the data
library(countrycode)
emmisions$country_code =countrycode(sourcevar = emmisions$iso2c,
                                    origin = "iso2c",
                                    destination = "iso3c",
                                    warn = TRUE)

gdp_per_capita$country_code =countrycode(sourcevar = gdp_per_capita$iso2c,
                                         origin = "iso2c",
                                         destination = "iso3c",
                                         warn = TRUE)

#filtering out the regional data
#we can do this because the regional codes do not have iso3
library(tidyverse)
emmisions<-
  emmisions%>%
  dplyr::filter(!(country_code=="NA"))

gdp_per_capita <-
  gdp_per_capita%>%
  dplyr::filter(!(country_code=="NA"))

#check to make sure everything went through
subset(emmisions,country_code=="NA")
subset(gdp_per_capita,country_code=="NA")

#merging data
merged_data= left_join(x=gdp_per_capita,y=emmisions,
                       by=c("country_code","year"))
#creating new variable to see if country x and y match
merged_data<-
  merged_data %>% 
  mutate(countries_match= ifelse(country.x==country.y,
                                 "yes",
                                 "no"))
#check where they did not match
subset(merged_data, countries_match=="no")


#drop country.x and rename country.y as country
merged_data<-
  merged_data %>% 
  select(-c("country.x","iso2c.y","iso2c.x")) %>% #the select function selects or deletes variables
  dplyr::rename("country"="country.y")


#changing the name of the variables
merged_data<-  
  merged_data %>% 
  dplyr::rename("Emmisions"="EN.ATM.CO2E.PC")

merged_data<-
  merged_data %>% 
  dplyr::rename("GDP per Capita"="NY.GDP.PCAP.CD")

merged_data<-
  merged_data %>% 
  dplyr::rename("GDP_per_Capita"="GDP per Capita")


#sorting the variables using relocate
library(dplyr)
merged_data <-
  merged_data%>%
  relocate("country", "country_code", "year", 
           "GDP per Capita", "Emmisions")

#drop the countries match variabe
merged_data$countries_match=NULL

#cleaning global enviroment of dataframes we dont need anymore
rm(emmisions,gdp_per_capita)

#checking to see if there are NA valtues
table(merged_data$Emmisions, exclude=TRUE)
table(merged_data$GDP_per_Capita, exclue=TRUE)
subset(merged_data, is.na(Emmisions))
subset(merged_data,is.na(GDP_per_Capita))

#checking for correlation
cor(merged_data$GDP_per_Capita,
    merged_data$Emmisions,use="complete.obs")

#other way to remove 
data_no_NAS=na.omit(merged_data, select=c("GDP_per_Capita","Emmisions"))
cor(data_no_NAS$Emmisions,data_no_NAS$GDP_per_Capita)

#Viewing the data on the scatter plot
library(ggplot2)
scatterplot= ggplot(data=data_no_NAS,
                    aes(GDP_per_Capita,Emmisions))+
  geom_point(stat="identity")+
  labs(x="Gdp per Capita",
       y="Emmisions",
       title="Emmisions vs GDP 2013")
print(scatterplot)

#take the log to aviod the skwness of the data
data_no_NAS$log_GDP= log(data_no_NAS$GDP_per_Capita +1)
data_no_NAS$log_Emmisions= log(data_no_NAS$Emmisions +1)

#using it in a scatterplot
scatterplot2= ggplot(data=data_no_NAS,
                     aes(log_GDP,log_Emmisions))+
  geom_point(stat="identity")+
  geom_smooth(method=lm)
labs(x="Gdp per Capita",
     y="Emmisions",
     title="Emmisions vs GDP 2013")
print(scatterplot2)

#finding corratlation
cor(data_no_NAS$log_Emmisions,data_no_NAS$log_GDP)
