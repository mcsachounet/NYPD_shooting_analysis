#load the dataset
shootings <- read.csv(file = 'data/NYPD_Shooting_Incident_Data__Historic_.csv')
str(shootings)

#install and load packages
install.packages("tidyverse")
install.packages("anytime")
install.packages("forcats")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(anytime)
library(forcats)

#clean the data
#remove the NA and population = 0
a<-shootings$ZIP != "New York" & shootings$ZIP!= "Naan" & shootings$POPULATION!="#N/A" & shootings$POPULATION!="0"
shootings<-shootings[a,]
#transform the occur date into date format
shootings$OCCUR_DATE<- anydate(shootings$OCCUR_DATE)
str(shootings)
min(shootings$OCCUR_DATE)
#transform the population and density into numeric
shootings$POPULATION<- as.numeric(as.character(shootings$POPULATION))
shootings$DENSITY<- as.numeric(as.character(shootings$DENSITY))

##look at the borough by doing a plot
#by occur_date and density
p<- shootings%>%ggplot(aes(x=OCCUR_DATE, y = ..count.., col=BORO))
q=p+geom_density()+xlab("DATE")+ ylab("COUNT")+ggtitle("DENSITY PLOT OF NYC SHOOTING EVENTS")
q
#simple barplot(the best) 
p<-shootings%>%ggplot(aes(x=forcats::fct_infreq(BORO)))+geom_bar()+xlab("")+ ylab("COUNT")+ggtitle("HISTO OF NYC SHOOTING EVENTS FROM 2016 to 2020")
p

#map
total_map<-shootings%>%ggplot(aes(x=Longitude, y=Latitude, col=BORO))+geom_point()+xlab("LONG")+ ylab("LAT")+ggtitle("MAP OF NYC SHOOTING EVENTS FROM 2016 to 2020")
total_map

#map of brooklyn
brooklyn_map<-shootings%>%filter(BORO=="BROOKLYN")%>%ggplot(aes(x=Longitude, y=Latitude))+geom_point()+xlab("LONG")+ ylab("LAT")+ggtitle("MAP OF BROOKLYN SHOOTING EVENTS FROM 2016 to 2020")
brooklyn_map

#look at the 5 worst Precinct in Brooklyn and plot them
shootings_2<-shootings%>%group_by(PRECINCT)%>% tally()
shootings_2<-shootings_2%>%arrange(desc(n))%>%head(n=5)
shootings_2

#look at the top 5 frequent ZIP
group_zip<-shootings%>%group_by(ZIP)%>% tally()
top_5_zip<-group_zip%>%arrange(desc(n))%>%head(n=5)
top_5_zip
#plot this top 5
top_5_histo<-shootings%>%filter(ZIP %in% top_5_zip$ZIP)  %>%
  ggplot(aes(x=forcats::fct_infreq(ZIP)),col=BORO)+geom_bar()+xlab("ZIP")+ ylab("COUNT")+ggtitle("HISTO OF TOTAL SHOOTING EVENTS BY ZIP")
top_5_histo
#map the top 5
group_zip<-shootings%>%group_by(ZIP)%>% tally()
top_5_zip<-group_zip%>%arrange(desc(n))%>%head(n=5)
top_5_zip
brooklyn_map_top_5<-shootings%>%filter(BORO=="BROOKLYN")%>%
  ggplot(aes(x=Longitude, y=Latitude, 
             col=ifelse(ZIP%in%top_5_zip$ZIP,ZIP,"other")))+
  geom_point()+xlab("LONG")+ ylab("LAT")+
  ggtitle("MAP OF BROOKLYN SHOOTING EVENTS FROM 2016 to 2020")+
  labs(col="ZIP")
brooklyn_map_top_5

#plot with the time serie
p<- shootings%>%filter(ZIP %in% top_3_zip$ZIP)%>%ggplot(aes(x=OCCUR_DATE, y = ..count.., col=ZIP))
q=p+geom_density()
q
#simple barplot
p<- shootings%>%filter(ZIP %in% shootings_2$ZIP)%>%ggplot(aes(x = forcats::fct_infreq(ZIP)))
q=p+geom_bar()
q


#take the population into account

group_zip<-shootings%>%group_by(ZIP)%>% tally()
group_zip
str(group_zip)
distinct<-shootings%>%distinct(ZIP,.keep_all = TRUE)%>%arrange(ZIP)
distinct

#see the correlation
group_zip_pop<-group_zip%>%mutate(POPULATION = distinct$POPULATION)
group_zip_pop<-group_zip_pop%>%mutate(DENSITY = distinct$DENSITY)
group_zip_pop<-group_zip_pop%>%mutate(BORO = distinct$BORO)

group_zip_pop_plot<-group_zip_pop%>%
  ggplot(aes(POPULATION,n, col=BORO))+geom_point()+
  scale_x_continuous(trans="log", limit=c(20000,150000), 
                     breaks =c(20000,60000,130000))+
  scale_y_continuous(trans="log", breaks=c(2,20,150,1100))+
  xlab("LOG(POPULATION)")+ 
  ylab("LOG(NUMBER OF SHOOTING FROM 2016 TO 2020)")+
  ggtitle("                        ZIP SCATTERPLOT")

group_zip_pop_plot
cor(group_zip_pop$POPULATION,group_zip_pop$n, method="pearson")


#define the top 5 shooting rates

group_zip_shooting_rate<-group_zip_pop%>%mutate(shooting_rate = as.numeric(n)/
                                                  as.numeric(POPULATION))
group_zip_shooting_rate<-group_zip_shooting_rate%>%arrange(desc(shooting_rate))%>%
  head(5)
group_zip_shooting_rate

#plot this top 5
top_5_histo_shooting_rate_plot<-group_zip_shooting_rate%>%
  ggplot(aes(x=reorder(ZIP,-shooting_rate),y = shooting_rate, fill=BORO))+
  geom_bar(stat = "identity")+xlab("ZIP")+ ylab("SHOOTING RATE")+ggtitle("SHOOTING RATES BY ZIP")
top_5_histo_shooting_rate_plot

#plot this top 5 with count and homicide
top_5_histo_murder_rate_plot<-shootings%>%filter(ZIP %in% group_zip_shooting_rate$ZIP)%>%
  ggplot(aes(x=forcats::fct_infreq(ZIP),fill=STATISTICAL_MURDER_FLAG))+
  geom_bar()+xlab("ZIP")+ ylab("COUNT")+ggtitle("SHOOTING EVENTS BY ZIP")+labs(col="ZIP")
top_5_histo_murder_rate_plot


shootings%>%filter(ZIP %in% group_zip_shooting_rate$ZIP)


#map the top brooklyn
group_zip_shooting_rate
map_top_3_shooting_rate<-shootings%>%filter(BORO=="BROOKLYN")%>%
  ggplot(aes(x=Longitude, y=Latitude, 
             col=ifelse(ZIP%in%group_zip_shooting_rate$ZIP,ZIP,"other")))+
  geom_point()+xlab("LONG")+ ylab("LAT")+
  ggtitle("MAP OF BROOKLYN SHOOTING EVENTS FROM 2016 to 2020")+
  labs(col="ZIP")
map_top_3_shooting_rate


#map the top bronx
group_zip_shooting_rate
map_top_3_shooting_rate<-shootings%>%filter(BORO=="BRONX")%>%
  ggplot(aes(x=Longitude, y=Latitude, 
             col=ifelse(ZIP%in%group_zip_shooting_rate$ZIP,ZIP,"other")))+
  geom_point()+xlab("LONG")+ ylab("LAT")+
  ggtitle("MAP OF BRONX SHOOTING EVENTS FROM 2016 to 2020")+
  labs(col="ZIP")
map_top_3_shooting_rate
