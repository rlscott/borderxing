library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)

# border <- read.csv("https://data.bts.gov/api/views/keg4-3bc2/rows.csv?accessType=DOWNLOAD",stringsAsFactors = T)
border <- read.csv("~/Border_Crossing_Entry_Data.csv", stringsAsFactors=TRUE)

# Filtering out garbage
look<-border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port.Code,Port.Name,month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Value==0)
summary(look$Port.Name)

look<-look %>% mutate(numtime=year+(month-1)/12)

look %>% 
  ggplot(aes(numtime))+geom_histogram(binwidth=1)
look %>% 
  filter(year==1996) %>% View()

# Each Port averaged over time
border %>% 
  mutate(date=lubridate::my(Date),
       month=month(date),
       year=year(date)) %>% 
  group_by(Port.Code,Port.Name,month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line(aes(group=Port.Name))+
  geom_vline(xintercept=2001+(9-1)/12,color='red')+
  facet_wrap(~Border)+
  theme(legend.position = "none")

# Map of station locations
stations<-unique(border %>% select(Port.Code,Port.Name,Longitude,Latitude))

leaflet(stations) %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude,~Latitude,radius = 3)

# San Yisidro over time
border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port.Code,Port.Name,month,year) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Port.Name=="San Ysidro") %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+geom_line()

# Avg over time
border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line()+
  geom_vline(xintercept=1996+(12-1)/12,color='red')+ # end guatemala civil war
  geom_vline(xintercept=2001+(9-1)/12,color='red')+ # sep 11
  geom_vline(xintercept=2011+(11-1)/12,color='red')+ # Guatemala President
  geom_vline(xintercept=2017+(1-1)/12,color='red')+ # Trump
  geom_vline(xintercept=2020+(3-1)/12,color='red')+ # Covid
  facet_wrap(~Border)+
  theme(legend.position = "none")

# Port Sasabe over time
p<-border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port.Code,Port.Name,month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Port.Name=="Sasabe") %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value,color=Port.Name))+
  geom_point()+
  theme(legend.position = "none")
ggplotly(p)

# Avg of each measure in mexico over time
border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border,Measure) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Border=="US-Mexico Border") %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line()+
  geom_vline(xintercept=2020+(3-1)/12,color='red')+ # Covid
  facet_wrap(~Measure,scales="free_y")+
  theme(legend.position = "none")

Trainp<-border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border,Measure) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Border=="US-Mexico Border",Measure=="Train Passengers") %>% 
  ggplot(aes(x=Value))+
  geom_histogram()
ggplotly(Trainp)

sum(border$Value[border$Measure=='Truck Containers Empty'])+sum(border$Value[border$Measure=='Truck Containers Loaded'])
sum(border$Value[border$Measure=='Trucks'])

border %>% filter(Measure=='Train Passengers',Border=='US-Mexico Border')

