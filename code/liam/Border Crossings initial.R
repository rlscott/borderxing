library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)

# border <- read.csv("https://data.bts.gov/api/views/keg4-3bc2/rows.csv?accessType=DOWNLOAD",stringsAsFactors = T)
# border <- read.csv("~/Border_Crossing_Entry_Data.csv", stringsAsFactors=TRUE)
load("data/border_clean.Rdata")

# Filtering out garbage
look<-border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port_code,Port_name,month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Value==0)
summary(look$Port_name)

look<-look %>% mutate(numtime=year+(month-1)/12)

look %>% 
  ggplot(aes(numtime))+geom_histogram(binwidth=1)
look %>% 
  filter(year==1996) %>% View()

# Each Port averaged over time
ggplotly(border %>% 
  filter(Type=="People") %>% 
  mutate(date=lubridate::my(Date),
       month=month(date),
       year=year(date)) %>% 
  group_by(Port_code,Port_name,month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value,color=Port_name))+
  geom_line()+
  geom_vline(xintercept=2001+(9-1)/12,color='red')+
  facet_wrap(~Border)+
  theme(legend.position = "none"))

# Map of station locations
stations<-unique(border %>% select(Port_code,Port_name,Longitude,Latitude))

leaflet(stations) %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude,~Latitude,radius = 3)

# San Yisidro over time
border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port_code,Port_name,month,year) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Port_name=="San Ysidro") %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+geom_line()+
  geom_vline(xintercept=2001+(9-1)/12,color='red') # sep 11

# Avg over time
border %>% 
  filter(Type=="People") %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line()+
  # geom_vline(xintercept=1996+(12-1)/12,color='red')+ # end guatemala civil war
  geom_vline(xintercept=2001+(9-1)/12,color='red')+ # sep 11
  geom_vline(xintercept=2011+(11-1)/12,color='red')+ # Guatemala President
  geom_vline(xintercept=2017+(1-1)/12,color='red')+ # Trump
  geom_vline(xintercept=2020+(3-1)/12,color='red')+ # Covid
  facet_wrap(~Border)+
  theme(legend.position = "none")
summary(my(border$Date))
# Port By state and explaining 9/11 with 3 ports
border %>% 
  filter(Type=="People") %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port_code,Port_name,month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Port_name=="El Paso") %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value,color=Port_name))+
  geom_line()+
  theme(legend.position = "none")

border %>% 
  filter(Type=="People",Border=="US-Mexico Border",
         Port_name!="San Ysidro",Port_name!="El Paso",Port_name!="Calexico") %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border,State) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12))+
  geom_line(aes(y=Value))+
  # geom_line(aes(y=filt$Value))+
  # geom_vline(xintercept=1996+(12-1)/12,color='red')+ # end guatemala civil war
  geom_vline(xintercept=2001+(9-1)/12,color='red')+ # sep 11
  # geom_vline(xintercept=2011+(11-1)/12,color='red')+ # Guatemala President
  # geom_vline(xintercept=2017+(1-1)/12,color='red')+ # Trump
  # geom_vline(xintercept=2020+(3-1)/12,color='red')+ # Covid
  facet_wrap(~State,scales="free_y")+
  theme(legend.position = "none")

filt<-border %>% filter(Type=="People",Border=="US-Mexico Border",
                        Port_name!="El Paso",Port_name!="Calexico") %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value))

border %>% 
  filter(Type=="People",Border=="US-Mexico Border") %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12))+
  geom_line(aes(y=Value))+
  geom_line(aes(y=filt$Value),color='deepskyblue3')+
  # geom_vline(xintercept=1996+(12-1)/12,color='red')+ # end guatemala civil war
  geom_vline(xintercept=2001+(9-1)/12,color='red')+ # sep 11
  # geom_vline(xintercept=2011+(11-1)/12,color='red')+ # Guatemala President
  # geom_vline(xintercept=2017+(1-1)/12,color='red')+ # Trump
  # geom_vline(xintercept=2020+(3-1)/12,color='red')+ # Covid
  # facet_wrap(~State,scales="free_y")+
  theme(legend.position = "none")

border %>% filter(Border == "US-Mexico Border") %>% 
  ggplot(aes(x = reorder(Port_name, as.numeric(Measure), na.rm = TRUE), fill = Measure)) + 
  geom_bar(position = "fill") + coord_flip() + xlab("Port Name") + 
  ylab("Count Precentage by Measure")

# Avg of each measure in mexico over time
border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border,Measure,Port_name) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Border=="US-Mexico Border") %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line(aes(group=Port_name))+
  geom_vline(xintercept=2020+(3-1)/12,color='red')+ # Covid
  facet_wrap(~Measure,scales="free_y")+
  theme(legend.position = "none")

Trainp<-border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(month,year,Border,Measure,Port_name) %>% 
  summarize(Value=sum(Value)) %>% 
  filter(Border=="US-Mexico Border",Measure=="Bus Passengers")
  ggplot(aes(x=Value))+
  geom_histogram()
ggplotly(Trainp)

Trainp$Port_name[Trainp$Value==max(Trainp$Value)]

ggplotly(border %>% 
  # filter(Measure=="Bus Passengers") %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  group_by(Port_code,Port_name,month,year,Border,Measure) %>% 
  summarize(Value=sum(na.omit(Value))) %>% 
  filter(Port_name=="Otay Mesa") %>%
  ggplot(aes(x=year + (month-1)/12,y=Value,color=Port_name))+
  geom_point()+
  facet_wrap(~Measure)+
  theme(legend.position = "none"))

sum(border$Value[border$Measure=='Truck Containers Empty'])+sum(border$Value[border$Measure=='Truck Containers Loaded'])
sum(border$Value[border$Measure=='Trucks'])

border %>% filter(Measure=='Train Passengers',Border=='US-Mexico Border')

# length value correlation
stateeffect<-border %>% filter(Border == "US-Mexico Border") %>% 
  group_by(State) %>% summarize(Value=sum(Value)) %>% ungroup()
bordlength<-c(372.5,140.4,179.5,1241.0)
reg<-stateeffect %>% mutate(long=bordlength)
reg %>% 
  ggplot(aes(x=long,y=Value))+geom_point()+geom_smooth(method = 'lm',se=F)
lin<-lm(Value~long,data=reg)
adjlin<-lm(Value~long,data=filter(reg,State!='California'))
summary(lin)
summary(adjlin)

