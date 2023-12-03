load("data/border_clean.Rdata")

unique(border$Border)
#library
library(tidyverse)
library(plotly)
ggplotly()
#read data
Canada_border <- border %>%
  filter(Border == "US-Canada Border")%>%
  mutate(
    Year = factor(Year,levels = c(1996,1997,1998,1999,2000,2001,2002,
                                  2003,2004,2005,2006,2007,2008,2009,2010,2011,
                                  2012,2013,2014,2015,2016,2017,2018,2019,2020,
                                  2021,2022,2023)),
    date=lubridate::my(Date),
    month=month(date),
    year=year(date)
  )

#port code
#How many port have complete set
Canada_border %>%
  group_by(Port_code) %>%
  summarise(
    n = n()
  ) %>%
  arrange(n) %>%
  View()

Canada_border %>%
  group_by(Port_name) %>%
  summarise(
    n = n()
  ) %>%
  arrange(n) %>%
  View()
#How many port should we consider ?
#Does Port_name change ? There's 118 levels but I only have 90 rows.
#Only 89 when I group_by Port_name

Canada_border %>%
  group_by(Port_code)%>%
  summarise(
    n = n()
  ) %>%
  arrange(n)%>%
  ggplot(aes(x = Port_code,y=n))+geom_point()+
  scale_x_discrete(guide = guide_axis(angle = 90))

Canada_border %>%
  group_by(Port_name)%>%
  summarise(
    n = n()
  ) %>%
  arrange(n)%>%
  ggplot(aes(x = Port_name,y=n))+geom_point()+
  scale_x_discrete(guide = guide_axis(angle = 90))

#Eastport
Canada_border %>%
  filter(Port_name == 'Eastport')%>%
  group_by(Port_code)%>%
  summarise(
    n = n()
  )
# Eastport has two Port_code
Canada_border %>%
  filter(Port_name == 'Eastport')%>%
  group_by(Port_code)%>%
  ggplot(aes(x = Year,y = Value,colour = Port_code))+geom_point()


# State
Canada_border %>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State)

Canada_border %>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State,scales = 'free_y')

#I think we can consider the population in each state ?
#i.e. the transportation rate ?

#Measure
Canada_border %>%
  group_by(Measure) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~Measure)

Canada_border %>%
  group_by(Measure) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~Measure,scales = 'free_y')

#We can see some seasonal patterns in measure

# How about group_by State with specific measure ?
Canada_border %>%
  filter(Measure == 'Buses')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State)

Canada_border %>%
  filter(Measure == 'Buses')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State,scales = 'free_y')

Canada_border %>%
  filter(Measure == 'Personal Vehicles')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State)

Canada_border %>%
  filter(Measure == 'Personal Vehicles')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State,scales = 'free_y')

Canada_border %>%
  filter(Measure == 'Personal Vehicle Passengers')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State)

Canada_border %>%
  filter(Measure == 'Personal Vehicle Passengers')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_line()+facet_wrap(~State,scales = 'free_y')

#I think Personal Vehicles is a factor.

#Consider Month
Canada_border %>%
  group_by(Year,Month) %>%
  summarise(
    average_value = mean(Value,na.rm = T)
  ) %>%
  ggplot(aes(x = factor(Month), y=average_value,group = Year,colour = Year))+geom_line()

Canada_border %>%
  group_by(Year) %>%
  ggplot(aes(x = Month, y=Value))+geom_point()+
  facet_wrap(~Year,scales = 'free_y')

ggplotly()
Canada_border %>%
  filter(Measure == 'Buses' & State == 'Montana')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()

Group_1 <- Canada_border %>%
  filter(Year=='1996'|Year=='1997'|Year=='1998'|Year=='1999'|Year=='2000'|Year=='2001'|Year=='2002')

levels(Canada_border$Month)

Canada_border %>%
  filter(Year == '2000' & State == 'Montana' & Month == 'Nov')%>%
  group_by(Measure) %>%
  summarise(
    n = sum(Value)
  )%>% View()

Canada_border %>%
  filter(Measure == 'Buses' & Type == 'Object' & State == 'Montana' & Value == 1375) %>% View()

Canada_border %>%
  filter(Year == '1999' & State == 'Montana' & Measure == 'Buses'& Month == 'Nov')%>%
  group_by(Port_name) %>%
  summarize(
    n = Value
  ) %>%View()

Canada_border %>%
  filter(Measure == 'Buses' & Type == 'Object'& State == 'Montana' & Month == 'Nov' & Year == '2000')%>%
  group_by(State) %>%View()

Canada_border %>%
  filter(Year == '2000' & State == 'Montana' & Month == 'Dec')%>%
  group_by(Measure) %>%
  summarise(
    n = sum(Value)
  )%>% View()

Canada_border %>%
  filter(Year == '2000' & State == 'Montana' )%>%
  group_by(Month) %>%
  summarise(
    n = sum(Value)
  )%>% View()

#Overall doesn't change

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Montana' & Year == '2000')%>%
  group_by(Port_name) %>%
  ggplot(aes(x = year + (month-1)/12, y=Value))+geom_point()

Canada_border %>%
  filter(Measure == 'Buses' & State == 'Montana' & Year == '2000')%>%
  group_by(Port_name) %>%
  ggplot(aes(x = year + (month-1)/12, y=Value))+geom_point()

ggplotly()
Canada_border %>%
  filter(Measure == 'Buses' & State == 'Maine')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()

Canada_border %>%
  filter(Year == '2021' & State == 'Maine' & Measure == 'Buses'& Month == 'Jun')%>%
  group_by(Port_name) %>%
  summarize(
    n = Value
  ) %>%View()

#Jun 2021 Maine

Canada_border %>%
  filter(Year == '2021' & State == 'Maine'& Measure == 'Buses')%>%View()


ggplotly()
Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Maine')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Maine' & Year == '1998' & Month == 'Nov' )%>%View

#Maine 1998 Nov 46712


Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Washington')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()
ggplotly()

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Washington' & Year == '2006' & Month == 'Oct' )%>%View()


# Washi 2006 Oct 159086

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Minnesota')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()
ggplotly()

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Minnesota' & Year == '1997' & Month == 'Jun' )%>%View()


#Jun 1997 Min 6026

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Idaho')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()
ggplotly()

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Idaho' & Year == '2000' & Month == 'May' )%>%View()


#May 2000 Idaho 2203

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Michigan')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()
ggplotly()

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Michigan' & Year == '1997' & Month == 'Dec' )%>%View()


#1997 Dec Michigan 135753


Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Vermont')%>%
  group_by(State) %>%
  ggplot(aes(x = Date, y=Value))+geom_point()
ggplotly()

Canada_border %>%
  filter(Measure == 'Bus Passengers' & State == 'Vermont' & Year == '2019' & Month == 'May' )%>%View()


# May 2019 Vermont 19322



