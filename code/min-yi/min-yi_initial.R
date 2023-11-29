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
                                  2021,2022,2023))
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
  ggplot(aes(x = Date, y=Value))+geom_line()

Group_1 <- Canada_border %>%
  filter(Year=='1996'|Year=='1997'|Year=='1998'|Year=='1999'|Year=='2000'|Year=='2001'|Year=='2002')

levels(Canada_border$Month)
