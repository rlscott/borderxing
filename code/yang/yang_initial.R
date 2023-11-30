load("data/border_clean.Rdata")
library(tidyverse)
library(plotly)
library(gridExtra)
library(ggplot2)
#ggplotly()
#read data
Canada_border <- border %>%
  filter(Border == "US-Canada Border")%>%
  mutate(
    Year = factor(Year,levels = c(1996,1997,1998,1999,2000,2001,2002,
                                  2003,2004,2005,2006,2007,2008,2009,2010,2011,
                                  2012,2013,2014,2015,2016,2017,2018,2019,2020,
                                  2021,2022,2023))
  )
Mexico_border <- border %>%
  filter(Border == "US-Mexico Border")%>%
  mutate(
    Year = factor(Year,levels = c(1996,1997,1998,1999,2000,2001,2002,
                                  2003,2004,2005,2006,2007,2008,2009,2010,2011,
                                  2012,2013,2014,2015,2016,2017,2018,2019,2020,
                                  2021,2022,2023))
  )

Mexico_border_max10 <- Mexico_border %>%
  filter(Type == "People") %>%
  slice_max(Value,n = 10)

Canada_border_max10 <- Canada_border %>%
  filter(Type == "People") %>%
  slice_max(Value,n = 10)


Canada_border_VP <- Canada_border %>%
  filter(Measure == "Personal Vehicle Passengers")
Canada_border_VP %>%
  ggplot(aes(x= Port_name,y=Value))+geom_boxplot()

Mexico_border_VP <- Mexico_border %>%
  filter(Measure == "Personal Vehicle Passengers")
Mexico_border_VP %>%
  ggplot(aes(x= Port_name,y=Value))+geom_boxplot()

border1 <- border
border1$Date <- as.Date(paste0("01 ", border1$Date), format = "%d %b %Y")
border1 %>%
  filter(Port_name =="El Paso" |Port_name =="Detroit"|Port_name =="Buffalo Niagara Falls") %>%
  filter(Measure == "Personal Vehicle Passengers")%>%
  ggplot(aes(x = Date, y = Value, color = Port_name)) +
  geom_line() +
  labs(title = "Value vs. Date for Ports",
       x = "Date",
       y = "Value") 

####### Next step 1. compared the seasonal part of Mexico and Canada border and compare them with each other(just select the normal time).
#Mexico
Mexico_value <- Mexico_border %>%
  filter(Type == "People")%>%
  group_by(Month,Year)%>%
  summarize(Value = sum(Value))
Mexico_value$Date <- paste(Mexico_value$Year, Mexico_value$Month, "01", sep = "-")
Mexico_value$Date <- as.Date(Mexico_value$Date,format = "%Y-%b-%d")
Mexico_value$Date <- Mexico_value[order(Mexico_value$Date, decreasing = FALSE), ]
Mexico_value %>%
  ggplot(aes(x = Date$Date, y = Value)) +
  geom_line() +
  labs(title = "Value vs. Date",
       x = "Date",
       y = "Value") 
Mexico_time_series <- ts(data = Mexico_value$Value, frequency = 12)
Mexico_decomposed_series <- decompose(Mexico_time_series, type = "additive")
Mexico_season <- Mexico_decomposed_series$seasonal[1:12]

year <- 0
start_date <- as.Date(paste(year, "-01-01", sep = ""))
end_date <- as.Date(paste(year, "-12-31", sep = ""))
dates <- seq(start_date, end_date, by = "month")
a <- data.frame(month1 = dates, Mexico_value = Mexico_season)
a$month1 <- as.Date(a$month1)

#Canada

Canada_value <- Canada_border %>%
  filter(Type == "People")%>%
  group_by(Month,Year)%>%
  summarize(Value = sum(Value))

Canada_value$Date <- paste(Canada_value$Year, Canada_value$Month, "01", sep = "-")
Canada_value$Date <- as.Date(Canada_value$Date,format = "%Y-%b-%d")
Canada_value$Date <- Canada_value[order(Canada_value$Date, decreasing = FALSE), ]
Canada_value %>%
  ggplot(aes(x = Date$Date, y = Value)) +
  geom_line() +
  labs(title = "Value vs. Date",
       x = "Date",
       y = "Value") 
Canada_time_series <- ts(data = Canada_value$Value, frequency = 12)
Canada_decomposed_series <- decompose(Canada_time_series, type = "additive")
Canada_season <- Canada_decomposed_series$seasonal[1:12]

year <- 0
start_date <- as.Date(paste(year, "-01-01", sep = ""))
end_date <- as.Date(paste(year, "-12-31", sep = ""))
dates <- seq(start_date, end_date, by = "month")
b<- data.frame(Canada_value = Canada_season)
c<- cbind(a,b)
library(reshape2)
c_long <- melt(c, id.vars = "month1")
c_long%>%
  ggplot(aes(x = month1, y = value, color = variable)) +
  geom_line() +
  labs(x = "month", y = "Value", color = "Data Series") +
  ggtitle("Two Sets of Values on Same Plot")


####### Next step 2. compared the impact of big event(911, covid) on the value of Mexico and Canada.
# 911
start_date1 <- ymd("2000-01-01")  # Convert the start date to a Date object
end_date1 <- ymd("2003-01-01")    # Convert the end date to a Date object
date_range1 <- seq(start_date1, end_date1, by = "day")  

border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  filter(date %in% date_range1 )%>%
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>%
  filter(month == 9 & year == "2000" )

border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  filter(date %in% date_range1 )%>%
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>%
  filter(month == 9 & year == "2002" )

p1 <- border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  filter(date %in% date_range1 )%>%
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line()+
  geom_vline(xintercept=2001+(9-1)/12,color='red')+ # sep 11
  facet_wrap(~Border,scales = "free_y")+
  theme(legend.position = "none")

line1 <- data.frame(Border = c("US-Canada Border","US-Mexico Border"),wt = c(13369583,32002624))
line2 <- data.frame(Border = c("US-Canada Border","US-Mexico Border"),wt = c(10848047,28613279))
p2 <- p1 + geom_hline(aes(yintercept = wt),line1, color = 'blue')
p2 + geom_hline(aes(yintercept = wt),line2, color = 'yellow')+
  ggtitle("911")

#covid
start_date2 <- ymd("2019-01-01")  # Convert the start date to a Date object
end_date2 <- ymd("2022-01-01")    # Convert the end date to a Date object
date_range2 <- seq(start_date2, end_date2, by = "day")  

border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  filter(date %in% date_range2 )%>%
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>%
  filter(month == 3 & year == "2019")

border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  filter(date %in% date_range2 )%>%
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>%
  filter(month == 3 & year == "2021" )

p3 <- border %>% 
  mutate(date=lubridate::my(Date),
         month=month(date),
         year=year(date)) %>% 
  filter(date %in% date_range2 )%>%
  group_by(month,year,Border) %>% 
  summarize(Value=sum(Value)) %>% 
  ggplot(aes(x=year + (month-1)/12,y=Value))+
  geom_line()+
  geom_vline(xintercept=2020+(3-1)/12,color='red')+ # covid
  facet_wrap(~Border,scales = "free_y")+
  theme(legend.position = "none")

line3 <- data.frame(Border = c("US-Canada Border","US-Mexico Border"),wt = c(7098470,23889091))
line4 <- data.frame(Border = c("US-Canada Border","US-Mexico Border"),wt = c(1818274,16060812))
p4 <- p3 + geom_hline(aes(yintercept = wt),line3, color = 'blue')
p4 + geom_hline(aes(yintercept = wt),line4, color = 'yellow')+
  ggtitle("Covid")

####### Next step 3. compared 3 largest port:El Paso,Buffalo,Detroit (these 3 ports are selected by the top10 value in Canada and Mexico). I want to compare the transport proportion and trend of them
border1 %>%
  filter(Port_name =="El Paso" |Port_name =="Detroit"|Port_name =="Buffalo Niagara Falls")%>%
  filter(Type == "Object")%>%
  group_by(Port_name, Measure) %>%
  summarize(Value=sum(Value))%>%
  ggplot(aes(x = factor(Port_name),y = Value))+
  geom_bar(stat = "identity",position = position_dodge()) +
  facet_wrap(~Measure, scales = "free_y")

####### Next step 4. compared the period time that port closed.











