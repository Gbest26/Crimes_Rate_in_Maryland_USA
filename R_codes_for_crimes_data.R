library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(colorspace)
library(here)

library(readr)
crime1 <- read_csv("C:/Users/pc/Desktop/crime1.csv")
View(crime1)

glimpse(crime1)

sapply(crime1, class)
str(crime1)
crime1$time_date <- mdy_hm(crime1$Start_Date_Time)
sapply(crime1, class)
glimpse(crime1)

crime1$month <- format(as.Date(crime1$time_date), "%m")
crime1$day <- format(as.Date(crime1$time_date), "%d")
crime1$year <- format(as.Date(crime1$time_date), "%Y")
crime1$days_of_the_week <- format(as.Date(crime1$time_date), "%A")

crimes <- crime1 %>% 
  group_by(year) %>% 
  summarise(total_yearly_victims=sum(victims))
View(crimes)

ggplot(data=crimes)+
  geom_point(mapping = aes(x=year, y=total_yearly_victims, color=year))+
  labs(title = "Yearly Victims in Maryland", 
       subtitle = "Total Victim Affected by Crimes on Year Bases.")

crime001 <- crime1 %>% 
  group_by(month, year,) %>% 
  summarise(total_yearly_victims=sum(victims))
View(crime001)

crimes001 <- crime001 %>% 
  group_by(total_yearly_victims,month,year) %>% 
  summarise(year_vic=max(total_yearly_victims)) %>% 
  tail(10)
view(crimes001)

crime01 <- crime1 %>% 
  group_by(year,crime_name1, city) %>% 
  drop_na() %>% 
  summarise(total_yearly_victims=sum(victims))
view(crime01)

crimes1 <- crime01 %>% 
  group_by(total_yearly_victims,year, crime_name1,city) %>% 
  drop_na() %>% 
  summarise(highest_yearly_victims=max(total_yearly_victims)) %>% 
  tail(10)
view(crimes1)

ggplot(data = crimes1, aes(x=crime_name1, highest_yearly_victims, fill=year))+
  geom_col(position = "dodge")+
  facet_wrap(~city)+
  labs(title = "Yearly: Crimes by Cities", 
       subtitle = "CIties in Maryland with the Highest Number of Crimes")

crime02 <- crime1 %>% 
  group_by(year,crime_name2, city) %>% 
  drop_na() %>% 
  summarise(total_yearly_victims=sum(victims))
view(crime02)

crimes2 <- crime02 %>% 
  group_by(total_yearly_victims,year, crime_name2,city) %>% 
  drop_na() %>% 
  summarise(highest_yearly_victims=max(total_yearly_victims)) %>% 
  tail(10)
view(crimes2)

ggplot(data = crimes2, aes(x=crime_name2, highest_yearly_victims, fill=year))+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  facet_wrap(~city)+
  labs(title = "Yearly: Crimes by Cities", 
       subtitle = "CIties in Maryland with the Highest Number of Crimes")

crime03 <- crime1 %>% 
  group_by(year, crime_name3, city) %>% 
  drop_na() %>% 
  summarise(total_yearly_victims=sum(victims))
view(crime03)

crimes3 <- crime03 %>% 
  group_by(total_yearly_victims,year, crime_name3,city) %>% 
  drop_na() %>% 
  summarise(highest_yearly_victims=max(total_yearly_victims)) %>% 
  tail(10)
view(crimes3)

ggplot(data = crimes3, aes(x=crime_name3, highest_yearly_victims, fill=year))+
  geom_col(position = "dodge")+
  theme(axis.text.x = element_text(angle = 45))+
  facet_wrap(~city)+
  labs(title = "Yearly: Crimes by Cities", 
       subtitle = "CIties in Maryland with the Highest Number of Crimes")

ggplot(data = crime1, aes(x="", y=year,fill=committed_at_morning))+
  geom_bar(width = 1,stat = "identity", color="black")+
  coord_polar("y", start = 0)

crime_logical2 <- crime1 %>% 
  group_by(days_of_the_week) %>% 
  drop_na() %>% 
  summarise(total_crimes_committed_daily_at_morning=sum(committed_at_morning))
view(crime_logical2)

ggplot(data = crime_logical2)+
  geom_point(mapping = aes(x=days_of_the_week, y=total_crimes_committed_daily_at_morning))+
  theme(axis.text.x = element_text(angle = 50))+
  facet_wrap(~days_of_the_week)+
  labs(title = "Weekly Crimes", 
       subtitle = "Number of Crimes committed in the Morning from 2016 to 2022")

crime_logical3 <- crime1 %>% 
  group_by(month) %>% 
  drop_na() %>% 
  summarise(total_crimes_committed_daily_at_morning=sum(committed_at_morning))
view(crime_logical3)

ggplot(data = crime_logical3)+
  geom_point(mapping = aes(x=month, y=total_crimes_committed_daily_at_morning))+
  theme(axis.text.x = element_text(angle = 50))+
  facet_wrap(~month)+
  labs(title = "Total Monthly Crimes", 
       subtitle = "Number of Crimes committed in the Morning from 2016 to 2022")

crime_logical4 <- crime1 %>% 
  group_by(year) %>% 
  drop_na() %>% 
  summarise(total_crimes_committed_daily_at_morning=sum(committed_at_morning))
view(crime_logical4)

ggplot(data = crime_logical4)+
  geom_point(mapping = aes(x=year, y=total_crimes_committed_daily_at_morning))+
  theme(axis.text.x = element_text(angle = 50))+
  facet_wrap(~year)+
  labs(title = "Total Year Crimes", 
       subtitle = "Number of Crimes Committed in the morning from 2016 to 2022")

ggplot(data = crime1)+
   geom_bar(mapping = aes(x=agency, fill=year))+
   theme(axis.text.x = element_text(angle = 50))+
   facet_wrap(~committed_at_morning)+
   labs(title = "Crime Recorded by Agencies",
        subtitle = "Total Amount of Crime Recorded by Local Authorities")
  
  