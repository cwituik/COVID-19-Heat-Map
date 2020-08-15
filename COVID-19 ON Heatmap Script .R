#ON COVID-19 Heatmap----

#Import data and load packages
data2 <- read.csv(file.choose())


library(dplyr)
library(runner)
library(zoo)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)


#Clean data, remove observatioons missing case report dates and age groups
ON2 <- data2 %>%
  select(Case_Reported_Date, Age_Group)%>%
  drop_na()%>%
  filter(Age_Group!="UNKNOWN" & Case_Reported_Date!="" & Age_Group!="")%>%
  group_by(Case_Reported_Date, Age_Group)%>%
  summarise(count=n())%>%
  drop_na()

class(ON2$Case_Reported_Date)
ON2$Case_Reported_Date <- str_sub(ON2$Case_Reported_Date, end=-10)
ON2$Case_Reported_Date <- ymd(ON2$Case_Reported_Date) #convert to date format 


#calculate rolling 7 day avg by age group 
ON3 <- ON2%>%
  arrange(desc(Age_Group))%>%
  group_by(Age_Group)%>%
  complete(Case_Reported_Date = seq.Date(min(Case_Reported_Date), as.Date("2020-08-15"), by = "day"), fill = list(count = 0))%>%
  mutate(cases_7da = rollmean(count, k=7, fill=NA, align = "right", pad.na=T))%>%
  group_by(Age_Group)%>%
  complete(cases_7da)

#subset min date 
ON4 <- ON3%>%
  subset(Case_Reported_Date>"2020-03-24")

#set date limits for heat map 
lims <- ymd(c("2020-03-26", "2020-08-15"))

#plot
ggplot(ON4, aes(x=Case_Reported_Date, y=Age_Group, fill=cases_7da))+
  geom_tile()+
  scale_fill_viridis_c()+
  coord_fixed(ratio = 7)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust=1))+
  scale_x_date(date_breaks = "1 week", limits =lims, expand = c(0, 0))+
  labs(x="Week", 
       y="Age Group",
       title = "Heatmap of COVID-19 cases in Ontario by age group over time",
       fill="Daily reported cases
(rolling average)",
       caption = "Notes: Average daily reported cases on a rolling 7 day basis
       Updated: Aug 15, 2020
       Source: https://data.ontario.ca/dataset/confirmed-positive-cases-of-covid-19-in-ontario")

#Enjoy!!!