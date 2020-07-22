# -- libraries----
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyverse")
# -- libraries----
library(dplyr)
library(plyr)
library (ggplot2)
library(tidyverse)

# -- locations of interest----
counties=data.frame(county=c("Denver", "Boulder", "El Paso", 
                              "New York City","King", "Fulton", 
                             "District of Columbia", "Knox", "Davidson"),
                    state=c('Colorado','Colorado','Colorado', 
                             'New York', 'Washington', 'Georgia',
                            "District of Columbia", "Tennessee", "Tennessee"))

# -- data sources----
cases=read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

pops=read.csv('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv')

#to add new locations may need to check how formatted in the census vs nytimes
# and further adjust

pops=pops%>%mutate(
  county=
    case_when(CTYNAME=='District of Columbia' ~ 'District of Columbia',
              CTYNAME=='New York'  ~ 'New York City',
              TRUE ~ gsub(' County$', '',CTYNAME)))

# -- plotting----
quartz()

cases%>%
  right_join(., counties)%>%
  #filter(county%in%counties)%>%
  left_join(., pops%>%select('county', 'STNAME', 'POPESTIMATE2019')%>%
              distinct(),
            by=c('county'="county", 'state'='STNAME'))%>%
  group_by(county)%>%
  mutate(date=lubridate::ymd(date))%>%
  arrange(date)%>%
  mutate(new_cases=c(NA, diff(cases)))%>%
  mutate(new_cases_per_cap=new_cases/POPESTIMATE2019)%>%
  mutate(new_deaths=c(NA, diff(deaths)))%>%
  mutate(new_deaths_per_cap=new_deaths/POPESTIMATE2019)%>%
  
  
# New Deaths Per Capita
  ggplot(data=., aes(x=date, y=new_cases))+
  geom_point(alpha=0.5, color='red')+
  geom_smooth()+
  facet_wrap(~county, scales='free_y')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(limits = c(Sys.Date() - 100, NA))

#############################################################################################
states<-c("Tennessee", "Colorado", "New York", "California", "Washington", "Georgia")

head(cases)
cases$ID<-paste(cases$date, cases$state, sep="_")
head(cases)

stateCases<-ddply(cases,.(ID), summarize,
                  Cases=sum(cases),
                  Deaths=sum(deaths))
head(stateCases)
StateCases<-separate(stateCases, ID, c("date", "state"), sep = "_", remove = TRUE)

ggplot(data=StateCases, aes(x=as.Date(date), y=Cases))+
  geom_point(alpha=0.5, color='red')+
  geom_smooth()+
  facet_wrap(~state, scales='free_y')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(limits = c(Sys.Date() - 50, NA))
str(StateCases)


cases%>%
  right_join(., states)%>%
  #filter(county%in%counties)%>%
  left_join(., pops%>%select('STNAME', 'POPESTIMATE2019')%>%
              distinct(),
            by=c('state'='STNAME'))%>%
  group_by(state)%>%
  mutate(date=lubridate::ymd(date))%>%
  arrange(date)%>%
  mutate(new_cases=c(NA, diff(cases)))%>%
  mutate(new_cases_per_cap=new_cases/POPESTIMATE2019)%>%
  mutate(new_deaths=c(NA, diff(deaths)))%>%
  mutate(new_deaths_per_cap=new_deaths/POPESTIMATE2019)%>%
  
  # New Deaths Per Capita
  ggplot(data=., aes(x=date, y=new_cases))+
  geom_point(alpha=0.5, color='red')+
  geom_smooth()+
  facet_wrap(~state, scales='free_y')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(limits = c(Sys.Date() - 50, NA))

