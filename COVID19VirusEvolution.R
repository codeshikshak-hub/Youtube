#Corona Virus cases over the time using GGanimate:John Hopkins data set

library(gganimate)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gifski)
library(av)



#  Load data from John Hopkins Github repository

confirmedCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deathCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
recoveredCases= read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')


confirmedCases<- confirmedCases %>% select(-c(Lat,Long)) %>% melt(id=c('Country/Region','Province/State'))
confirmedCases<- confirmedCases %>% group_by(`Country/Region`,variable) %>% summarise(Confirmed=sum(value))

deathCases<-deathCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
deathCases<-deathCases%>%group_by(`Country/Region`,variable)%>%summarise(Deaths=sum(value))

recoveredCases<-recoveredCases%>%select(-c(Lat,Long))%>%melt(id=c('Country/Region','Province/State'))
recoveredCases<-recoveredCases%>%group_by(`Country/Region`,variable)%>%summarise(Recovered=sum(value))


# rename columns
colnames(confirmedCases) <- c("Country","Date","Confirmed")
colnames(deathCases) <- c("Country","Date","Death")
colnames(recoveredCases) <- c("Country","Date","Recovered")



# merge all the tables

mergedCases <- merge(confirmedCases, deathCases,by.y=c("Country","Date"))

mergedCases <- merge(mergedCases, recoveredCases,by.y=c("Country","Date"))


# format date
mergedCases$Date <- as.Date(mergedCases$Date,"%m/%d/%y")


# Summarise cases by date
df1 <- mergedCases %>% group_by(Date) %>% summarise_at(c("Confirmed","Recovered","Death"),sum)

dff<- as.data.frame(rbind(c(0,0,0),apply(df1[-1],2,diff)))
colnames(dff)<- c("New","New recovered","New Death")



df1 <- cbind(df1,dff)


# stack columns

df2 <- data.frame(Date = rep(df1$Date,4),
                  cases= c(df1$Confirmed,df1$Death, df1$Recovered,df1$New),
                  State = rep(c("Confirmed","Deaths","Recovered","New"),each = nrow(df1)))



#Retrive
lastDate <- max(df1$Date)


#Define the plot

plot <- ggplot(df2,aes(x= Date,y= cases,group=State,color= State))+
        geom_line()+
  geom_segment(aes(xend = max(Date),yend= cases),linetype = 2,color = 'blue')+
  geom_point(size=3)+
  geom_text(aes(x= max(Date)+0.1,label = sprintf("%5.0f",cases,hjust=-0.5)))+
  transition_reveal(Date)+
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') + 
  xlab("Day") +
  ylab("Number of cumulative cases") + ggtitle(paste("Evolution of COVID cases over time as of ",lastDate)) +
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.8))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(5.5, 40, 5.5, 5.5))


# Create animation gif file

animate(plot, fps = 5, renderer = av_renderer('COVIDcasesEvolution.mp4'))














































