#The Office Show Data Analysis

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


library(tidyverse)
library(schrute)


#Which season had the most IMDB ratings


office_ratings %>% 
  group_by(season) %>% 
  summarize(average_ratings =mean(imdb_rating)) %>% 
  ggplot(aes(season,average_ratings))+
  geom_line()+
  scale_x_continuous(breaks = 1:9 )


########################################################################################
# Shrute Data analysis

transcript_data <- schrute::theoffice


transcript_data<- transcript_data %>% 
  mutate(season = as.numeric(season),
         episode= as.numeric(episode),
         title = episode_name)

transcript_data$character <- gsub("\"","",transcript_data$character)


# Find the joke Thats what she said

shesaid <- transcript_data %>% 
  filter(grepl("that's what she said",tolower(text))) %>% 
  select(index,season,episode,episode_name,character,text,title) %>% 
  mutate(category= "shesaid")


hesaid <- transcript_data %>% 
  filter(grepl("that's what he said",tolower(text))) %>% 
  select(index,season,episode,episode_name,character,text,title) %>% 
  mutate(category= "hesaid")



allsaid <- rbind(shesaid,hesaid) %>% rbind(c(NA,8,NA,NA,NA,NA,NA,NA))

theme_set(theme_light())
theme_update(
  plot.background = element_rect(fill="#ccffcc",color="#ccffcc"),
panel.background = element_blank()
  )


Character_color <- c(
  "#cc0066", # Creed 
  "#ff9933", # David .
  "#990066", # Dwight
  "#3300cc", # Holly
  "#00cc99", # Jan
  "#cc0000", # Jim
  "#330066", # Michael
  "#006666" # Pam
)


allsaid %>% 
  filter(complete.cases(.)) %>% 
  group_by(character) %>% 
  summarise(count= n()) %>% 
  ggplot()+
  geom_bar(aes(x=character,y = count,fill= character),
           stat= "identity", color="grey20",size = 0.25)+
  scale_x_discrete(limits= rev(c("Michael","Dwight","Jim","Pam","Creed","David","Holly","Jan")))+
  scale_fill_manual(values=(Character_color))+
  geom_text(aes(label= character,x= character,y=count+0.3),
            hjust = 0,family = "Chalkboard")+guides(fill=F)+
  coord_flip(ylim = c(0,31),xlim = c(0.25,8.75),expand = F)+
  labs(title = "That's What She Said Jokes Cracked by the characters in the Show")+
  theme(text = element_text(family="Chalkboard"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
  )
        







        
        







































