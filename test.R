library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(DT)



# hurricane_df = read.csv("Atlantic_hurricane_data.csv", stringsAsFactors = FALSE)

hurricane_df = Atlantic_hurricane_data
pacific_df = Pacific_hurricane_data

hurricane_df = hurricane_df %>% mutate(color = case_when(status_full == 'Hurricane: Category 5' ~ '#b10026',
                                                         status_full == 'Hurricane: Category 4' ~ '#e31a1c',
                                                         status_full == 'Hurricane: Category 3' ~ '#fc4e2a',
                                                         status_full == 'Hurricane: Category 2' ~ '#fd8d3c',
                                                         status_full == 'Hurricane: Category 1' ~ '#feb24c',
                                                         status_full == 'Tropical Storm' ~ '#fed976',
                                                         status_full == 'Tropical Depression' ~ '#ffffb2'))

# summary(hurricane_df)
hurricane_df$graph_status <- case_when(
  hurricane_df$status_full == "Hurricane: Category 1" ~ "Hurricane: Category 1",
  hurricane_df$status_full == "Hurricane: Category 2" ~ "Hurricane: Category 2",
  hurricane_df$status_full == "Hurricane: Category 3" ~ "Hurricane: Category 3",
  hurricane_df$status_full == "Hurricane: Category 4" ~ "Hurricane: Category 4",
  hurricane_df$status_full == "Hurricane: Category 5" ~ "Hurricane: Category 5",
  hurricane_df$status_full == "Tropical Storm" ~ "Tropical Storm",
  hurricane_df$status_full == "Tropical Depression" ~ "Tropical Depression",
  TRUE ~ "Other"
)

pacific_df$graph_status <- case_when(
  pacific_df$status_full == "Hurricane: Category 1" ~ "Hurricane: Category 1",
  pacific_df$status_full == "Hurricane: Category 2" ~ "Hurricane: Category 2",
  pacific_df$status_full == "Hurricane: Category 3" ~ "Hurricane: Category 3",
  pacific_df$status_full == "Hurricane: Category 4" ~ "Hurricane: Category 4",
  pacific_df$status_full == "Hurricane: Category 5" ~ "Hurricane: Category 5",
  pacific_df$status_full == "Tropical Storm" ~ "Tropical Storm",
  pacific_df$status_full == "Tropical Depression" ~ "Tropical Depression",
  TRUE ~ "Other"
)



yearList = 2005:2018

nameList = (hurricane_df %>% 
              filter(year > 2005) %>%
              group_by(year, storm_num)  %>% 
              select(name, year, storm_num) %>% 
              distinct() %>% 
              summarise(new_name = if_else(name == 'Unnamed', 
                                           paste0('Unnamed:', storm_num, ' (', year, ')'), 
                                           paste0(name, ' (', year,')' ))) %>% 
              ungroup() %>%
              distinct() %>%
              select(new_name))[[1]]

nameList = sort(nameList)

maxList = (hurricane_df %>%
             filter(year > 2005) %>%
             group_by(year, storm_num) %>%
             select(maxWindSpeed, year, storm_num))[[1]]
maxList = sort(maxList, decreasing=TRUE)

minList = (hurricane_df %>%
             filter(year > 2005) %>%
             group_by(year, storm_num) %>%
             select(minWindPressure, year, storm_num))[[1]]
minList = sort(minList, decreasing=TRUE)


popup_string = function(name, storm_num, status_full, maxWindSpeed, minWindPressure, date_time){
  if(name == 'Unnamed'){
    name = paste(name,storm_num, sep = ':')
  }
  
  val = paste0('Name = ', as.character(name),'<br>status = ', status_full, '<br>', date_time)
  
  if(!is.na(maxWindSpeed)){
    val = paste0(val, '<br>Max Wind Speed = ', maxWindSpeed)
  }
  
  if(!is.na(minWindPressure)){
    val = paste0(val, '<br>Min Wind Pressure = ', minWindPressure)
  }
  return(val)
}

hurricane_df['popup_string'] = mapply(popup_string, hurricane_df$name, hurricane_df$storm_num, 
                                      hurricane_df$status_full, hurricane_df$maxWindSpeed, hurricane_df$minWindPressure, hurricane_df$date_time)

pacific_df = read.csv("Pacific_hurricane_data.csv", stringsAsFactors = FALSE)

pacific_df = pacific_df %>% mutate(color = case_when(status_full == 'Hurricane: Category 5' ~ 'red4',
                                                     status_full == 'Hurricane: Category 4' ~ 'red3',
                                                     status_full == 'Hurricane: Category 3' ~ 'red2',
                                                     status_full == 'Hurricane: Category 2' ~ 'red1',
                                                     status_full == 'Hurricane: Category 1' ~ 'red',
                                                     status_full == 'Tropical Storm' ~ 'orange',
                                                     status_full == 'Tropical Depression' ~ 'yellow'))

p_yearList = 2005:2016

p_nameList = (pacific_df %>%
                group_by(year, storm_num)  %>%
                select(name, year, storm_num))[[1]]

p_nameList = sort(p_nameList)

p_maxList = (pacific_df %>%
               group_by(year, storm_num) %>%
               select(maxWindSpeed, year, storm_num))[[1]]
p_maxList = sort(p_maxList, decreasing=TRUE)

p_minList = (pacific_df %>%
               group_by(year, storm_num) %>%
               select(minWindPressure, year, storm_num))[[1]]
p_minList = sort(p_minList, decreasing=TRUE)


p_popup_string = function(name, storm_num, status_full, maxWindSpeed, minWindPressure, date_time){
  val = paste0('Name = ', as.character(name),'<br>status = ', status_full, '<br>', date_time)
  
  if(!is.na(maxWindSpeed)){
    val = paste0(val, '<br>Max Wind Speed = ', maxWindSpeed)
  }
  
  if(!is.na(minWindPressure)){
    val = paste0(val, '<br>Min Wind Pressure = ', minWindPressure)
  }
  return(val)
}


## num of categories of hurricanes
g <- ggplot(hurricane_df, aes(x=hurricane_df$graph_status)) + 
  geom_bar(fill="#003366") +
  xlab("Types of Hurricanes") + 
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
g 
