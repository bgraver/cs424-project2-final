library(stringr)
library(lubridate)
library(dplyr)

headerLines = read.delim("hurdat2-1851-2018-120319.txt")

parse_df = data.frame(name = character(),
                         storm_num = numeric(),
                         year = numeric(),
                         date_time = double(),
                         status = character(),
                         status_full = character(),
                         longitude = numeric(),
                         latitude = numeric(),
                         maxWindSpeed = numeric(),
                         minWindPressure = numeric(),
                         NEQuadrant1 = numeric(),
                         SEQuadrant1 = numeric(),
                         SWQuadrant1 = numeric(),
                         NWQuadrant1 = numeric(),
                         NEQuadrant2 = numeric(),
                         SEQuadrant2 = numeric(),
                         SWQuadrant2 = numeric(),
                         NWQuadrant2 = numeric(),
                         NEQuadrant3 = numeric(),
                         SEQuadrant3 = numeric(),
                         SWQuadrant3 = numeric(),
                         NWQuadrant3 = numeric(),
                         stringsAsFactors=FALSE)

i = 1
count = 1
while (i <= length(headerLines)) {
  curline = headerLines[i]
  if (grepl('^AL', curline)){
    match_string = str_match(curline, '^AL(\\d{2})\\d{4},\\s+(\\S+),\\s+(\\d+)')
    storm_num = match_string[2]
    name = paste0(substring(match_string[3], 1,1), tolower(substring(match_string[3],2)))
    num_rows = as.numeric(match_string[4])
    
    for (j in (i+1):(i+num_rows)) {
      curline = headerLines[j]
      #date
      match_string = str_match(curline, '^(\\d{4})(\\d{2})(\\d{2}), (\\d{2})(\\d{2}),')
      year = match_string[2]
      month = match_string[3]
      day = match_string[4]
      hour = match_string[5]
      minute = match_string[6]
      time = str_interp('${month}/${day}/${year} ${hour}:${minute}')
      
      #status, longitude, latitude, max wind speed, min wind pressure
      match_string = str_match(curline, ', (\\w{2}),\\s+(\\S+),\\s+(\\S+),\\s+(-?\\d+),\\s+(-?\\d+),')
      status = match_string[2]
      lat_sign = substr(match_string[3],nchar(match_string[3]),nchar(match_string[3]))
      lat = substr(match_string[3],1,nchar(match_string[3])-1)
      latitude = ifelse(lat_sign == 'N', as.numeric(lat), as.numeric(lat)*-1)
      long_sign = substr(match_string[4],nchar(match_string[4]),nchar(match_string[4]))
      long = substr(match_string[4],1,nchar(match_string[4])-1)
      longitude = ifelse(long_sign == 'E', as.numeric(long), as.numeric(long)*-1)
      maxWindSpeed = ifelse(match_string[5] == -99, NaN, as.numeric(match_string[5]))
      minWindPressure = ifelse(match_string[6] == '-999', NaN, as.numeric(match_string[6]))
      status_full = case_when(status == 'HU' ~ paste0('Hurricane: ',
                                                      case_when(maxWindSpeed >= 137 ~ 'Category 5',
                                                               (maxWindSpeed >= 113 & maxWindSpeed <= 136) ~ 'Category 4',
                                                               (maxWindSpeed >=  96 & maxWindSpeed <= 112) ~ 'Category 3',
                                                               (maxWindSpeed >=  83 & maxWindSpeed <=  95) ~ 'Category 2',
                                                               (maxWindSpeed >=  64 & maxWindSpeed <=  82) ~ 'Category 1')), 
                                   status == 'TS' ~ 'Tropical Storm', 
                                   status == 'TD' ~ 'Tropical Depression',
                                   TRUE ~ paste0('Other:', status))
      
      #Quadrant
      match_string = str_match(curline, ',\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s+(-?\\d+),\\s*$')
      
      NEQuadrant1 = ifelse(match_string[2] == '-999', NaN, as.numeric(match_string[2]))
      SEQuadrant1 = ifelse(match_string[3] == '-999', NaN, as.numeric(match_string[3]))
      SWQuadrant1 = ifelse(match_string[4] == '-999', NaN, as.numeric(match_string[4]))
      NWQuadrant1 = ifelse(match_string[5] == '-999', NaN, as.numeric(match_string[5]))
      
      NEQuadrant2 = ifelse(match_string[6] == '-999', NaN, as.numeric(match_string[6]))
      SEQuadrant2 = ifelse(match_string[7] == '-999', NaN, as.numeric(match_string[7]))
      SWQuadrant2 = ifelse(match_string[8] == '-999', NaN, as.numeric(match_string[8]))
      NWQuadrant2 = ifelse(match_string[9] == '-999', NaN, as.numeric(match_string[9]))
      
      NEQuadrant3 = ifelse(match_string[10] == '-999', NaN, as.numeric(match_string[10]))
      SEQuadrant3 = ifelse(match_string[11] == '-999', NaN, as.numeric(match_string[11]))
      SWQuadrant3 = ifelse(match_string[12] == '-999', NaN, as.numeric(match_string[12]))
      NWQuadrant3 = ifelse(match_string[13] == '-999', NaN, as.numeric(match_string[13]))
      
      temp_df = data.frame(name = name,
                           year = as.numeric(year),
                           storm_num = as.numeric(storm_num),
                           date_time = strptime(time, '%m/%d/%Y %H:%M', tz='UTC'),
                           status = status,
                           status_full = status_full,
                           longitude = longitude,
                           latitude = latitude,
                           maxWindSpeed = maxWindSpeed,
                           minWindPressure = minWindPressure,
                           NEQuadrant1 = NEQuadrant1,
                           SEQuadrant1 = SEQuadrant1,
                           SWQuadrant1 = SWQuadrant1,
                           NWQuadrant1 = NWQuadrant1,
                           NEQuadrant2 = NEQuadrant2,
                           SEQuadrant2 = SEQuadrant2,
                           SWQuadrant2 = SWQuadrant2,
                           NWQuadrant2 = NWQuadrant2,
                           NEQuadrant3 = NEQuadrant3,
                           SEQuadrant3 = SEQuadrant3,
                           SWQuadrant3 = SWQuadrant3,
                           NWQuadrant3 = NWQuadrant3,
                           stringsAsFactors = FALSE)
      
      parse_df = rbind(parse_df, temp_df)
    }
  }
}

maxStatus = function(status, maxWindSpeed) {
  if ('HU' %in% status) {
    stormSpeed = max(maxWindSpeed)
    val = case_when(stormSpeed >= 137 ~ 'Hurricane: Category 5',
                        (stormSpeed >= 113 & stormSpeed <= 136) ~ 'Hurricane: Category 4',
                        (stormSpeed >=  96 & stormSpeed <= 112) ~ 'Hurricane: Category 3',
                        (stormSpeed >=  83 & stormSpeed <=  95) ~ 'Hurricane: Category 2',
                        (stormSpeed >=  64 & stormSpeed <=  82) ~ 'Hurricane: Category 1')
  } 
  else if ('TS' %in% status) val = 'Tropical Storm'
  else val = 'Tropical Depression'
  return(val)
}

parse_df = parse_df %>% 
  group_by(year, storm_num) %>%
  mutate(max_status = maxStatus(status, maxWindSpeed))

write.csv(parse_df,file='Atlantic_hurricane_data.csv',row.names = FALSE)
