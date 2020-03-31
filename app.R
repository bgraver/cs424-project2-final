library(shiny)
library(dplyr)
library(stringr)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(tidyr)
library(DT)


hurricane_df = read.csv("Atlantic_hurricane_data.csv", stringsAsFactors = FALSE)


hurricane_df = hurricane_df %>% mutate(color = case_when(status_full == 'Hurricane: Category 5' ~ '#980026',
                                                         status_full == 'Hurricane: Category 4' ~ '#c41a1c',
                                                         status_full == 'Hurricane: Category 3' ~ '#fc4e2a',
                                                         status_full == 'Hurricane: Category 2' ~ '#fd8d3c',
                                                         status_full == 'Hurricane: Category 1' ~ '#feb24c',
                                                         status_full == 'Tropical Storm' ~ '#fed976',
                                                         status_full == 'Tropical Depression' ~ '#ffffb2',
                                                         status_full == TRUE ~ 'white'))
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

  
# data.username <- as.data.frame(table(data$username))
# order.username <- order(data.username$Freq,  decreasing=TRUE)
# data.username <- data.username[order.username, ]
# colnames(data.username) <- c("Usernames", "Count")


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

pacific_df = pacific_df %>% mutate(color = case_when(status_full == 'Hurricane: Category 5' ~ '#980026',
                                                     status_full == 'Hurricane: Category 4' ~ '#c41a1c',
                                                     status_full == 'Hurricane: Category 3' ~ '#fc4e2a',
                                                     status_full == 'Hurricane: Category 2' ~ '#fd8d3c',
                                                     status_full == 'Hurricane: Category 1' ~ '#feb24c',
                                                     status_full == 'Tropical Storm' ~ '#fed976',
                                                     status_full == 'Tropical Depression' ~ '#ffffb2',
                                                     status_full == TRUE ~ 'white'))

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

pacific_df['p_popup_string'] = mapply(p_popup_string, pacific_df$name, pacific_df$storm_num, 
                                      pacific_df$status_full, pacific_df$maxWindSpeed, pacific_df$minWindPressure, pacific_df$date_time)

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2: Against the Wind"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("Info", tabName = 'Info'),
                     
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     
                     menuItem("Map", tabName = 'Map'),
                     menuItem("Pacific Map", tabName = 'PacificMap'),
                     
                     conditionalPanel(
                       condition = 'input["tabs"] != "cheapBlankSpace"',
                       radioButtons("radio", label = "Sort by: ",
                                    choices = list("Atlantic Year" = 1, "Atlantic Name" = 2, "Atl. Max Wind Speed" = 5,
                                                   "Atl. Min Wind Pressure" = 6,
                                                   "Pacific Year" = 3, "Pacific Name" = 4,
                                                   "Pac. Max Wind Speed" = 7, "PAc. Min Wind Pressure" = 8), 
                                    selected = 1), 
                       uiOutput("menu_select"),
                       actionButton("clear", "clear")
                     )
                   )
                   
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Map',
              fluidRow(
                column(9,
                       box(title="Hurricane Map", solidHeader=TRUE, status="primary", width=12,
                           leafletOutput("my_map", height=800))
                ),
                column(3, 
                       box(title="Category Graph", solidHeader=TRUE, status="primary", width=12,
                           plotOutput("cat_graph", height=800))
                       )
              ) 
              
              # fluidPage(
              #   box(title="Total count",  solidHeader=TRUE, status="primary", width=12,
              #       textOutput("text"))
              # )
              # fillPage(
              #   title = 'Hurricane Map',
              #   leafletOutput("my_map", height = 700),
              #   
              # 
              
      ),
      tabItem(tabName = 'PacificMap',
              fluidRow(
                column(9,
                       box(title="Hurricane Map", solidHeader=TRUE, status="primary", width=12,
                           leafletOutput("my_p_map", height=800))
                ),
                column(3, 
                       box(title="Category Graph", solidHeader=TRUE, status="primary", width=12,
                           plotOutput("p_cat_graph", height=800))
                )
              ) 
              
              
              
              # fillPage(
              #   title = 'Hurricane Map',
              #   leafletOutput("my_p_map", height = 700)
              # )
      ),
      tabItem(tabName = 'Info',
              fillPage(
                title = 'Hurricane Info',
                box(title="About", solidHeader=TRUE,  status="primary", width=12,
                    htmlOutput("about", height=200))
              )
      )
      
    )
  )
)

server <- function(input, output) {
  output$menu_choice <- renderPrint({ input$menuInput })
  
  output$about <- renderUI({
    str0 <- paste("- Dashboard made by Brandon Graver, Nicholas Abassi, and Ho Chon.")
    str1 <- paste("- Libraries used: Shiny, Shinydashboard, leaflet, dplyr, ggplot2, stringr, tidyr")
    str2 <- paste("- Data comes from http://www.nhc.noaa.gov/data/#hurdat.")
    HTML(paste(str0, str1, str2, sep='<br>'))
  })
  
  output$my_map <- renderLeaflet({
    map = leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(-70, 30, zoom = 4)
  })
  
  output$my_p_map <- renderLeaflet({
    map = leaflet() %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setView(-120, 20, zoom = 4)
  })
  
  output$cat_graph <- renderPlot({
    g <- ggplot(hurricane_df, aes(x=hurricane_df$graph_status)) + 
      geom_bar(fill="#003366") +
      xlab("Types of Hurricanes") + 
      ylab("Count") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g 
  })
  
  output$p_cat_graph <- renderPlot({
    g <- ggplot(pacific_df, aes(x=pacific_df$graph_status)) + 
      geom_bar(fill="#003366") +
      xlab("Types of Hurricanes") + 
      ylab("Count") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g 
  })
  
  val = reactiveValues(cur_df = NULL, 
                       curFilter = 'year',
                       curList = yearList,
                       curMap = 1,
                       clear = FALSE)
  
  filter_helper = function(){
    if(val$curFilter == 'year') return('year %in% input$menuInput')
    else if(val$curFilter == 'maxWind') return('maxWindSpeed %in% input$menuInput')
    else if(val$curFilter == 'minPressure') return('minWindPressure %in% input$menuInput')
    else {
      ret_list = list()
      for(cur_input in input$menuInput) {
        match_string = str_match(cur_input, '(\\w+) \\((\\d+)')
        filter_term = paste0('(name == \'', match_string[2], 
                             '\' & year == ', match_string[3], ')')
        ret_list  = append(ret_list, filter_term) 
      }
      return(paste(ret_list, collapse = ' | '))
    }
  }
  
  render_menu_select = reactive({
    if (input$radio == 1) {
      val$curList = yearList
      inputString = 'Select Year'
    }
    if (input$radio == 2) {
      val$curList = nameList
      inputString = 'Select Name'
    }
    if (input$radio == 3) {
      val$curList = p_yearList
      inputString = 'Select Year'
    }
    if (input$radio == 4) {
      val$curList = p_nameList
      inputString = 'Select Name'
    }
    if (input$radio == 5) {
      val$curList = maxList
      inputString = 'Select Max Speed'
    }
    if (input$radio == 6) {
      val$curList = minList
      inputString = 'Select Min Pressure'
    }
    if (input$radio == 7) {
      val$curList = p_maxList
      inputString = 'Select Max Speed'
    }
    if (input$radio == 8) {
      val$curList = p_minList
      inputString = 'Select Min Pressure'
    }
    output$menu_select = renderUI({
      selectizeInput("menuInput", 
                     inputString, choices = val$curList)
    }) 
  })
  
  observeEvent(input$radio, {
    val$curFilter = case_when(input$radio == 1 ~ 'year',
                              input$radio == 2 ~ 'name',
                              input$radio == 3 ~ 'year',
                              input$radio == 4 ~ 'name',
                              input$radio == 5 ~ 'maxWind',
                              input$radio == 6 ~ 'minPressure',
                              input$radio == 7 ~ 'maxWind',
                              input$radio == 8 ~ 'minPressure')
    render_menu_select()
  })
  
  render_map_proxy = reactive({
    if(input$radio == 1){
      proxy = leafletProxy('my_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$storm_num)){
          temp_df = val$cur_df %>% filter(eval(as.name(val$curFilter)) == i & storm_num == j) 
          proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude) %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~popup_string, fillOpacity = 2.0)
        }
      }
    } 
    
    else if(input$radio == 2){
      proxy = leafletProxy('my_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      temp_df = val$cur_df %>% mutate(new_name = paste0(name, year))
      for(i in unique(temp_df$new_name)) {
        temp2_df = temp_df %>% filter(new_name == i)
        proxy %>% addPolylines(data=temp2_df,lng = ~longitude, lat = ~latitude) %>%
          addCircles(data = temp2_df, ~longitude, ~latitude, color = ~color, 
                     radius = ~maxWindSpeed * 300, popup = ~popup_string, fillOpacity = 2.0)
      }
    }
    
    else if(input$radio == 3){
      proxy = leafletProxy('my_p_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      val$cur_df = pacific_df
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$storm_num)){
          temp_df = val$cur_df %>% filter(eval(as.name(val$curFilter)) == i & storm_num == j) 
          proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude) %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~p_popup_string, fillOpacity = 2.0)
        }
      }
    }
    
    else if (input$radio == 4) {
      proxy = leafletProxy('my_p_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      val$cur_df = pacific_df
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$storm_num)){
          temp_df = val$cur_df %>% filter(eval(as.name(val$curFilter)) == i & storm_num == j) 
          proxy %>% addPolylines(data=temp_df,lng = ~longitude, lat = ~latitude) %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~p_popup_string, fillOpacity = 2.0)
        }
      }
    }
    
    else if(input$radio == 5){
      proxy = leafletProxy('my_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$maxWindSpeed)){
          temp_df = val$cur_df %>% filter(input$menuInput == i & maxWindSpeed == j) 
          proxy  %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~popup_string, fillOpacity = 2.0)
        }
      }
    } 
    
    else if(input$radio == 6){
      proxy = leafletProxy('my_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$minWindPressure)){
          temp_df = val$cur_df %>% filter(input$menuInput == i & minWindPressure == j) 
          proxy  %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~popup_string, fillOpacity = 2.0)
        }
      }
    }
    
    else if(input$radio == 7){
      proxy = leafletProxy('my_p_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$maxWindSpeed)){
          temp_df = val$cur_df %>% filter(input$menuInput == i & maxWindSpeed == j) 
          proxy %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~p_popup_string, fillOpacity = 2.0)
        }
      }
    } 
    
    else if(input$radio == 8){
      proxy = leafletProxy('my_p_map')
      
      clearShapes(proxy)
      
      val$groupList = list()
      
      for(i in input$menuInput){
        for(j in unique(val$cur_df$minWindPressure)){
          temp_df = val$cur_df %>% filter(input$menuInput == i & minWindPressure == j) 
          proxy %>%
            addCircles(data = temp_df, ~longitude, ~latitude, color = ~color, 
                       radius = ~maxWindSpeed * 300, popup = ~p_popup_string, fillOpacity = 2.0)
        }
      }
    }
    
  })
  
  observeEvent(input$radio, {
    render_menu_select()
    val$cur_df = NULL
    val$curMap = input$radio
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == 'Map') {
      render_map_proxy()
    }
    else if (input$tabs == 'PacificMap') {
      render_map_proxy()
    }
  })
  
  observeEvent(input$menuInput,{
    output$menu_items <- renderPrint({ input$menuInput })
    val$cur_input = input$menuInput
    
    if(input$radio == 1){
      val$cur_df = hurricane_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    
    else if(input$radio == 2){
      val$cur_df = hurricane_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    
    else if(input$radio == 3){
      val$cur_df = pacific_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_p_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    else if(input$radio == 4) {
      val$cur_df = pacific_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_p_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    else if (input$radio == 5) {
      val$cur_df = hurricane_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    else if (input$radio == 6) {
      val$cur_df = hurricane_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    else if (input$radio == 7) {
      val$cur_df = pacific_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_p_map')
      clearShapes(proxy)
      render_map_proxy()
    }
    else if (input$radio == 8) {
      val$cur_df = pacific_df %>%
        filter_(filter_helper())
      proxy = leafletProxy('my_p_map')
      clearShapes(proxy)
      render_map_proxy()
    }
  })
  
  observeEvent(input$clear, {
    val$clear = input$clear
    render_menu_select()
    proxy = leafletProxy('my_map')
    clearShapes(proxy)
    proxy2 = leafletProxy('my_p_map')
    clearShapes(proxy2)
  })
}

shinyApp(ui = ui, server = server)


