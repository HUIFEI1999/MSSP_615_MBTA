library(readr)
library(tidyverse)
library(magrittr)
library(plotly)
library(ggplot2)
library(leaflet)
library(shiny)
# 
# 
# stop <- read_csv("stops.txt")
# r1 <- read_csv("HRTravelTimesQ4_21.csv")
# l1 <- read_csv("LRTravelTimesQ4_21.csv")
# 
# 
# 
# # table for number of station
# 
# ## unique stop id
# 
# station <- rbind(select(r1,c(3,4)),select(l1,c(3,4))) %>% unique()
# 
# 
# n_station <- station %>% count(route_id)
# 
# mycolor = c("blue","chartreuse","chartreuse1","chartreuse2","chartreuse3","cyan","orange","red")
# 
# his_station <- barplot(height=n_station$n, names=n_station$route_id, col=mycolor,
#                        xlab="Main Subway Lines", 
#                        ylab="Counts", 
#                        main="Count of stations per MBTA line", 
#                        ylim=c(0,55) )
# 
# nacol <- c("stop_id","stop_lat","stop_lon","platform_name")
# mstop <-select(stop,nacol)
# 
# b <- as.character(station$to_stop_id)
# ans = stop[which(mstop$stop_id ==b[1]),]
# 
# for (i in 1:length(station$to_stop_id)){
#   mt <- stop[which(mstop$stop_id ==b[i]),]
#   ans = rbind(mt,ans)
# }
# 
# mt <- select(ans[1:length(station$to_stop_id),],nacol)
# mt$stop_id <- as.numeric(mt$stop_id)
# mt <- mt[order(mt$stop_id),]
# mt$line <- as.factor(station$route_id)
# 
# 
# theme_set(theme_bw())
# world <- ne_countries(scale = "medium", returnclass = "sf")
# counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
# counties <- subset(counties, grepl("massachusetts", counties$ID))
# counties$area <- as.numeric(st_area(counties))
# 
# p4<- ggplot(data = world) +
#   geom_sf(fill = "cornsilk") +
#   
#   geom_sf(data = counties, fill = NA, color = gray(.5)) +
#   
#   geom_point(data = mt, aes(x = stop_lon, y = stop_lat,color = line), size = 0.65
#   ) +
#   
#   annotate(geom = "text", x = -71.1, y = 42.16, label = "Norfalk", 
#            color = "grey22", size = 4.5) +
#   
#   annotate(geom = "text", x = -71.15, y = 42.45, label = "Middlesex", 
#            color = "grey22", size = 4.5) +
#   
#   annotate(geom = "text", x = -70.98, y = 42.47, label = "Essex", 
#            color = "grey22", size = 4.5) +
#   
#   annotate(geom = "text", x = -70.88, y = 42.14, label = "Plymouth", 
#            color = "grey22", size = 4.5) +
#   
#   
#   coord_sf(xlim = c(-71.3, -70.8), ylim = c(42.1, 42.5), expand = FALSE) +
#   scale_color_manual(values=mycolor)
# 
# p5
# 
# ggplot(data = world) +
#   geom_sf(fill = "cornsilk") +
#   
#   geom_sf(data = counties, fill = NA, color = gray(.5)) +
#   
#   geom_point(data = mt[mt$line == "Blue",], aes(x = stop_lon, y = stop_lat), size = 0.65, 
#              color = "blue") +
#   geom_line(data = mt[mt$line == "Blue",],aes(x = stop_lon,y = stop_lat),color = "blue",alpha = 0.6) +
#   
#   geom_point(data = mt[mt$line == "Orange",], aes(x = stop_lon, y = stop_lat), size = 0.65, 
#              color = "orange")+
#   # geom_line(data = mt[mt$line == "Orange",],aes(x = stop_lon,y = stop_lat),color = "orange") +
#   
#   
#   geom_point(data = mt[mt$line == "Red",], aes(x = stop_lon, y = stop_lat), size = 0.65, 
#              color = "red") +
#   # geom_line(data = mt[mt$line == "Red",],aes(x = stop_lon,y = stop_lat),color = "red") +
#   
#   annotate(geom = "text", x = -71.1, y = 42.16, label = "Norfalk", 
#            color = "grey22", size = 4.5) +
#   
#   annotate(geom = "text", x = -71.15, y = 42.45, label = "Middlesex", 
#            color = "grey22", size = 4.5) +
#   
#   annotate(geom = "text", x = -70.98, y = 42.47, label = "Essex", 
#            color = "grey22", size = 4.5) +
#   
#   annotate(geom = "text", x = -70.88, y = 42.14, label = "Plymouth", 
#            color = "grey22", size = 4.5) +
#   
#   
#   coord_sf(xlim = c(-71.3, -70.8), ylim = c(42.1, 42.5), expand = FALSE)
# 
# # rest

pacman::p_load('shiny','shinythemes','tidyverse','leaflet','tigris','sf')

ferry <- read_csv("Ferry_stop_time.csv")
stations <- unique(ferry$arrival_terminal)

c1 <- c("station","station","station","station","station")

ui <- fluidPage(
  navbarPage(theme = shinytheme('lumen'), title = 'MBTA',

             tabPanel("map",
                      
                      selectInput("whatline", "which subway line u are goingt o take?", c("orange","blue","green","red","other")),

                      textOutput("text4"),
                      selectInput("d", "depart?", c1),
                      selectInput("d", "arrival?", c1),
                      
                      textOutput("text3"),
                      

             leafletOutput("p2")),
             
             
             
             
             
             tabPanel('ferry',
  
  selectInput("a", "depart?", stations),
  selectInput("b", "arrival?", stations),
  
  textOutput("text1"),
  textOutput("text2"),
  plotOutput("p1")
  ),
  
  
  
  tabPanel("bus", 
           textOutput("text5"),
           selectInput("r", "depart?", c1),
           selectInput("g", "arrival?", c1)),
  
  
  
  ))
server <- function(input, output, session) {
  
  output$text1 <- renderText({ 
    "This part is supposed to show a density curve for the distribution, the two input is the departure and destination, once selected, the plot will generate the density curve for travel time " 
  })

  
    output$text2 <- renderText({ 
    "/n but i faild to link input with the parameter used for plots"})
 
    output$text4 <- renderText({ 
      "the plots will show a line connecting the stations based on the path, and a point marker will highlight the depart/arrival station"})  
  
    
    output$text5 <- renderText({ 
      "again, this part would be very similar to what the previous two did. it outputs two leafflet plots with line and marker. also a computation of the travel time it might take. "}) 
    
  md <- ferry
  md <- md %>% filter(md$departure_terminal=="Hingham")
  md %>% filter(md$arrival_terminal=="Rowes Wharf")
  
  time <-data.frame( "x" = md$actual_arrival-md$actual_departure)
 
  output$text3 <- renderText({ "this part is supposed to be a Leaflet map, but it some how no show in the shiny, wokring on to fix the bug" })
  
  
   output$p1 <- renderPlot(
    #   md <- ferry,
    #   md <- md %>% filter(md$departure_terminal==input$a),
    #   md %>% filter(md$arrival_terminal==input$b),
    #   
    #   time <-data.frame( "x" = md$actual_arrival-md$actual_departure),
    #   
    #   ggplot(data = time, aes(x=x)) +
    #     geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
    ggplot(data = time, aes(x=x)) +
      geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
    
  )
  output$p2 <- renderLeaflet(
    
    {
   # Base layer
   leaflet() }
  )
  
  
}
shinyApp(ui = ui, server = server)