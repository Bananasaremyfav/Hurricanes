### Leaflet Tutorial ###

# Load the libraries for the tutorial 
.libPaths("C:/R Packages")
library(leaflet)
library(dplyr)
library(xml2)
library(rvest)
library(stringr)

# Example 1: birthplace of R
m <- leaflet() %>%
  addTiles() %>%  
  addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R")
m  
# What did we learn? How to initialize a map, add the tiles, and add a marker

# Example 2: Nashville
m <- leaflet() %>% setView(lat = 36.1627, lng = -86.7816, zoom = 12)
m %>% addTiles()
# What did we learn? How to set the zoom automatically on a map

# Example 3: Nashville (different tiles)
m %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite)
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
# What did we learn? Leaflet provides various tiles 

# Example 4: Nashville & Labels
m <- leaflet() %>% setView(lat = 36.1592, lng = -86.7785, zoom = 16) %>% addTiles()
m %>% addMarkers(lat = 36.1592, lng = -86.7785, label = "Home of the Predators!")
# What did we learn? How to add a label

# Example 4 (continued): Other marker options 
m %>%  addMarkers(
  lat = 36.1592, 
  lng = -86.7785, 
  label = "Home of the Preds!",
  labelOptions = labelOptions(noHide = T))

m %>% addLabelOnlyMarkers(
  lat = 36.1592, 
  lng = -86.7785, 
  label = "Home of the Preds!",
  labelOptions = labelOptions(noHide = T))
# What did we learn? We learnt about different marker options

# Example 5: Weather (Base Reflectivity: measure of the intensity of precipitation)
leaflet() %>% addTiles() %>% setView(lng = -93.65, lat = 42.0285, zoom = 4) %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )
# What did we learn? How to use WMS tiles

# Example 6: Tracking the trajectory of Hurricane Irma & Jose
# Code to scrape data 
Irma <- read_html("https://www.wunderground.com/hurricane/atlantic/2017/hurricane-irma")
Irma_data <- html_nodes(Irma, "table")
Irma_data <- html_table(Irma_data)[[1]]
Irma_data$Wind <- gsub("\n    ","",Irma_data$Wind)
Irma_data$Pressure <- gsub("\n    ","",Irma_data$Pressure)
Irma_data$Category_chr <- ifelse(Irma_data$Category == "-","None",
                                 ifelse(Irma_data$Category == "1","One",
                                 ifelse(Irma_data$Category == "2","Two",
                                        ifelse(Irma_data$Category == "3","Three",
                                               ifelse(Irma_data$Category == "4","Four",
                                                      "Five")))))
names(Irma_data) <- c("Date","Time","lat","lng","Wind","Pressure","Storm Type","Category","Category_chr")
Irma_data$lat <- as.numeric(gsub("°","",Irma_data$lat))
Irma_data$lng <- as.numeric(gsub("°","",Irma_data$lng))
Irma_data$Name <- "Irma"

Jose <- read_html("https://www.wunderground.com/hurricane/atlantic/2017/hurricane-jose")
Jose_data <- html_nodes(Jose, "table")
Jose_data <- html_table(Jose_data)[[1]]
Jose_data$Wind <- gsub("\n    ","",Jose_data$Wind)
Jose_data$Pressure <- gsub("\n    ","",Jose_data$Pressure)
Jose_data$Category_chr <- ifelse(Jose_data$Category == "-","None",
                                 ifelse(Jose_data$Category == "1","One",
                                        ifelse(Jose_data$Category == "2","Two",
                                               ifelse(Jose_data$Category == "3","Three",
                                                      ifelse(Jose_data$Category == "4","Four",
                                                             "Five")))))
names(Jose_data) <- c("Date","Time","lat","lng","Wind","Pressure","Storm Type","Category","Category_chr")
Jose_data$lat <- as.numeric(gsub("°","",Jose_data$lat))
Jose_data$lng <- as.numeric(gsub("°","",Jose_data$lng))
Jose_data$Name <- "Jose"

Total_data <- rbind(Irma_data,Jose_data)
Total_data$Category_chr <- as.factor(Total_data$Category_chr)

con <- file("H:/Irma_data.csv", encoding = "UTF-8")
write.csv(Total_data, file = con)

# Code for maps 
leaflet(Irma_data) %>% addTiles() %>%
  addCircleMarkers(
    ~lng, 
    ~lat,
    radius = 8
  )

pal <- colorFactor(rev(c("green","yellow","green","orange","red")),domain = c("None","Two","Three","Four","Five"))

leaflet(Irma_data) %>% addTiles() %>%
  addCircleMarkers(
    ~lng, 
    ~lat,
    radius = 8,
    color = ~pal(Irma_data$Category_chr),
    popup = paste(
      "Date: ",Irma_data$Date,"<br>",
      "Time: ",Irma_data$Time,"<br>",
      "Wind: ",Irma_data$Wind,"<br>",
      "Pressure: ",Irma_data$Pressure,"<br>",
      "Storm Type: ",Irma_data$`Storm Type`,"<br>",
      "Category: ",Irma_data$Category,sep=""
    )
  )

# Extra Coding Examples (Alice only to run)
HurricaneIcons <- iconList(
  None = makeIcon("H:/Hurricane_green.png",30,30),
  One = makeIcon("H:/Hurricane_green.png",30,30),
  Two = makeIcon("H:/Hurricane_green.png",30,30),
  Three = makeIcon("H:/Hurricane_yellow.png",30,30),
  Four = makeIcon("H:/Hurricane_orange.png",30,30),
  Five = makeIcon("H:/Hurricane_red.png",30,30)
)

leaflet(Irma_data) %>% 
  setView(lat = 13.1132219, lng = -59.5988, zoom = 4) %>%
  addTiles() %>%
  addMarkers(
    icon = ~HurricaneIcons[Category_chr],
    popup = paste(
      "Date: ",Irma_data$Date,"<br>",
      "Time: ",Irma_data$Time,"<br>",
      "Wind: ",Irma_data$Wind,"<br>",
      "Pressure: ",Irma_data$Pressure,"<br>",
      "Storm Type: ",Irma_data$`Storm Type`,"<br>",
      "Category: ",Irma_data$Category,sep=""
    )
  )

leaflet(Jose_data) %>% 
  setView(lat = 13.1132219, lng = -59.5988, zoom = 4) %>%
  addTiles() %>%
  addMarkers(
    icon = ~HurricaneIcons[Category_chr],
    popup = paste(
      "Date: ",Jose_data$Date,"<br>",
      "Time: ",Jose_data$Time,"<br>",
      "Wind: ",Jose_data$Wind,"<br>",
      "Pressure: ",Jose_data$Pressure,"<br>",
      "Storm Type: ",Jose_data$`Storm Type`,"<br>",
      "Category: ",Jose_data$Category,sep=""
    )
  )
