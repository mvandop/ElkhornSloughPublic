#This server script produces the figures and content shown on the Elkhorn Slough Shiny web application

#On the first tab of content: "Map", a leaflet map is produced, as well as a land cover pie chart and 
#  water quality, given the subbasin, nutrient, and year inputs in the ui function
#On the second tab: "Long Term Trends", a land cover bar graph, precipitation graph, and water quality 
#  graph is generated, based on the year range, subbasin, and nutrient chosen. 

#Load required packages
library(shinydashboard)
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(htmltools)
library(tidyverse)
library(DT)
library(viridis)
library(ggplot2)
library(scales)
library(rgdal)

#Set working directory
workdir <- c("C:/Users/Molly/Box/ElkhornSlough/",
             "/Users/anayahall/projects/ds421/elkslo/ElkhornSlough/"#,
             #        "your local box folder",
)
workdir <- workdir[dir.exists(workdir)]
setwd(workdir)

#Bring in Water Quality Data

waterqual <- read_csv(paste0(workdir, "MasterQuery2019subbasins.csv")) 

#Remove values from Azevedo North (just keep Azevedo Central)

waterqual <- waterqual %>% filter(ESNERR_StCode %in% c("APC", "BSE", "MCS", "STB"))


#ggplot theme for the quality graphs
theme_m <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())

#Bring in data for precipitation -----
precip <- read_csv(paste0(workdir, "temp_precip.csv"))

###Fixing data for Pie charts -----
#Bring in land cover data 
lc <- read_csv(paste0(workdir, "landcoverdata/Reclass_2007_2018.csv"))

# split value type into two columns: metric and year
lc$metric <- NA
lc$year <- NA

lc$metric <- ifelse(grepl("Prop", lc$X1), 'Prop', 'Pixel')
lc$year <- substr(lc$X1,nchar(lc$X1)-3, nchar(lc$X1))

# can get rid of concatenated value type column
lc$X1 <- NULL

# filter(str_detect(Activity, "Compost")
lc_prop <- lc %>% filter(str_detect(metric, "Prop"))

#make wide
lc_wide <- gather(lc_prop, "category", "value", -Location, -metric, -year)

### Chart symbology

# blank theme (to remove axes ticks) for pie charts
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# categories of landcover
colcats = c("Developed", "Field Crops", "Forest/Shrubland", "Fruits & Veg" , 
            "Grass/Open Space", "Strawberries", "Tree Crops", "Wetlands")
# colors used in chippie's landcover symbology
mycols <- c("gray54", "darkorange", "darkgreen", "chartreuse", 
            "darkolivegreen3",  "red2", "darkorchid3", "paleturquoise2", "gray54")
#Tree crops, grass, forest, developed, open water, strawberries, fruits and veg, wetlands

###Bring in data for leaflet -----
#landuse
testsp <- geojson_read("testsp.json", what = "sp")

#subbasinoutlines
subbasin <- readOGR("Subbasins.shp", layer="Subbasins")
latlong <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
subbasin <- subbasin %>% spTransform(CRS(latlong))


#Colors for leaflet
pal <- colorNumeric(c("gray54", "darkolivegreen3", "red2", "darkorange","chartreuse",
                      "darkgreen",  "paleturquoise2", "darkorchid3", "blue"), NULL)

###Server function-----
server <- function(input, output, session) {
  # Send a pre-rendered image, and don't delete the image after sending it
  # output$landmap <- renderImage({
  #   filename <- normalizePath(file.path(paste0("~/Downloads/Landcover_Layouts_2013_2018/Resized/Resize_", input$yearchoice, ".png")))
  #   # Return a list containing the filename and alt text
  #   list(src = filename,
  #        alt = paste0("Image number", input$yearchoice))
  #   
  # }, deleteFile = FALSE)
  
  
  output$LandPie <- renderPlot({
    plotdata <- lc_wide %>% filter(Location == input$basinchoice, year == input$yearchoice) 
    bp <- ggplot(plotdata, aes(x="", y = value, fill = category)) +
      geom_bar(width = 1, stat = "identity")
    # turn into pie
    bp + coord_polar("y", start=0) + blank_theme +
      geom_text(aes(label = paste(value*100, "%")), 
                position = position_stack(vjust = 0.5), 
                color= "black", size = 4) +
      theme(axis.text.x=element_blank(), plot.title = element_text(hjust=0.5)) + 
      ggtitle(paste0(input$basinchoice, " ", input$yearchoice)) + 
      scale_fill_manual(values = mycols, name = "Land Cover", labels = colcats)

  })
  

  
  output$LandLongTerm <- renderPlot({
    plotdata <- lc_wide %>% filter(Location == input$basinchoice2, year >= min(input$year2), year <= max(input$year2)) 
    ggplot(plotdata, aes(fill=category, y=value, x=year)) +
      geom_bar(stat="identity", position="fill") +
      scale_fill_manual(values = mycols, name = "Land Cover", labels = colcats) +
      ggtitle(paste(input$basinchoice2)) + labs(y = "Proportion of Sub-Basin", x = "Year") +
      theme_minimal() +
      theme(plot.title = element_text(hjust=0.5), text = element_text(size=20))
  })
  
  output$QualYear <- renderPlot({
    # draw the graph with the specified number of dates
    subsetting <- subset(waterqual, Year == as.numeric(input$yearchoice)) %>% filter(Subbasin == input$basinchoice)
    ggplot(subsetting, aes_string(x = "month", y = input$nutrientchoice)) + 
      geom_line(color = "coral3") + 
      ylab(paste0(input$nutrientchoice, " (mg/L)")) + 
      ggtitle(paste0(input$nutrientchoice, " (mg/L): ", input$yearchoice)) + 
      theme_m + 
      scale_x_discrete(limits=c("Jan", "Feb", "Mar",
                                "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep",
                                "Oct", "Nov", "Dec"),
                       labels=c("Jan", "", "",
                                "Apr", "", "",
                                "Jul", "", "",
                                "", "", "Dec"))  })
  
  output$QualLongTerm <- renderPlot({
    # draw the graph with the specified number of dates
    subsetting <- waterqual %>% filter(Subbasin == input$basinchoice2, Year >= min(input$year2), Year <= max(input$year2))
    subsetting$Date <- as.Date(subsetting$Date, format = "%m/%d/%Y")
    ggplot(subsetting, aes_string(x = "Date", y = input$nutrientchoice2, group = 1)) + 
      geom_line(color = "coral3") + 
      ylab(paste0(input$nutrientchoice2, " (mg/L)")) + 
      ggtitle(paste0(input$nutrientchoice2, " (mg/L): ", input$basinchoice2, ", ", as.character(min(input$year2)), "-", as.character(max(input$year2)))) + 
      theme_m  + 
      scale_x_date(labels = date_format("%Y"))
                   })
  #Output graph depicting precipitation on Long Term Trends Tab
  
  output$precipLongTerm <- renderPlot({
    # draw the graph with the specified number of dates
    subsetting <- precip %>% filter(Year >= min(input$year2), Year <= max(input$year2))
    subsetting$Date <- as.Date(subsetting$Date, format = "%m/%d/%Y")
    ggplot(subsetting, aes(x = Date, y = Precip, group = 1)) + 
      geom_line(color = "steelblue") + 
      ylab("Precipitation (inches)") + 
      ggtitle(paste0("Precipitation (inches): ", as.character(min(input$year2)), "-", as.character(max(input$year2)))) + 
      theme_m  + 
      scale_x_date(labels = date_format("%Y"))
  })

  tiles_cust <- "https://api.mapbox.com/styles/v1/vmcg/cjgjigumz001n2rqvo8xef3t1/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoidm1jZyIsImEiOiJjajRxMjdlNDUwc3I1MzN0Y3Q4M25yaTFvIn0._RA-S8a296YTmsdkBryKDQ"
  tiles_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © E McGlynn"
  
 #Leaflet function 
  output$leafmap <- renderLeaflet({
    chooseYear <- as.numeric(input$yearchoice)
    testyear <- testsp[testsp@data$yeartest == chooseYear,]
    leaflet() %>%
      addTiles(
        urlTemplate = tiles_cust,
        attribution = tiles_attr
      ) %>%
      setView(lng = -121.75, lat = 36.83, zoom = 12) %>% 
      addEasyButton(easyButton(
        id ="OV",
        icon ="fa-globe", title="Zoom to Level 12",
        onClick = JS("function(btn, map){ map.setZoom(12); }"))) %>% 
    addEasyButtonBar(
      position = "bottomright",
      easyButton(
        id = "cHI",
        position = "bottomright",
        icon = icon("circle-o"),
        title = "Zoom back to Elkhorn Slough",
        onClick = JS("function(btn, map){map.setView([36.83, -121.75],12); }"))) %>% 
      addPolygons(data = testyear, stroke = FALSE, 
                  smoothFactor = 0.3, fillOpacity = 1, fillColor = ~pal(FID)) %>% 
      addPolygons(data = subbasin, fillOpacity = 0, color = "black", 
                  popup = paste0(subbasin$Name, "<br>", "Land Use in ", input$yearchoice))
       #addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
      #           labFormat = labelFormat(transform = function(x) round(10^x)))
    
  }) #end of leaflet function
  
}



