#This ui script produces the layout and user inputs for the Elkhorn Slough Shiny web application

# We set up three tabs of content: "Map", showing annual land use by sub-basin, as well as nutrient 
#  trends over that span (User inputs include year, sub-basin, and nutrient). "Long term trends"
#  show land use changes over time, as well as precipitation and water quality (User inputs include
#  year range, sub-basin, and nutrient). "Links" link to the ESNERR website and the Github page 
 

library(shinydashboard)
library(shiny)
library(sf)
library(leaflet)


#### Header ####
header <- dashboardHeader(title = "Elkhorn Slough")

#### Sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "Map"),
    menuItem("Long Term Trends", tabName = "LongTerm"),
    menuItem("Links", tabName = "Links", 
             menuSubItem("ESNERR Website", icon = icon("home"), 
                         href = "https://www.elkhornslough.org/esnerr/", 
                         newtab = T),
             menuSubItem("Source code (Github)", icon = icon("file-code-o"), 
                         href = "https://github.com/mvandop/ElkhornSloughPublic", 
                         newtab = T))
  ))

#### Body ####
body <- dashboardBody(

  tabItems(
    ### Map Tab - First Tab ####
    tabItem(tabName = "Map",
            fluidRow(
              # Map output
              column(width = 7,  
                     # box(width = NULL, 
                     #     title = "Land Use in the Elkhorn Slough: Four Major Sub-Basins",
                     #     solidHeader = F,
                     #     background = "light-blue",
                     #     imageOutput("landmap", height = "800px"))),
              box(width = NULL, 
                  title = "Land Use in the Elkhorn Slough: Four Major Sub-Basins",
                  solidHeader = F,
                  background = "light-blue",
                  leafletOutput("leafmap", height = 600))),
              # Right content bar
              column(width = 5,
                     box(width =NULL,
                         status = "primary", # Makes header blue
                         # Select map data
                         #title = "Land Use (Choose Year)",
                         #for choices, 1st is for the menu, second for the legend
                         selectInput(inputId = "yearchoice",
                                     label = "Land Use (Choose Year)",
                                     selectize = FALSE,
                                     choices = list(
                                       "2007" = "2007",
                                       "2008" = "2008",
                                       "2009" = "2009",
                                       "2010" = "2010",
                                       "2011" = "2011",
                                       "2012" = "2012",
                                       "2013" = "2013",
                                       "2014" = "2014",
                                       "2015" = "2015",
                                       "2016" = "2016",
                                       "2017" = "2017", 
                                       "2018" = "2018"
                                     ),
                                     selected = "2018")),
                     
                     box(width = NULL,
                         status = "primary", # Makes header blue
                         # Choose sub-basin
                         #for choices, 1st is for the menu, second for the legend
                         selectInput(inputId = "basinchoice",
                                     label = "Sub-basin for Analysis",
                                     selectize = FALSE,
                                     choices = list(
                                       "Azevedo Pond" = "Azevedo Pond",
                                       "North Marsh" = "North Marsh",
                                       "East Bennett" = "East Bennett",
                                       "Moro Cojo" = "Moro Cojo"),
                                     selected = "East Bennett")),
                     
                     # Land Use Plot
                     box(width = NULL, 
                         status = "primary",
                         solidHeader = T, title = "Land Use by Sub-Basin",
                         plotOutput(outputId = "LandPie",
                                    height = 250)),
                     
                     box(width = NULL,
                         status = "primary", # Makes header blue
                         # Choose sub-basin
                         #for choices, 1st is for the menu, second for the legend
                         selectInput(inputId = "nutrientchoice",
                                     label = "Nutrient for Analysis",
                                     selectize = FALSE,
                                     choices = list(
                                       "Ammonia as N" = "Ammonia",
                                       "Orthophosphate" = "Phosphate"),
                                     selected = "Ammonia as N")),
                     
                     # Water Quality Plot
                     box(width = NULL, 
                         status = "primary",
                         solidHeader = T, title = "Water Quality by Sub-Basin",
                         plotOutput(outputId = "QualYear",
                                    height = 200)
                     )))
    ),
    
      ### Trends Tab - Second Tab ####
      tabItem(tabName = "LongTerm",
              fluidRow(
                column(width = 8,  
                       box(width = NULL, 
                           title = "Long Term Land Use in the Elkhorn Slough",
                           solidHeader = F,
                           background = "light-blue",
                           plotOutput(outputId = "LandLongTerm",
                                      height = 300)), 
                       box(width = NULL, 
                           title = "Long Term Water Quality Patterns in the Elkhorn Slough",
                           solidHeader = F,
                           background = "light-blue",
                           plotOutput(outputId = "QualLongTerm",
                                      height = 300)),
                
               box(width = NULL, 
                    title = "Long Term Precipitation in the Elkhorn Slough",
                    solidHeader = F,
                    background = "light-blue",
                    plotOutput(outputId = "precipLongTerm",
                               height = 300))),
              
                # Right content bar
                column(width = 4,
                       box(width =NULL,
                           status = "primary", # Makes header blue
                           # Select map data
                           #title = "Land Use (Choose Year)",
                           #for choices, 1st is for the menu, second for the legend
                           sliderInput("year2", "Year Range", 2007, 2018, value = c(2007, 2018), sep = "")),
                       
                       box(width = NULL,
                           status = "primary", # Makes header blue
                           # Choose sub-basin
                           #for choices, 1st is for the menu, second for the legend
                           selectInput(inputId = "basinchoice2",
                                       label = "Sub-basin for Analysis",
                                       selectize = FALSE,
                                       choices = list(
                                         "Azevedo Pond" = "Azevedo Pond",
                                         "North Marsh" = "North Marsh",
                                         "East Bennett" = "East Bennett",
                                         "Moro Cojo" = "Moro Cojo"),
                                       selected = "East Bennett")),
                       

                       
                       box(width = NULL,
                           status = "primary", # Makes header blue
                           # Choose sub-basin
                           #for choices, 1st is for the menu, second for the legend
                           selectInput(inputId = "nutrientchoice2",
                                       label = "Nutrient for Analysis",
                                       selectize = FALSE,
                                       choices = list(
                                         "Ammonia as N" = "Ammonia",
                                         "Orthophosphate" = "Phosphate"),
                                       selected = "Ammonia as N"))
      ))
      )
    
    ))



# Call the dashboard elements  ----          
dashboardPage(
  title = "ESNERR WebApp",
  skin = "blue",
  header,
  sidebar,
  body)