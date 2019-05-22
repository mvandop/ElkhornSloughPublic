# Classifying Land Cover in the Elkhorn Slough: Shiny App

This repository contains the data and code to generate an R Shiny app that displays information about the Elkhorn Slough's water quality and land cover types for four sub-basins, including Azevedo Pond, Moro Cojo, East Bennett, and North Marsh. 

Before running the app, make sure to change the directory. While it can be changed individually, it can also be done by adding your working directory to the list "workdir"--this is helpful when working with collaborators. Otherwise, just make sure all packages are installed, and the app is ready to run. 

A brief description of the files: 

Folder 'app' contains ui.R and server.R, for the generation of the Shiny app. 

Folder 'landcoverdata' contains Reclass_2007_2018.csv, which provides information on the percent land cover for each of our 10 different categories. These data are derived from the USDA product CropScape, but are reclassified to the limited number of categories. These data are used for the pie charts and bar graphs in the Shiny app. 

MasterQuery2019subbasins.csv contains monthly water quality data for key nutrients in the four sub-basins of interest, from 1990-2018. These data are used to generate the water quality graphs. 

Subbasins.shp (.dbf, .prj, .sbn, .sbx, .cpg, .shp.xml, .shx) provides the borders of the sub-basins in the Leaflet component of the Shiny app

temp_precip.csv contains monthly precipitation and temperature data for the Elkhorn Slough, from 1990-2018. These data are used to generate the precipitation graphs. 

testsp.json contains the spatial polygons for the land cover in the Elkhorn Slough, with data from 2007-2018. These data are integrated into Leaflet.

Please email vandopmolly@gmail.com with any questions or comments on ways to improve the code. 

