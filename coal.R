library(sp)
library(leaflet)
library(spatstat)

# set working directory and read in data
setwd("/Users/Courtney/Desktop/")
data = read.csv("Coal_Ash.csv")

# clean data and convert to Spatial Points Data Frame object type
data$ID = c(1:737)
coords = data[,36:38]
points = SpatialPointsDataFrame(coords = coords[,1:2], 
                                proj4string = CRS("+init=epsg:3857"),
                                data = data,
                                match.ID = coords$ID)

# new data frames for each heavy metal 
LithiumPoints = points[grepl("lithium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
MolybdenumPoints = points[grepl("molybdenum",points$Groundwater.Contamination.Summary, fixed=TRUE),]
ArsenicPoints = points[grepl("arsenic",points$Groundwater.Contamination.Summary, fixed=TRUE),]
CobaltPoints = points[grepl("cobalt",points$Groundwater.Contamination.Summary, fixed=TRUE),]
SeleniumPoints = points[grepl("selenium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
FluoridePoints = points[grepl("fluoride",points$Groundwater.Contamination.Summary, fixed=TRUE),]
LeadPoints = points[grepl("lead",points$Groundwater.Contamination.Summary, fixed=TRUE),]
BerylliumPoints = points[grepl("beryllium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
CadmiumPoints = points[grepl("cadmium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
ThaliumPoints = points[grepl("thalium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
RadiumPoints = points[grepl("radium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
BoronPoints = points[grepl("boron",points$Groundwater.Contamination.Summary, fixed=TRUE),]
BariumPoints = points[grepl("barium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
MercuryPoints = points[grepl("mercury",points$Groundwater.Contamination.Summary, fixed=TRUE),]
ChromiumPoints = points[grepl("chromium",points$Groundwater.Contamination.Summary, fixed=TRUE),]
AntimonyPoints = points[grepl("antimony",points$Groundwater.Contamination.Summary, fixed=TRUE),]

# function to create a ggplot2 color palette
gg_color_hue <- function(n) {
  hues = seq(10, 370, length = n + 1)
  hcl(h = hues, l = 78, c = 73)[1:n]
}

# set up color palette and circle sizes
ggcols = gg_color_hue(7)
sizes = seq(7, 24, by=(12/6))

### just the circles
### Criteria: EPA MCLG < 0.01 mg/L; n >= 5
leaflet(data=points) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircles(lat=ArsenicPoints$Lat, lng=ArsenicPoints$Long, opacity=.4, 
             color=ggcols[1], weight=sizes[7], group="Arsenic") %>%
  addCircles(lat=RadiumPoints$Lat, lng=RadiumPoints$Long, opacity=.5, 
             color=ggcols[2], weight=sizes[6], group="Radium") %>%
  addCircles(lat=BerylliumPoints$Lat, lng=BerylliumPoints$Long, opacity=.6, 
             color=ggcols[3], weight=sizes[5], group="Beryllium") %>%
  addCircles(lat=CadmiumPoints$Lat, lng=CadmiumPoints$Long, opacity=.7, 
             color=ggcols[4], weight=sizes[4], group="Cadmium") %>%
  addCircles(lat=AntimonyPoints$Lat, lng=AntimonyPoints$Long, opacity=.8, 
             color=ggcols[5], weight=sizes[3], group="Antimony") %>%
  addCircles(lat=LeadPoints$Lat, lng=LeadPoints$Long, opacity=.9, 
             color=ggcols[6], weight=sizes[2], group="Lead") %>%
  addCircles(lat=MercuryPoints$Lat, lng=MercuryPoints$Long, opacity=.8, 
             color=ggcols[7], weight=sizes[1], group="Mercury") %>%
  addCircles(lat=points$Lat, lng=points$Long, opacity=.5, color="white", weight=.5) %>%
    addCircleMarkers(lng=~Long, lat=~Lat, label = ~as.character(Name.of.Plant.or.Site),
               fillOpacity=0, radius=20, stroke=FALSE) %>%
    addLegend("bottomleft", colors=ggcols, labels=c("Arsenic (n=126, MCLG=0 mg/L)", 
                                                     "Radium (n=42, MCLG=0 mg/L)",
                                                     "Beryllium (n=24, MCLG=0.004 mg/L)",
                                                     "Cadmium (n=13, MCLG=0.005 mg/L)",
                                                     "Antimony (n=11, MCLG=0.006 mg/L)",
                                                     "Lead (n=8, MCLG=0 mg/L)",
                                                     "Mercury (n=5, MCLG=0.002 mg/L)"), 
              group="Legend",
              title = "Toggle each contaminant on or off</br>
                      using the Layers button. Contaminants</br>
                      chosen for this map have more than 5</br>
                      sites affected (n) and a low Maximum</br>
                      Contaminant Level Goal (MCLG) as set</br>
                      by the <b><a href='https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations#Inorganic'>USEPA</a></b>.</br>   </br>
                      CCR Rule site compliance data</br>
                      were collected and published by</br>
                      EarthJustice. View the data and additional </br>
                      information <b><a href='https://earthjustice.org/features/map-coal-ash-contaminated-sites'>here</a></b>.</br>   </br>") %>%
     addLayersControl(overlayGroups = c("Arsenic", "Radium", "Beryllium","Cadmium","Antimony","Lead","Mercury","Legend"),
                        options = layersControlOptions("bottomright", collapsed = TRUE))
