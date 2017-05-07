library(rgdal)
library(rgeos)
library(sp)
library(maptools)
library(readxl)
library(dplyr)
library(ggplot2)
library(tmap)
library(tmaptools)


load("/Users/Erin/Desktop/git/Homeless_USA/CoCShapefiles/coc2016.rda")

# Add IL CoC shapefile from 2015
#coc2015[[14]]$COCNUM
#coc2016[[14]] <- coc2015[[14]]
#coc2016[[4]] <- coc2015[[4]]
#coc2016[[5]] <- coc2015[[5]]
#save(coc2016, file = "/Users/Erin/Desktop/git/Homeless_USA/CoCShapefiles/coc2016.rda")

# Plot CoC shapefiles by state
# pdf("state_coc.pdf")
# for (i in 1:51)
#   try(plot(coc2016[[i]], main = coc2016[[i]]$ST[1]))
# dev.off()

# Change CRS for some replaced states
for (i in c(4,5,14)){
  proj4string(coc2016[[i]]) <- proj4string(coc2016[[1]])
}
# Stack CoC shapefiles for all states
names(coc2016[[1]]) <- toupper(names(coc2016[[1]]))
coc_shp_16 <- coc2016[[1]]
for (i in 2:length(coc2016)){
  names(coc2016[[i]]) <- toupper(names(coc2016[[i]]))
  coc_shp_16 <- spRbind(coc_shp_16,coc2016[[i]])
  print(i)
}

# Add PIT counts to CoC shapefiles
coc_pit_16 <- read_excel("/Users/Erin/Desktop/git/ShinyApps/data/2007-2016-PIT-Counts-by-CoC.xlsx")
coc_pit_16 <- coc_pit_16[-(403:405),]
#class(coc_shp_16)
#dim(coc_shp_16)
unique(coc_pit_16$`CoC Number`)
unique(coc_shp_16$COCNUM)

coc_shp_16 <- merge(coc_shp_16,coc_pit_16, by.x="COCNUM", by.y="CoC Number", all.x=TRUE)
names(coc_shp_16)

# Download US county shapefiles
get_US_county_2010_shape <- function() {
  dir <- tempdir()
  download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
  unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
  read_shape(file.path(dir, "gz_2010_us_050_00_20m.shp"))
}
US_county <- get_US_county_2010_shape()
US_county$County <- US_county$NAME

mystate <- "CA"
mycode <- "06"
current_state <- coc_shp_16[coc_shp_16$ST==mystate,]
state2 <- US_county[US_county$STATE==mycode,]
tm <- tm_shape(current_state) +
        tm_polygons("Total Homeless, 2016", style="quantile", border.alpha = 0, alpha = 0.8) +
      tm_shape(state2) +
        tm_polygons("County", alpha = 0, border.alpha = 0.5, border.col = "green", legend.show = FALSE) +
      tm_shape(current_state) +
        tm_polygons("COCNAME", alpha = 0, legend.show = FALSE, border.col = "red")
tmap_leaflet(tm, mode = "view")

#tmap_mode("view")
#last_map()

#save_tmap(tm, "World_map.html")

#-----------------------------------------------
coc_shp_16_fortify <- ggplot2::fortify(coc_shp_16)
class(coc_shp_16_fortify)
head(coc_shp_16_fortify)

# Add PIT data to coc_fortify
coc2016_coord_pit <- merge(coc_shp_16_fortify,coc_pit_16,by.x="id",by.y="CoC Number",all.x=TRUE)
head(coc2016_coord_pit$id)
unique(coc2016_coord_pit$id)


pdf("coc_polygons.pdf",width = 12)
p <- ggplot() + geom_polygon(data = coc2016_coord_pit, aes(x=long, y=lat, group=id, fill=`Total Homeless, 2016`))
print(p)
dev.off()


