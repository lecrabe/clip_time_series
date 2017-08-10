####################################################################################
#######    object: PREPARE 2kmBOXES                             ####################
#######    Update : 2017/05/13                                  ####################
#######    contact: remi.dannunzio@fao.org                      ####################
####################################################################################

#################### SET OPTIONS AND NECESSARY PACKAGES
options(stringsAsFactors = FALSE)

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(foreign)
library(dplyr)


#######################################################################
##############################     SETUP YOUR DATA 
#######################################################################
setwd("/media/dannunzio/OSDisk/Users/dannunzio/Documents/countries/myanmar/")
dir.create("gee_kml_files")
getwd()

## Read the datafile and setup the correct names for the variables
pts <- read.table("70_test_plots.txt",header = T)
pts$map_code <- 1

names(pts)
map_code <- "map_code"
point_id <- "id"
xcoord   <- "location_x"
ycoord   <- "location_y"

##############################     Perform some checks
names(pts)
head(pts)
summary(pts)

###    Are the point ID unique indeed ?
nrow(pts)==length(unique(pts[,point_id]))


#######################################################################
### PART I: Create square boxes around the sampling points (2km)
#######################################################################

############### Set the size of the boxes for the points of sampling (2km = 2*1000m)
ysize <- 1000/111321


#################### Loop through points to create a box around each point
lp<-list()

for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,1])
  lp <- append(lp,list(ps))
}

#################### Transform the list of polygons into a SPDF
spdf<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id)],#,xcoord,ycoord)], 
  match.ID = F
)

proj4string(spdf)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

head(spdf)
plot(spdf)
#################### Export as shapefile or KML
writeOGR(obj=spdf,dsn="gee_kml_files/pts_2km_boxes.kml",layer="pts_2km_boxes",driver="KML",overwrite_layer = T)
#writeOGR(obj=spdf,dsn="pts_2km_boxes.shp",layer="pts_2km_boxes",driver="ESRI Shapefile",overwrite_layer = T)

e <- extent(spdf)

#######################################################################
### PART II: Create rectangular grid for export in GEE-API LANDSAT
#######################################################################

### What grid size do we need ? (in degrees)
grid_size = 2           ## for Landsat  @ 30m spatial resolution

if((e@xmax-e@xmin) < grid_size & (e@ymax-e@ymin) < grid_size){
  
  p  <- Polygon(cbind(c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin)))
  ps <- Polygons(list(p),1)
  
  sqr_df <- SpatialPolygonsDataFrame(
    Sr=SpatialPolygons(list(ps)), 
    data = data.frame(1),
    match.ID = F
  )
}else{


### Create a set of regular SpatialPoints on the extent of the created polygons  
sqr <- SpatialPoints(makegrid(spdf,offset=c(0,0),cellsize = grid_size))

### Convert points to a square grid
grid <- points2grid(sqr)

### Convert the grid to SpatialPolygonDataFrame
SpP_grd <- as.SpatialPolygons.GridTopology(grid)

sqr_df <- SpatialPolygonsDataFrame(
  Sr=SpP_grd,
  data=data.frame(rep(1,length(SpP_grd))),
  match.ID=F)
}

### Assign the right projection
proj4string(sqr_df) <- proj4string(spdf)
sqr_df_selected <- sqr_df[spdf,]


### Plot the results
plot(sqr_df_selected)
plot(spdf,add=T)

### Give the output a decent name, with unique ID
names(sqr_df_selected) <- "tileID"
sqr_df_selected@data$tileID <-row(sqr_df_selected@data)[,1]

### Check how many tiles will be created
nrow(sqr_df_selected@data)


#######################################################################
### PART III: Export as KML
#######################################################################
base_sqr <- paste("download_area_grid_lsat",sep="")
#writeOGR(obj=sqr_df_selected,dsn=paste(base_sqr,".shp",sep=""),layer=base_sqr,driver = "ESRI Shapefile",overwrite_layer = T)
writeOGR(obj=sqr_df_selected,dsn=paste("gee_kml_files/",base_sqr,".kml",sep=""),layer=base_sqr,driver = "KML",overwrite_layer = T)


#######################################################################
### PART IV: Create rectangular grid for export in GEE-API SENTINEL
#######################################################################

### What grid size do we need ? (in degrees)
grid_size = 10000/11200 ## for Sentinel @ 10m spatial resolution

if((e@xmax-e@xmin) < grid_size & (e@ymax-e@ymin) < grid_size){
  
  p  <- Polygon(cbind(c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin)))
  ps <- Polygons(list(p),1)
  
  sqr_df <- SpatialPolygonsDataFrame(
    Sr=SpatialPolygons(list(ps)), 
    data = data.frame(1),
    match.ID = F
  )
}else{
  
  
### Create a set of regular SpatialPoints on the extent of the created polygons  
sqr <- SpatialPoints(makegrid(spdf,offset=c(0,0),cellsize = grid_size))

### Convert points to a square grid
grid <- points2grid(sqr)

### Convert the grid to SpatialPolygonDataFrame
SpP_grd <- as.SpatialPolygons.GridTopology(grid)

sqr_df <- SpatialPolygonsDataFrame(
  Sr=SpP_grd,
  data=data.frame(rep(1,length(SpP_grd))),
  match.ID=F)
}

### Assign the right projection
proj4string(sqr_df) <- proj4string(spdf)
sqr_df_selected <- sqr_df[spdf,]

### Plot the results
plot(sqr_df_selected)
plot(spdf,add=T)

### Give the output a decent name, with unique ID
names(sqr_df_selected) <- "tileID"
sqr_df_selected@data$tileID <-row(sqr_df_selected@data)[,1]

### Check how many tiles will be created
nrow(sqr_df_selected@data)


#######################################################################
### PART V: Export as KML
#######################################################################
base_sqr <- paste("download_area_grid_stnl",sep="")
#writeOGR(obj=sqr_df_selected,dsn=paste(base_sqr,".shp",sep=""),layer=base_sqr,driver = "ESRI Shapefile",overwrite_layer = T)
writeOGR(obj=sqr_df_selected,dsn=paste("gee_kml_files/",base_sqr,".kml",sep=""),layer=base_sqr,driver = "KML",overwrite_layer = T)

