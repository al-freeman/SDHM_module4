library (raster)
library(sf)
library(ggplot2)

seed <- read.csv("./data/exercise/traindat/spp106pr_SEED.csv") # read occurrence data
crs(seed)
names(seed) #note wgs_xF
head(seed)

prj.wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" # epsg:4326
#assigning crs for dataframe
seed.pres <- st_as_sf(seed, coords = c("wgs_xF", "wgs_yF"),  #st_as_sf converts the dataframe to a 'sf' (simplefeatures) 
                      crs = prj.wgs84, remove = F) # remove=F to retain input x,y, crs = the projection we're using  
head(seed.pres,2)
#examine points
ggplot() +
  geom_sf(data=seed.pres, pch = 20, color = 'darkgreen') #adds the points   





#### Build point buffered bounding box ####
#have to build all bounding boxes for points

#build point buffered bounding box
seed.bufptSF <- st_union(st_buffer(seed.pres, dist = 0.5))#both steps
st_crs(seed.bufptSF)$proj4string #check crs
#plot
ggplot() +
  geom_sf(data=seed.bufptSF, linetype = 'dashed', fill = 'wheat')   # adds the polygon

# bounding box for polygon created
pres_bufbb <- st_as_sfc(st_bbox(seed.bufptSF, crs = prj.wgs84)) #st_bbox sets bounding around simple        feature 
pres_bufbb 
#plot
ggplot() + geom_sf(data=seed.bufptSF, linetype = 'dashed', fill = 'wheat') + # bufpts
  geom_sf(data=pres_bufbb, linetype = 'dashed', fill = NA)

#graph of all the parts
ggplot() +#calls a ggplot
  geom_sf(data=seed.bufptSF, linetype = 'dashed', fill = 'wheat') + #bufpts
  geom_sf(data=seed.pres, pch = 20, color = 'darkgreen') + #points
  geom_sf(data=pres_bufbb, linetype = 'dashed', fill = NA) #bounding box of bufpts

#examine the extent
st_bbox(pres_bufbb) # presence bbox
extent(as_Spatial(pres_bufbb)) 






#### Rasterize polygon ####
# the as_Spatial is needed to move from the sf world to the raster world
#  NOTE as_Spatial only works on sf-based polygons

pres_bufbb.SP <- extent(as_Spatial(pres_bufbb)) # new blob bbox
class(pres_bufbb.SP) # extent amenable to raster for fishnet conversion
extent(pres_bufbb.SP)
pres_bufbb.SP

pres_bufbbR <- raster(pres_bufbb.SP)
pres_bufbbR# examine
crs(pres_bufbbR) <- st_crs(pres_bufbb)$proj4string # assign crs
crs(pres_bufbbR)
pres_bufbbR
res(pres_bufbbR) <- 1000 #why does this change dimensions to 1,1,1? 
values(pres_bufbbR) <- 1
pres_bufbbR





#### build FNET ####
f1 <- sp::coordinates(pres_bufbbR)# get spatial coords 
f2 <- cellFromXY(pres_bufbbR, f1) # grab center x,y
fnet_DF <- as.data.frame(cbind(f1, f2))
# examine DF characteristics
names(fnet_DF) <- c("cell.wgs_x", "cell.wgs_y", "FNETID") # assign names
fnet_DF <- fnet_DF[c("FNETID", "cell.wgs_x", "cell.wgs_y")] # re-order columns
head(fnet_DF, 2) # examine
tail(fnet_DF, 1) # examine last obs
ncell(pres_bufbbR) # examine; should match tail() from above






#### convert dataframe to spatial object ####
fnet_SF <- st_as_sf(fnet_DF, coords = c("cell.wgs_x", "cell.wgs_y"), 
     crs = prj.wgs84, remove = F) # remove=F retains input x,y) #we did this earlier
head(fnet_SF, 2) # examine
            
extent(as_Spatial(fnet_SF))   #extent is not the same        
extent(as_Spatial(pres_bufbb))



#### Use the fishnet to create a pseudo--absence data frame of [X,Y]’s ####
#For simplicity’s sake, set $n$ for pseudo--absence at twice (2$\times$) the number of presences in your respective RANG, PERS, MORT, and SEED data set
# Bind these data to the presence dataframe you imported in Question #1

            
#### Link Species Data to FISHNET ####        
## First convert species presences to spatial points 
seed.pres_tru <- seed[c("wgs_xF", "wgs_yF", "SEED106")] #get x,y presences 
p1 <- sp::coordinates(seed.pres_tru) # set spatial coords
head(seed.pres_tru) 
class(seed.pres_tru)

pres_bufbbR

#punch species x,y through fishnet and extract cell fnetid where species located
  #result is vector of fnetids 
pres.fnetID <- cellFromXY(pres_bufbbR, p1)# FNETID of presence from pt-buffered poly
head(pres.fnetID)
class(pres.fnetID)


#Internal checking
length(pres.fnetID)#836
length(seed.pres_tru$SEED106) #836

#bind extracted FNETID's with species presence dataframe
pres_truFNET <- cbind(pres.fnetID, seed.pres_tru) 
head(pres_truFNET)
names(pres_truFNET)[1] <- "FNETID"
#rename(pres_truFNET, FNETID = pres.fnetID) #changes variable name in dataframe without having to know which column it is in 
#I think there might be an issue with this not being within the bounding box. Not sure why the extent didn't transfer

plot(pres_bufbb.SP, col = "gray70", legend = F, main = "Points in modelling frame")  # main plot
points(seed.pres_tru$wgs_xF, seed.pres_tru$wgs_yF, pch = 20, col = "darkgreen") # add spp locations

ggplot() +#calls a ggplot
  geom_sf(data=, linetype = 'dashed', fill = 'wheat')  #bufpts
    #I dont' think this is right, it should be in the extent of the bouding box
 




#####IMPORTANT: GROUP USED seed.bufptSF BUT I REALLY THINK WE SHOULD BE USING 
#BOUNDING BOX pres_bufbb for extent 
#moving forward following group code because I can't seem to make the code work



#### Rasterize polygon ####
# the as_Spatial is needed to move from the sf world to the raster world
#  NOTE as_Spatial only works on sf-based polygons

#pres_bufbb.SP <- extent(as_Spatial(pres_bufbb)) # new blob bbox
#class(pres_bufbb.SP) # extent amenable to raster for fishnet conversion
#class(pres_bufbb.SP)

#pres_bufbbR <- raster(pres_bufbb.SP)
#pres_bufbbR# examine
#crs(pres_bufbbR) <- st_crs(pres_bufbb)$proj4string # assign crs
#crs(pres_bufbbR)
#pres_bufbbR
#res(pres_bufbbR) <- 1000 #why does this change dimensions to 1,1,1? 
#values(pres_bufbbR) <- 1
#pres_bufbbR


#### build FNET ####
#f1 <- sp::coordinates(pres_bufbb.R)# get spatial coords 
#f2 <- cellFromXY(pres_bufbb.R, f1) # grab center x,y
#fnet_DF <- as.data.frame(cbind(f1, f2))
# examine DF characteristics
#names(fnet_DF) <- c("cell.wgs_x", "cell.wgs_y", "FNETID") # assign names
#fnet_DF <- fnet_DF[c("FNETID", "cell.wgs_x", "cell.wgs_y")] # re-order columns
#head(fnet_DF, 2) # examine
#tail(fnet_DF, 1) # examine last obs
#ncell(pres_bufbb.R) # examine; should match tail() from above

#### convert dataframe to spatial object ####
#fnet_SF <- st_as_sf(fnet_DF, coords = c("cell.wgs_x", "cell.wgs_y"), 
crs = prj.wgs84, remove = F) # remove=F retains input x,y) #we did this earlier
#head(fnet_SF, 2) # examine

#extent(fnet_SF)   #extent is the same        
#extent(as_Spatial(pres_bufbb))

#class(fnet_SF)

#this step seems pointless...we already created a dataframe, why would we need to create the same dataframe?
#FNET_DF <- st_drop_geometry(fnet_SF) # build dataframe
#head(FNET_DF, 2) #This is the same as fnet_DF, not sure why this is a step in mod 2.3

















#### Link Modelling Frame to FISHNET ####
class(seed.bufptSF)
bufpt.fnetid <- extract(, fnet_DF[c("cell.wgs_x", "cell.wgs_y")])# fromat: raster, datagrame
#nopt.fnetid <- extract(pied.noptR, pied.fnetDF[c("cell.wgs_x", "cell.wgs_y")])
bufpt.fnetid[1:6] # examine: all NA
bufpt.fnetid[7787:7792] # examine: mix of 1 & NA

# create modelling dataframe with FNETIDs
tru.bufptFNET <- cbind(pied.fnetDF, bufpt.fnetid) # bind modelling frame w/FISHNET
head(tru.bufptFNET, 2) # examine
# some internal checking
length(tru.bufptFNET$bufpt.fnetid) # number FNETIDs in tru.bufptFNET
ncell(pied.bufptR) # should equal above

table(tru.bufptFNET$bufpt.fnetid)[[1]] # number of FNETIDs in pied.bufptR
length(which(is.na(tru.bufptFNET$bufpt.fnetid))) # No. NAs

###START LINK MODELLING FRAMES TO FISHNET 
##