
#Hiya! Here's the R script version with comments! (:

## Load Libraries------------------------------------------------------------------------------------------------------------------------
#Uncomment these if you need to download the packages first
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("terra")
# install.packages("CoordinateCleaner")
#install.packages("rnaturalearth")
#install.packages("units")
#install.packages("spdep")


#NOTE: For Terra you may need to download RTools for the C++ compiler if you have an older version of R and you don't update it. I encourage you to just use the newest version of R. More details can be found here about terra installation here: https://github.com/rspatial/terra?tab=readme-ov-file

#Then load
library(tidyverse)
library(sf)
library(terra)
library(CoordinateCleaner)
library(rnaturalearth)
library(units)
library(spdep)


## Import Data------------------------------------------------------------------------------------------------------------------------
#sp<-read.csv("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv", stringsAsFactors = T)
#Why does this give us an error? Let's look at the data file...our actual file is a TSV not a CSV

#?read.csv #look here to see which function is more suitable for our data type

sp<-read.delim("data_raw/dicamptodon_tenebrosus_gbif/dicamptodon_tenebrosus_gbif.csv",sep = "\t", stringsAsFactors = T) #Reads in and looks good! Take a gander!

#More details about the standardized GBIF columns here: https://dwc.tdwg.org/terms/

#Here you can uncomment the below line to try a different dataset on your own!
#sp<-read.delim("data_raw/cervus_canadensis_gbif/cervus_canadensis_gbif.csv",sep = "\t", stringsAsFactors = T)




## Data Cleaning and Introduction to Piping------------------------------------------------------------------------------------------------------------------------
 #Filtering criteria from: https://data-blog.gbif.org/post/gbif-filtering-guide/

  #Clean data----
  sp_clean <- sp %>%

    filter(occurrenceStatus  == "PRESENT") %>% # Here we say we want to filter sp where the occurrence status is "present" (not absent)

    filter(!basisOfRecord %in% c("FOSSIL_SPECIMEN","LIVING_SPECIMEN","MATERIAL_SAMPLE","MATERIAL_CITATION")) %>% #We don't want observations that are from a fossil record or specimens living in a zoo or parts of an animal in a museum collection (other parts of the animal may be in other museums) (we want extant wild species)

    filter(year >= 1994) %>% #We want observations after GPS tech was more commonly used for more coordinate precision. Additionally, species change. We want to make sure occurrences reflect more modern distributions.

    # assign NA where appropriate
    mutate(coordinateUncertaintyInMeters = case_when(coordinateUncertaintyInMeters %in% c(301,3036,999,9999) ~ NA,T ~ coordinateUncertaintyInMeters)) %>% #Change commonly used number "dummies" used to indicate no data for coordinate uncertainty

      # remove data with coordinate uncertainty above 3000
    filter(coordinateUncertaintyInMeters <= 3000) %>% #We don't want very uncertain coordinate data

    # remove points at (0, 0)
    filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>% #Remove values at 0,0 which may act as number "dummies" for no data

  #Country and capital centroids may be country or state level geographic data, we want to be sure we have precise coordinate data (not by default these functions know the names of our latitude and longitude, if we changed the name we would need to add arguments to the below functions)

    # remove points within 2km of country centroids
    cc_cen(buffer = 2000) %>%

    # remove points within 2km of capital
    cc_cap(buffer = 2000) %>%

  #We want to be certain these are wild species.
    # remove points within 2km of zoo/herbarium
    cc_inst(buffer = 2000) %>%

  #Points found in the ocean are not correct
    # remove points that are in the ocean
    cc_sea() %>%

    # remove duplicates
    distinct(decimalLongitude, decimalLatitude, speciesKey, datasetKey, .keep_all = TRUE) # only retain rows with a distinct combination of these variables (i.e. if the observation is from the same dataset, has the same coords, is the same species, it may be the same individual or a very intense sampling of multiple individuals. We want to remove these duplicates.)





## Convert to a Spatial Data Object Type using Sf------------------------------------------------------------------------------------------------------------------------
#Right now our data is just a data frame object type
class(sp_clean)

#Here we use st_as_sf to convert a "foreign object" to an sf data object. Data will be recognized as a spatial data object type.

sp_clean_sf<-st_as_sf(sp_clean,coords = c("decimalLongitude", "decimalLatitude"),crs = 4326) #Here we explicitly say our coordinates are the decimalLongitude and decimalLatitude columns with longitude=x and latitude=y (in decimal degree units)

#We explicitly define our datum as WGS 84 (identified as coordinate system (crs) number 4326) to tell sf that these are decimal degree latitude and longitude coordinates

sp_clean_sf #Let's look at the new sf object version of our data frame. Look at the last column and you'll see we now have a geometry attribute column which defines the point information associated with each row.

class(sp_clean_sf) #now an sf spatial object type with an extended data frame

str(sp_clean_sf) # we can also look at the structure (GCS is set to WGS 84)


## Visualizing Our Spatial Data------------------------------------------------------------------------------------------------------------------------

##Initial Plot

#We can visualize the output to see if the data makes sense visually
ggplot()+
  geom_sf(data=sp_clean_sf,colour="red") #Note how the syntax looks different here for ggplot. ggplot has a unique framework where you "build up" elements into the plot using the +

#Hmm we first may need a good point of reference. Maybe plotting the state borders of the US and provinces of Canada would be a useful reference.

#Lets pull some general use state border data from rnaturalearth

st <- ne_states(country = c("Canada","United States of America"),
                                 returnclass = "sf") #We want this as an sf data object

#Now let's redo the plot and add a state layer behind our points (make the state layer the first layer)
ggplot()+
  geom_sf(data=st,colour="black")+
  geom_sf(data=sp_clean_sf,colour="red") #Oop this is really zoomed out for plotting purposes..let's try to zoom in

#We can use the bounding box function to create a boundary box surrounding our species occurrence data and then use that as the plotting window limtis for our plot


##Zooming In

#Create bounding box
bb <- st_bbox(sp_clean_sf)
bb #look at bb, we want the xmin/max and ymin/max for the window limits

#Replotting with new window limits
ggplot()+
  geom_sf(data=st,colour="black")+
  geom_sf(data=sp_clean_sf,colour="red")+
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")])
#Nothing seems incredibly off (from the non-expert eye).


## Importance of Matching Projections------------------------------------------------------------------------------------------------------------------------

## Checks

#Here we change our species occurrence data so that it's in an equal area projection (specifically Albers Equal Area conic). We want an equal area grid in the later steps so we keep all of our data within this projection that preserves area measurements.

#It's highly encouraged to read about the various projected coordinate systems to ensure the system used matches your needs

sp_clean_sf<-sp_clean_sf%>%
st_transform(crs = 5070) #5070 is the crs code for Albers Equal Area

crs(sp_clean_sf) #our CRS is now 5070 which uses the NAD83 datum here (not the WGS84)
crs(st) #does not have a projected coordinate system, uses WGS84 for the datum (not NAD83)
crs(st)==crs(sp_clean_sf) #confirms these are not the same coordinate system definitions


##Plots

#Internally packages like sf and ggplot may coerce your coordinate systems into a common CRS before proceeding. This may not be an issue for plotting but it CAN be an issue for geometric operations or anaylsis because it may coerce your data using the unintended coordinate system.

#Here we see that our plot looks fine with ggplot even with two different coordinate systems used for states and species occurrences
ggplot()+
  geom_sf(data=st,colour="black")+
  geom_sf(data=sp_clean_sf,colour="red")+
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim =  bb[c("ymin", "ymax")])


#However, the standard plot function does NOT internally correct for differences in CRS.
#Using the same bounding box limits we see that our points aren't where they were before but the position of the states remains the same.

plot(sf::st_geometry(st), col = "lightgrey", xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], axes = TRUE)
plot(sf::st_geometry(sp_clean_sf), col = "red",add = TRUE)

#If we update the bounding box around our species data we get this:
bb <- st_bbox(sp_clean_sf)
plot(sf::st_geometry(st), col = "lightgrey", xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], axes = TRUE)
plot(sf::st_geometry(sp_clean_sf), col = "red",add = TRUE)

#Our points are off in space somewhere!


## Fix

#To prevent issues related to this we ALWAYS want to convert our data to a meaningful coordinate system based on our needs.

#Now converting state to the same CRS as species data
st<-st%>%
st_transform(crs = 5070)

#Replotting
bb <- st_bbox(sp_clean_sf)
plot(sf::st_geometry(st), col = "lightgrey", xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], axes = TRUE)
plot(sf::st_geometry(sp_clean_sf), col = "red",add = TRUE)

#Plot looks fine now!





## Creating a Spatial Grid------------------------------------------------------------------------------------------------------------------------

#Here, we're going to create a spatial grid over the range of our species of interest so that we can discretize space and clearly define spatial units.

##Convex Hull

#Getting a convex hull/convex polygon of our species data
#We need to first unionize the points so that the geometry is multipoint. This will make it so the convex hull will make one polygon over the range of points (rather than making a hull for each point).

sp_clean_sf_multi<-st_union(sp_clean_sf)
sp_clean_sf_multi

#Then we use the convex hull function to make the polygon and clipping it by the country borders (no range within the ocean)

sp_clean_sf_hull<-st_buffer(st_convex_hull(sp_clean_sf_multi),10000) #We create the hull and add a small buffer of area around the results

sp_clean_sf_hull #Note the changes in geometry type. We went from point to multipoint to polygon.

plot(sp_clean_sf_hull)
sp_clean_sf_hull<-st_intersection(sp_clean_sf_hull,st) #clip by the country borders
plot(sp_clean_sf_hull) #Oop because of the states borders in st, our convex hull shows this. We need to use union so our convex hull will be one unified polygon.

sp_clean_sf_hull<-st_union(sp_clean_sf_hull)
 #define as a single polygon type (not multi)

plot(sp_clean_sf_hull) #Better!

#Now look at the points compared to the hull
plot(sp_clean_sf_hull)
plot(sp_clean_sf_multi, add=T)



## Cell Size
#Define grid cell size for the grid

#A smaller grid cell size will mean more grid cells (meaning more data). This will generally take up more time, use up more ram (temporary storage), and use up more harddrive storage if you export the file out of R.

#For fun you could try to change the cell area here to something smaller to demonstrate above

cellarea<-40e6 #define our spatial unit (grid cell size) in meters squared (the units of our crs). This would be 40km^2.
cellsize<-sqrt((2 * cellarea) / (3 * sqrt(3)))#Set the grid cell size (opposite edge length) as a function of desired cell area, this is the input the st_make_grid function takes

## Grid!

#Make the grid.

  grid <- st_make_grid(x = sp_clean_sf_hull, #This will use the CRS of our species data since this is the only input. This is what we want. We want a grid that preserves area in the projection.
               cellsize = cellsize,
               square = F) %>% #we normally want hexbins (square=F)
    st_sf()%>% #change hexgrid to sf object
    st_intersection(sp_clean_sf_hull) %>% # we are returned a square grid, this step clip bins to our extent
    mutate(area_m2 = st_area(geometry)) %>%  # return area of each hexbin in square meters
    filter(area_m2 > max(area_m2)-set_units(1, m^2)) %>% #Take the max area and filter out non-full cells. Few cells are the exact area of interest so we filter by the max size of the bins minus 1 square meter unit.
    mutate(grid.id = row_number()) #save row numbers as the grid id column

#Here we plot our data with the grid
ggplot()+
  geom_sf(data=grid,colour="black")+
  geom_sf(data=sp_clean_sf,colour="red")+
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim =  bb[c("ymin", "ymax")])

  save(grid, file = "data_processed/grid_d.tenebrosus.rdata")





## Working with Raster and Vector Data------------------------------------------------------------------------------------------------------------------------

#Geodata is a package that uses a portal for a lot of possible covariate data. Before using any, it is highly encouraged to read about the details of each dataset and seeing if they are a good fit for your needs.

#Don't run this now. It takes a while to pull the data so we will go ahead and load the tif files from the raw data instead

#### DONT RUN BELOW ####
# tmax <-geodata::worldclim_global(var="tmax",res=0.5, path = "data_raw/clim") #spatial resolution in degree minutes (another angular unit, not the same unit as decimal degrees))
#
# prec <-geodata::worldclim_global(var="prec",res=0.5, path = "data_raw/clim")

#You may try to save these as rdata like we did with the vector data but sometimes very large rasters like this can become easily "corrupted"

#These functions will return us temperature maximums or amount of precipitation for each month summarized over the period from 1970 to 2000 (NOTE: If we were doing a real analysis we may want data that represent a more modern period like our species data)

#### DONT RUN ABOVE ####

#Pull files from here instead...

#Summarizing precip data:
prec_files <- list.files("data_raw/clim/climate/wc2.1_30s", pattern = "wc2.1_30s_prec_\\d+\\.tif$", full.names = TRUE) #works for 30 degree seconds (or 0.5 degree minutes) only. this will list all the files in the raw data that have this root pattern in the file name


# Stack them
prec_stack <- rast(prec_files) #returns a stack of our precipitation rasters which have the same alignment and spatial resolution

# Sum across all months
prec_yr <- sum(prec_stack) #this will take a minute

names(prec_yr)<-"Precip.Total" #Explicitly give it a name so it's easy to id in the stack

#Summarizing tmax data:
tmax_files <- list.files("data_raw/clim/climate/wc2.1_30s", pattern = "wc2.1_30s_tmax_\\d+\\.tif$", full.names = TRUE) #works for 30 degree seconds (or 0.5 degree minutes) only. this will list all the files in the raw data that have this root pattern in the file name


# Stack them
tmax_stack <- rast(tmax_files)[[5:8]] #returns a stack of our tmax rasters ONLY for summer months. (months 5-8) Why? I considered the ecology of the species. Salamanders generally don't fare well in extremely hot conditions with summer months likely being the limiting months for them. This may better inform where we dont expect to see the species.

# Sum across summer months
tmax_summer <- sum(tmax_stack) #this will take a minute

names(tmax_summer)<-"Summer.Temp.Maximum" #Explicitly give it a name so it's easy to id in the stack

clim_stack<-c(tmax_summer,prec_yr)

#Here we would summarize the raw data. I omitted this step so that you wouldn't have these large amounts of raw data (from the functions above) within the zipped project folder I provided to you.

#Check CRS
crs(clim_stack) #WGS84 geographic coordinate system/datum

sf::st_crs(sp_clean_sf_hull)==sf::st_crs(clim_stack) #here we use a different function from sf that checks if the crs are equal...as expected they are not because our climate stack data does not have a projected coordinate system associated with it.

#We need to reproject our temp and precip to match the species data but reprojecting rasters can sometimes take a very long time.

#Either choice, reprojecting raster or reprojecting vector, will cause (generally minor) distortions

#Here we'll choose to reproject the raster so that the raster information is summarized to the same area grid cell
#If we reprojected our grid then our equal area grid cells would become distorted especially towards the poles specifically leaving a distorted shape and area for these grid cells

#We should begin by cropping the temp/precip data so we aren't reprojecting the whole global raster, only the area we need for our analysis.

#Let's first take our species range and then reproject it to the projection of the temp/precip data then create a new bounding box that is at the extent of our data but in the projection of our covariates

hull.crop<-sp_clean_sf_hull%>%
  st_buffer(60000)%>% #add buffer so that we intentionally have extra covariate data so none of the grid cells are missing covariate data pixels
  st_transform(crs = st_crs(clim_stack))

plot(hull.crop)

bb.cov<-st_bbox(hull.crop)

#After creating a bounding box for cropping we then crop the data before reprojection
clim.stack1 <- terra::crop(clim_stack, bb.cov)

#Here we use project to reproject the raster data
clim.stack2 <- clim.stack1 %>%
    project(crs(grid), method="bilinear") #There are several methods that can be used for reprojecting rasters. Usually this can be handled internally, however, I set the method as bilinear explicitly which is suitable for continuous raster data like temperature or precipitation (if you have non-continuous data like land cover you would use a different method.)

#Let's look at some of our grid cells overlaid on our temp/precip data!

bb.cells<-st_bbox(grid[1:40,]) #Taking the first 40 grid cells as the boundary


plot(clim.stack2[[1]], xlim = bb.cells[c("xmin", "xmax")], ylim = bb.cells[c("ymin", "ymax")], axes = TRUE)
plot(sf::st_geometry(grid[1:40,]), col = NA, border = "grey23", add = TRUE)

#We definitely see here that some of the grid cells aren't full of pixels. That's okay for what we're doing since we're getting the average pixel value per grid cell (below) BUT we would need to account for this if we were summing any of our values instead.


#Here we now use the zonal statistics tool to summarize the raster pixels in our covariate (temp and precip) to our grid cell

#We summarize by getting the average here. This will take a little while.

covs <- terra::zonal(clim.stack2,
                        terra::vect(grid), #Our grid must be in the terra vector data type
                        fun="mean",na.rm=T,# average of the per pixel maximum temperature per grid cell for first layer, average of the per pixel amount of precipitation for a grid cell for second layer
                        weights=T)%>%
    mutate(grid.id = grid$grid.id) #here we append the grid ids back to our output



#Voila! Now we have cov information per grid cell!




## Working with Vector and Vector Data------------------------------------------------------------------------------------------------------------------------

#Species info aggregated to grid cell
sp.agg <- grid %>%
  mutate(count.to.bin = lengths(st_intersects(grid, sp_clean_sf))) #st_intersects checks how many and which id of the second argument intersects the first argument. if we get the lengths of each it will summarize the st_intersects information per grid cell.

nrow(sp.agg)==nrow(covs) #our row numbers match which is good! The row number also matches the length of our grid cell data which is also a good sign

all<-left_join(sp.agg,covs, by="grid.id") #There are several kind of joins and other kinds may be more suitable depending on your needs. Here we could use any kind of join because all data is of the same length and contains the same grid ids.

plot(all)



## Checking for simple correlation between species data and and covariate data------------------------------------------------------------------------------------------------------------------------

all%>%
select(Summer.Temp.Maximum,Precip.Total,count.to.bin) %>% #only looking at correlations between our covs and our sp data
  mutate(across(everything(), as.numeric)) %>% #needs to all be recognized as a numeric data type by R before correlations can be run
  cor(method = 'spearman', use = "pairwise.complete.obs") #will calculate correlations and omit NAs, spearman method does not assume normality in the data


#Correlations are in the correct directions (positive for precipitation and negative for temperature maximum) but the magnitudes are very poor

#This could be for a number of reasons
hist(all$count.to.bin)
  #Our count to bin data is very right skewed (large values within the dataset but an oversaturation of zeroes)

  #We haven't accounted for difference in effort over the landscape. i.e. closer to cities could mean more accessible which could mean more observations in our species occurrences in these areas

  #It's also possible the spatial resolution we use/spatial unit of the grid cell isn't aligned to how the species percieves the environment or maybe the scale in which these climate covariates are summarized to isn't the scale that impacts the ecological process of occurrence for this species

  #It's also possible that these covariates are not useful for this species

  #Additionally, if we look at a map of this area, we see there's a mountain range that goes through the middle of their range. This can have a major impact on how the relationships we see differ from our expectations. Mountains inhibit human movement which can reduce the number of observations found in the mountain area even when temperature and precipitation may be an important factor for the species. Additionally, these areas may otherwise be suitable based on temperature or precipitation but the species may be unable to encroach upon those areas due to the elevation gain.

#Also temporal mismatch in species data (1994-today) and clim variables (covers 1970-2000) may also have an impact on what we're seeing

#There are a lot of possibilities! Back to the drawing board we go!






## Checking for spatial autocorrelation in the temperature covariate ------------------------------------------------------------------------------------------------------------------------

#Morans Test for temperature

#Here we define the adjacency matrix using our grid geometry
nb <- all%>%
  poly2nb(queen=TRUE) #queens case


nb[52] #This returns the neighbor IDs of 52nd grid cell

#Here we assign the weights
lw <- nb2listw(nb, style="W", zero.policy=TRUE) #style= W means the weights assigned to its neighbors are scaled so that they sum to 1.
#zero.policy=T means a polygon with no neighbors will get a row of zero weights

lw$weights[52]


#These are the weights each neighboring temperature value will be multiplied by before being summed. If a polygon has 5 neighbors, each neighbor will have a weight of 1/4 or 0.25. This weight will then be used to compute the mean neighbor values as in 0.25(neighbor1) + 0.25(neighbor2) + 0.25(neighbor3) + 0.25(neighbor4).

#Tests global morans I (spatial autocorrelation across our region)
moran.test(all$Summer.Temp.Maximum,listw=lw, alternative="greater") #testing if our I value will be greater (i.e. grid cells closer together will have more similar values than those far away). We are testing for positive spatial autocorrelation.

#As expected! Very high spatial autocorrelation
#IF I=0 the values are randomly distributed, I>0 means spatially aggregated, I<0 means spatially disaggregated.

#We see that our I=0.9105069 which is VERY high meaning this data is highly spatially autocorrelated.

#If we encounter this issue with our data then this means we would need to consider a spatial statistical model to account for this (e.g. spatial CAR model in our linear regression)


