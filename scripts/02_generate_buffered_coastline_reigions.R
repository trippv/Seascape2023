
# 10/08/2023
# Analysis of region
# process:
## 1: read each region and corresponding coastline
## 2: buffer a region of 200 km from coastline using the terra package
## 3: mask each region with this buffer
## 4: export masked region as shp file

library(terra)
library(here)
library(tidyverse)

# coastlines files
Baja_path <- here("databases/coastline/BCP_coastline_path.kml")
NWA_path <- here("databases/coastline/NWA_coastline_path.kml")
SAF_path <- here("databases/coastline/SAF_coastline_path.kml")

# make list with paths
coastlines_paths <- list(Baja_path,
                         NWA_path,
                         SAF_path)


# open GHSS map for region
BMap.l <- vect(here("databases/GSHHS_shp/l/GSHHS_l_L1.shp"))

plot(BMap.l)



# define buffer distance
#Note that the distance unit of the buffer width parameter is meters if the CRS is (+proj=longlat), 
#and in map units (typically also meters) if not.
m <- 100000 # = 200km

# estrategia: generar un buffer objetivo de 200km y se hara el intersecto con el vector de coastline

# extract paths
coastline_buffered <- lapply(coastlines_paths, function(region) {
  coastline_vector <- terra::vect(region) #vector from kml
  coastline_asLines <- terra::as.lines(coastline_vector)
  buffer <- terra::buffer(coastline_asLines, m)[1, ]
  
  # crop region map
  coastline_lims <- coastline_vector |>
    geom() |>
    as.data.frame()|>
    dplyr::select(x,y)
  
  lat_lim <- range(coastline_lims$y)
  lat_lim[2] <- lat_lim[2] + 2# add some buffer to higher lat
  
  lon_lim <- range(coastline_lims$x)
  lon_lim[1] <- lon_lim[1]
  
  map_lims <- c(lon_lim, lat_lim)
  
  Bmap_crop <- crop(BMap.l, map_lims)
  
  
  
  # usar poly.pac.vect solamente para Baja california para enmascarar el Golfo de California
  
  if(str_detect(basename(region), "BCP")) {
    # set mask on GOC coast
    poly.pac <- data.frame(x = c(-130, -123.4134, -123.4134, -121.7512, -117.0138, -115.6840, -114.9776, -113.8556, -112.8998, -112.0271, -111.3622, -110.6558, -109.9078, -109.9909, -130), 
                           y = c(47.5, 47.5, 47.21261, 41.54468, 36.04748, 32.01847, 29.96982, 28.80892, 27.27243, 26.35054, 24.91649, 24.06288, 23.51658, 23.00442, 23.00442))
    poly.pac.vect <- vect(as.matrix(poly.pac), "polygons")
    
    # intersect with GOC mask
    new_poly <- terra::intersect(terra::erase(buffer, Bmap_crop[1, ]), poly.pac.vect)
  } else {
    
    
    new_poly <- terra::erase(buffer, Bmap_crop[1, ])
  }
  
  # save shapefile  
  distance <- m / 1000
  distance_label <- paste0("_", distance,"km.shp")
  filename <- str_replace(region, "kml", distance_label)
  writeVector(new_poly, filename = filename, overwrite = TRUE)
  return(new_poly)
  
})




# test
plot(coastline_buffered[[3]])



