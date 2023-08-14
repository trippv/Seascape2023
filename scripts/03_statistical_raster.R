
# 13/08/2023
# Code for extracting ssst temperature from a netcdf using the buffered layer
# already created

# protocol
# for each region:
# 1. open the netcdf with sst data
# 2. open the region-specific shapefile wit the buffer
# 3. Mask the netcdf with the buffer
# 4. Perform differen operations and save the new netcdf


library(terra)
library(here)
library(tidyverse)
library(moments)
library(tidyterra)





############### functions for rasters statistics ############
# range
range_func <- function(x){
  if (all(is.na(x))) {
    return(NA)
  }
  diff_range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  return(diff_range)
}


# skewness
kurtosis_func <- function(x){
  moments::kurtosis(x, na.rm = TRUE)
}



# Diferencia entre modas
Modes_range_func <- function(x) {
  # Remove NAs
  x <- x[!is.na(x)]
  
  # Return NA if all values were NA
  if (length(x) == 0) {
    return(NA)  
  }

  # find modes from ts
  Modes <- LaplacesDemon::Modes(x)
  
  #extract densities values from each mode
  all_modes <- Modes$modes
  
  # extract the highest and lower density value
  extreme_modes <- c(max(all_modes), min(all_modes))
  
  #get delta
  delta_modes <- extreme_modes[1] - extreme_modes[2]
  
  #print
  return(delta_modes)
}



## open netcdf

ncfile <- here("databases/sst/pathfinder_sst_1982-2021.nc")
sst_map_raster <- terra::rast(ncfile)
#sst_map_raster <- terra::flip(sst_map_raster)

## Opean buffered layer

buffer <- terra::vect(here("databases/coastline/BCP_coastline_path._100km.shp"))
buffer_proj <- project(buffer, crs(sst_map_raster))


## mask netcdf
masked_sst <- terra::crop(sst_map_raster, buffer_proj)
masked_sst <- terra::mask(masked_sst, buffer_proj)





## make calculalation of raster
mean_sst <- terra::app(masked_sst, mean, na.rm = TRUE)










# export data as an xy table
values <- as.numeric(values(mean_sst))
longitude <- xyFromCell(mean_sst, 1:ncell(mean_sst))[, 1]
latitude <- xyFromCell(mean_sst, 1:ncell(mean_sst))[, 2]

mean_sst_df <- data.frame(latitude, longitude, values, row.names = NULL)

mean_sst_df <- mean_sst_df |>
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) |>
  dplyr::filter(complete.cases(values))

# by latitude values

lat_sst_mean <- mean_sst_df %>% 
  summarise(lat_mean = mean(values, na.rm = TRUE), 
            .by = latitude)


#
# import countries layer from Natural Earth
countries <-  rnaturalearth::ne_countries(scale = 50, returnclass = "sf")

lon_lims = range(mean_sst_df$longitude)
lat_lims = range(mean_sst_df$latitude)

ggplot() +
  # add raster layer
  geom_spatraster(data = mean_sst, mapping = aes(fill = mean)) +
  scale_fill_distiller(palette = "Spectral", name = "mean sst (°C)")+
  geom_sf(data=countries,
          colour='grey75',
          linetype='solid',
          fill= "antiquewhite",
          size=0.3) +
   coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE, 
            ndiscr = 500)+
  labs(x = "Longitude")
