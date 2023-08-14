
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
library(patchwork)





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



#### inicia la funcion para cada estadistico


## make calculalation of raster
stat_sst <- terra::app(masked_sst, mean, na.rm = TRUE)

#change variable name for homogenizatin
names(stat_sst) <- "stat_value"


# export data as an xy table
values <- as.numeric(values(stat_sst))
longitude <- xyFromCell(stat_sst, 1:ncell(stat_sst))[, 1]
latitude <- xyFromCell(stat_sst, 1:ncell(stat_sst))[, 2]

stat_sst_df <- data.frame(latitude, longitude, values, row.names = NULL)

stat_sst_df <- stat_sst_df |>
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) |>
  dplyr::filter(complete.cases(values)) 

stat_sst_df_latmean <- stat_sst_df |>
  summarise(lat_mean = mean(values, na.rm = TRUE), 
            .by = latitude)



lon_lims = range(stat_sst_df$longitude)
lat_lims = range(stat_sst_df$latitude)

map_lims <- c(lon_lims, lat_lims)

#### map

# open GHSS map for region
BMap.l <- terra::vect(here("databases/GSHHS_shp/i/GSHHS_i_L1.shp"))
BMap.l <- terra::crop(BMap.l, map_lims)
BMap.l <- BMap.l[1, ]




map_plot <- ggplot() +
  # add raster layer
  geom_spatraster(data = stat_sst, 
                  mapping = aes(fill = stat_value), 
                  na.rm = TRUE) +
 # coord_sf(crs = "+proj=robin +over") +
  scale_fill_distiller(palette = "Spectral", 
                       name = "mean sst (°C)", 
                       na.value = NA)+
  geom_sf(data=BMap.l,
          colour='grey75',
          linetype='solid',
          fill= "antiquewhite",
          size=0.3) +
   coord_sf(expand = FALSE, 
            ndiscr = 500,
            )+ #crs = "+proj=robin +over"
  labs(x = "Longitude") +
  theme_bw()+
  theme(legend.position = "top")




line_plot <- ggplot(data = stat_sst_df_latmean, aes(y = lat_mean, x = latitude))+
  geom_line(linewidth = 1.5)+
  #lims(x = lat_lims)+
  labs(x = " ", y = "Temperature (°C)")+
  theme_bw()+
  scale_y_continuous(expand = c(0,0), n.breaks = 6)+
  scale_x_continuous(expand = c(0,0), limits = lat_lims)+

  coord_flip()+
  # put fake grid lines for reference
  geom_vline(xintercept = c(24, 26, 28, 30, 32, 34))+
  #scale_y_reverse()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") 

line_plot



all_plot <- line_plot + map_plot + plot_layout(ncol = 3, widths = c(1, 1, 3)) +
  theme(legend.position = "bottom")

all_plot
