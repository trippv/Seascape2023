# Download Pathfinder temperature data

# define region
north <- "47.47917"
south <- "22.97917"
west <- "-125.7708"
east <- "-110.1042"


options(timeout = max(300, getOption("timeout")))

url.sst.CC <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPH53sstdmday.nc?sea_surface_temperature%5B(1982-01-17T00:00:00Z):1:(1982-12-17T00:00:00Z)%5D%5B(47.47917):1:(22.97917)%5D%5B(-127.7708):1:(-110.1042)%5D"

url.sst.a <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPH53sstdmday.nc?sea_surface_temperature%5B("
url.sst.b <- "-01-17T00:00:00Z):1:("
url.sst.c <- "-12-17T00:00:00Z)%5D%5B(47.47917):1:(22.97917)%5D%5B(-125.7708):1:(-110.1042)%5D"

years <- 1982:2021
files.sst.path <- paste0("sst_", years, ".nc")

url.sst.y <- paste0(url.sst.a, years, url.sst.b, years, url.sst.c)

#download.file(url.sst.y[1], destfile = files.sst.path[1], mode = "wb")
#sst1982 <- rast("sst_1982.nc")
#plot(sst1982)

for(i in 1:length(years)){
  download.file(url.sst.y[i], destfile = files.sst.path[i], mode = "wb")
  print(i)
}



# Read downloaded files and merge using terra package
sst_list <- lapply(files.sst.path, terra::rast)
sst_stack <- sst_list[[1]]
sst_stack <- terra::flip(sst_stack)
for (i in 2:length(sst_list)) {
  sst_bind <- terra::flip(sst_list[[i]])
  sst_stack <- c(sst_stack, sst_bind)
}

# Save the merged NetCDF file
output_file <- "databases/sst/pathfinder_sst_1982-2021.nc"

terra::writeCDF(sst_stack, filename = output_file)



# merge with CDO 
# cdo mergetime *.nc pathfinder_sst_1982-2021.nc


rm(list = ls())
