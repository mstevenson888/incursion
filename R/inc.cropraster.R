inc.cropraster <- function(x, sf, type = "raster"){
  
  if(is(x, "SpatRaster")){
    # Crop the terra::spatRaster object to the sf extent:
    x.cr <- terra::crop(x = x, y = terra::ext(sf))
    
    # Mask the terra::spatRaster object to the sf polygon boundaries:
    x.r <- terra::mask(x.cr, sf)
  }
  
  if(is(x, "RasterLayer")){
    # Crop the raster::RasterLayer to the sf extent:
    x.cr <- raster::crop(x = x, y = raster::extent(sf))
    
    # Mask the raster::RasterLayer to the sf polygon boundaries:
    x.r <- raster::mask(x.cr, sf)
  }

  if(type == "raster"){
    rval <- x.r
  }
  
  if(type == "data.frame" & is(x, "SpatRaster")){
    rval <- terra::as.data.frame(x = x.r, xy = TRUE) 
  }
  
  if(type == "data.frame" & is(x, "RasterLayer")){
    rval <- raster::rasterToPoints(x = x.r) 
  }
  
  return(rval)
}