inc.cropraster <- function (raster, sf, snap = "out", type = "raster"){
  
  # Crop the raster to the sf extent:
  raster.cr <- terra::crop(x = raster, y = raster::extent(sf))
  
  # Mask the raster to the polygon boundaries:
  raster.r <- terra::mask(raster.cr, sf)
  
  if (type == "raster") {
    rval <- raster.r
  }
  if (type == "data.frame") {
    rval <- data.frame(raster::rasterToPoints(raster.r))
  }
  return(rval)
}