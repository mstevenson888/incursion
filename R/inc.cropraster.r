inc.cropraster <- function(raster, sf, snap = "out", type = "raster"){
  # Extent of the sf object:
  sf.ext <- sf::st_bbox(sf)
  
  # Crop the raster to the spatial polygon extent:
  raster.cr <- raster::crop(raster, sf.ext, snap = snap)
  
  # Dummy the raster with a spatial extent equal to the cropped raster, but full of NAs:
  crop <- raster::setValues(raster.cr, NA)
  
  # Rasterise the catchment boundaries, with NA outside the catchment boundaries:
  sf.r <- raster::rasterize(x = sf, y = crop)
  
  # Put NAs in all the raster cells outside the spatial polygons boundary:
  raster.r <- raster::mask(x = raster.cr, mask = sf.r)
  
  if(type == "raster"){
    rval <- raster.r
  }
  
  if(type == "data.frame"){
    rval <- data.frame(raster::rasterToPoints(raster.r))
  }
  
  return(rval)
}