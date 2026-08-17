inc.whichpoly <- function(points, poly.sf){
  
  requireNamespace("sf", quietly = TRUE)
  
  # Create sf objects:
  pointsll.sf <- sf::st_as_sf(points, coords = c("lon","lat"), remove = FALSE)
  sf::st_crs(pointsll.sf) <- 4326
  sf::st_crs(poly.sf) <- 4326
  
  # Look up attributes from aulgall.sf and add them to farmll.sf
  rval.sf <- sf::st_join(x = pointsll.sf, y = poly.sf, join = sf::st_within)

  rval.df <- sf::st_drop_geometry(rval.sf)
  return(rval.df)
}