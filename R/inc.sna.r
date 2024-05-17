inc.sna <- function(edge, vertex, epsg, fn = "SNA", shapefile = TRUE, socnetv = TRUE, dichotomise = TRUE){
   
  # requireNamespace(package = "sf", quietly = TRUE)
  # requireNamespace(package = "dplyr", quietly = TRUE)    
    
   dat.df <- edge
   att.df <- vertex

   # Turn each of the location identifiers into as.character:
   dat.df[,1] <- as.character(dat.df[,1])
   dat.df[,2] <- as.character(dat.df[,2])
   att.df[,1] <- as.character(att.df[,1])
   
   # Assign 1 to n numeric identifiers:
   old <- c(dat.df[,1], dat.df[,2])
   old <- sort(unique(old))
   fix.df <- data.frame(new = 1:length(old), old)
   
   # Assign new source and destination 1 to n numeric identifiers:
   dat.df$src.nid <- fix.df$new[match(dat.df[,1], fix.df$old)]
   dat.df$des.nid <- fix.df$new[match(dat.df[,2], fix.df$old)]

   # Make sure we're only using attributes for locations listed in data frame edge:
   id <- att.df[,1] %in% fix.df$old
   att.df <- att.df[id,]

   # Assign new location identifiers to the attribute data frame:
   att.df$nid <- fix.df$new[match(att.df[,1], fix.df$old)]

   # Sort the attribute data frame:
   att.df <- att.df[order(att.df[,4]),]
   
   # Create network in matrix format:
   n <- dim(att.df)[1]
   dat.nmov <- matrix(rep(0, times = n^2), nrow = n)

   for(i in 1:length(dat.df[,1])){
      # "org" equals the the ith row and first column of dat.df:
      src <- dat.df$src.nid[i]
      
      # "des" equals the the ith row and second column of dat.df:
      des <- dat.df$des.nid[i]
      
      # Take the number of movement events for src and des and slot it into the appropriate cell of the matrix:
      dat.nmov[src, des] <- dat.nmov[src, des] + 1
      }

    if(dichotomise == TRUE){
       # Dichotomise the matrix:
       dat.nmov[dat.nmov > 0] <- 1
       } 

   # SocNetV format:
   if(socnetv == TRUE){
      edge.fn <- paste(fn, "_edge.csv", sep = "")
      vert.fn <- paste(fn, "_vert.csv", sep = "")

      write.table(dat.nmov, file = edge.fn, append = FALSE, quote = TRUE, sep = ",", na = "", row.names = FALSE, col.names = FALSE)
      write.table(att.df, file = vert.fn, append = FALSE, quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
      }
      
   if(shapefile == TRUE){
      edge.fn <- paste(fn, "_edge.shp", sep = "")
      vert.fn <- paste(fn, "_vert.shp", sep = "")
      
      # Shape file for network vertices. Coordinates are in columns 2 and 3 (nid = column 4, id = column 1):
      rval.sf <- sf::st_as_sf(x = data.frame(att.df[,1:4]), coords = c("xcoord","ycoord"), crs = epsg, remove = FALSE)
      sf::st_write(obj = rval.sf, dsn = getwd(), driver = "ESRI Shapefile", layer = vert.fn)
      
     
     # Shape file for network edges. Add source and destination coordinates to each recorded movement event.
     dat.df$srcx <- att.df[,2][match(dat.df$src.nid, att.df$nid)]
     dat.df$srcy <- att.df[,3][match(dat.df$src.nid, att.df$nid)]
     dat.df$desx <- att.df[,2][match(dat.df$des.nid, att.df$nid)]
     dat.df$desy <- att.df[,3][match(dat.df$des.nid, att.df$nid)]
     head(dat.df)
     
     # Generate a spatial lines object to represent movement events:
     vert.df <- data.frame(eid = 1, 
        src = c(dat.df$srcherd[1], dat.df$srcherd[1]),
        des = c(dat.df$desherd[1], dat.df$desherd[1]), 
        x = c(dat.df$srcx[1], dat.df$desx[1]), 
        y = c(dat.df$srcy[1], dat.df$desy[1]))
     # head(vert.df)

     for(i in 2:nrow(dat.df)){
       tvert.df <- data.frame(eid = i, 
         src = c(dat.df$srcherd[i], dat.df$srcherd[i]),
         des = c(dat.df$desherd[i], dat.df$desherd[i]), 
         x = c(dat.df$srcx[i], dat.df$desx[i]), 
         y = c(dat.df$srcy[i], dat.df$desy[i])) 
       vert.df <- rbind(vert.df, tvert.df)
     }
     
     vert.sf <- sf::st_as_sf(x = vert.df, coords = c("x","y"), crs = epsg)
     # windows(); plot(vert.sf)

     edge.sf <- vert.sf %>%
       dplyr::group_by(eid) %>% 
       dplyr::summarize(src = dplyr::first(src), des = dplyr::first(des), do_union = FALSE) %>% 
       sf::st_cast("LINESTRING")
     # windows(); plot(edge.sf)
     
     sf::st_write(obj = edge.sf, dsn = getwd(), driver = "ESRI Shapefile", layer = edge.fn)
   }
}


