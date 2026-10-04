inc.stkml <- function(x, arrow = c(5000, 160), fname){
  
  if(is(x, "igraph")){

    vfile <- paste(fname, "_vertex.kml", sep = "")
    efile <- paste(fname, "_edge.kml", sep = "")
    
    # Vertex data preparation. List the vertices in graph:
    vid <- igraph::V(x)
    vlon <- as.numeric(igraph::vertex_attr(x, "lon"))
    vlat <- as.numeric(igraph::vertex_attr(x, "lat"))
    vert.df <- data.frame(id = vid, lon = vlon, lat = vlat, xy = paste(vlon, vlat, sep = ""))
    # head(vert.df); tail(vert.df)
    # range(vert.df$id)
    
    vertll.sf <- sf::st_as_sf(vert.df, coords = c("lon","lat"), remove = FALSE)
    sf::st_crs(vertll.sf) <- 4326
    
    
    # --------------------------------------------------------------------------
    # Edge data preparation. List the edges in the graph:
    
    src.v <- igraph::as_edgelist(x, names = TRUE)[,1]
    des.v <- igraph::as_edgelist(x, names = TRUE)[,2]
    edate <- igraph::get.edge.attribute(x, "edate")
    
    edg.df <- data.frame(src = src.v, des = des.v, edate)
    head(edg.df)
    
    # Edge KML file data preparation. Add srclat, srclon and deslat and deslon from vert file: 
    edg.df$slon <- vert.df$lon[match(edg.df[,1], vert.df[,1])]
    edg.df$slat <- vert.df$lat[match(edg.df[,1], vert.df[,1])]
    
    edg.df$dlon <- vert.df$lon[match(edg.df[,2], vert.df[,1])]
    edg.df$dlat <- vert.df$lat[match(edg.df[,2], vert.df[,1])]
    
    # Arrows:
    xMid <- c();   yMid <- c()
    MidBearing <- c()
    xA1 <- c(); xA2 <- c(); yA1 <- c(); yA2 <- c()
    xMid2 <- c(); yMid2 <- c()
    MidBearing2 <- c()
    
    for (i in 1:length(edg.df[,1])) {
      
      # If source and destination herd are the same, enter zeros:
      if(as.character(edg.df$src[i]) == as.character(edg.df$des[i])){
        xMid[i] <- 0;   yMid[i] <- 0
        MidBearing[i] <- 0
        xA1[i] <- 0; xA2[i] <- 0; yA1[i] <- 0; yA2[i] <- 0
        xMid2[i] <- 0; yMid2[i] <- 0
        MidBearing2[i] <- 0      
      }
      
      # If source and destination herd are not the same, create bearing and mid points:
      if(as.character(edg.df$src[i]) != as.character(edg.df$des[i])){
        
        zTemp <- geosphere::midPoint(c(edg.df$slon[i], edg.df$slat[i]), c(edg.df$dlon[i], edg.df$dlat[i])) 
        xMid[i] <- zTemp[1]; yMid[i] <- zTemp[2]
        MidBearing[i] <- geosphere::bearing(c(xMid[i], yMid[i]), c(edg.df$dlon[i], edg.df$dlat[i]))
        
        xyA <- c(xMid[i], yMid[i])
        aLength <- arrow[1]
        a1 <- MidBearing[i] - arrow[2]
        a2 <- MidBearing[i] + arrow[2]
        
        a1Temp <- geosphere::destPoint(xyA, a1, aLength)
        a2Temp <- geosphere::destPoint(xyA, a2, aLength)
        xA1[i] <- a1Temp[1]; yA1[i] <- a1Temp[2]
        xA2[i] <- a2Temp[1]; yA2[i] <- a2Temp[2]
      }
    }  
    
    edg.df <- data.frame(edg.df, xMidPath = xMid, yMidPath = yMid, MidOfLineBearing = MidBearing, xArrow1 = xA1,
                         yArrow1 = yA1, xArrow2 = xA2, yArrow2 = yA2)
    
    
    # --------------------------------------------------------------------------
    # Make the vertex KML file:
    head.1 <- '<?xml version="1.0" encoding="UTF-8"?>'
    write.table(head.1, file = vfile, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    head.2 <- '<kml xmlns="http://www.opengis.net/kml/2.2">'
    write.table(head.2, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE )
    
    head.3 <- '<Document>'
    write.table(head.3, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    name <- paste("<name>", "Network vertices", "</name>", sep = "")
    write.table(name, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    indexPlacemark <- '<Style id="indexPlacemark">
  <IconStyle>
  <color>ff6f0090</color>
  <Icon>
  <href>http://maps.google.com/mapfiles/kml/shapes/caution.png</href>
  </Icon>
  </IconStyle>
  </Style>'
    
    statusPlacemark <- '<Style id="statusPlacemark">
  <IconStyle>
  <color>7f00ffff</color>
  <Icon>
  <href>http://maps.google.com/mapfiles/kml/paddle/grn-blank.png</href>
  </Icon>
  </IconStyle>
  </Style>'
    
    write.table(indexPlacemark, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    write.table(statusPlacemark, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    name <- paste("<name>", "", "</name>", sep = "")
    write.table(name, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    # All vertices:
    for(i in 1:nrow(vert.df)){
      placemark.s <- "<Placemark>"
      write.table(placemark.s, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      name <- paste("<name>", "", "</name>", sep = "")
      write.table(name, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      description <- paste("<description>", vert.df[i,1], "</description>", sep = "")
      write.table(description, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      point.s <- "<Point>"
      write.table(point.s, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      coordinates <- paste("<coordinates>", vert.df$lon[i], ",", vert.df$lat[i], ",", 0, "</coordinates>", sep = "")
      write.table(coordinates, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      point.e <- "</Point>"
      write.table(point.e, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      placemark.e <- "</Placemark>"
      write.table(placemark.e, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    }
    
    foot.1 <- "</Document>"
    write.table(foot.1, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    foot.2 <- "</kml>"
    write.table(foot.2, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    
    # --------------------------------------------------------------------------
    # Edge KML file:
    
    head.1 <- '<?xml version="1.0" encoding="UTF-8"?>'
    write.table(head.1, file = efile, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    head.2 <- '<kml xmlns="http://www.opengis.net/kml/2.2">'
    write.table(head.2, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    head.3 <- "<Document>"
    write.table(head.3, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    head.4 <- "<name>Network edges</name>"
    write.table(head.4, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    for(i in 1:length(edg.df[,1])){
      placemark.s <- "<Placemark>"
      write.table(placemark.s, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      timestamp <- paste("<TimeStamp><when>", edg.df[i,3], "T00:00:00Z</when></TimeStamp>", sep = "")  
      write.table(timestamp, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      name <- paste("<name>", edg.df[i,1], " to ", edg.df[i,2], "</name>", sep = "")
      write.table(name, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      visibility <- "<visibility>1</visibility>"
      write.table(visibility, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      description <- paste("<description>", edg.df[i,3], "</description>", sep = "")
      write.table(description, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      styleUrl <- "<styleUrl>#transPurpleLineGreenPoly</styleUrl>"
      write.table(styleUrl, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      linestring.s <- "<LineString>"
      write.table(linestring.s, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      tesselate <- "<tessellate>1</tessellate>"
      write.table(tesselate, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      altitudeMode <- "<altitudeMode>clampToGround</altitudeMode>"
      write.table(altitudeMode, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      coordinates.s <- "<coordinates>"
      write.table(coordinates.s, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      src <- paste(edg.df[i,5], edg.df[i,6], 0, sep = ",")
      write.table(src, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      des <- paste(edg.df[i,7], edg.df[i,8], 0, sep = ",")
      write.table(des, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      coordinates.e <- "</coordinates>"
      write.table(coordinates.e, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      # Only draw a line if coordinates are present:
      if(edg.df[i,"xMidPath"] != 0){
        coordinates.s <- "<coordinates>"  
        write.table(coordinates.s, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
        
        # Arrows:
        larrow <- paste(edg.df[i,"slon"],      ",", 
                        edg.df[i,"slat"],      " ", 
                        edg.df[i,"xMidPath"],  ",", 
                        edg.df[i,"yMidPath"],  " ", 
                        edg.df[i,"xArrow1"],   ",", 
                        edg.df[i,"yArrow1"],   " ", 
                        edg.df[i,"xArrow2"],   ",", 
                        edg.df[i,"yArrow2"],   " ", 
                        edg.df[i,"xMidPath"],  ",", 
                        edg.df[i,"yMidPath"],  " ", 
                        edg.df[i,"dlon"],      ",", 
                        edg.df[i,"dlat"], 
                        sep = "")
        write.table(larrow, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
        
        coordinates.e <- "</coordinates>"
        write.table(coordinates.e, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      }
      
      linestring.e <- "</LineString>"
      write.table(linestring.e, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      placemark.e <- "</Placemark>"
      write.table(placemark.e, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    }
    
    foot.1 <- "</Document>"
    write.table(foot.1, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    foot.2 <- "</kml>"
    write.table(foot.2, file = efile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
  }

  if(is(x, "data.frame")){
    
    vfile <- paste(fname, "_vertex.kml", sep = "")
    vert.df <- x
    
    # Make the vertex KML file:
    head.1 <- '<?xml version="1.0" encoding="UTF-8"?>'
    write.table(head.1, file = vfile, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    head.2 <- '<kml xmlns="http://www.opengis.net/kml/2.2">'
    write.table(head.2, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE )
    
    head.3 <- '<Document>'
    write.table(head.3, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    name <- paste("<name>", "Network vertices", "</name>", sep = "")
    write.table(name, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    indexPlacemark <- '<Style id="indexPlacemark">
  <IconStyle>
  <color>ff6f0090</color>
  <Icon>
  <href>http://maps.google.com/mapfiles/kml/shapes/caution.png</href>
  </Icon>
  </IconStyle>
  </Style>'
    
    statusPlacemark <- '<Style id="statusPlacemark">
  <IconStyle>
  <color>7f00ffff</color>
  <Icon>
  <href>http://maps.google.com/mapfiles/kml/paddle/grn-blank.png</href>
  </Icon>
  </IconStyle>
  </Style>'
    
    write.table(indexPlacemark, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    write.table(statusPlacemark, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    name <- paste("<name>", "", "</name>", sep = "")
    write.table(name, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    # All vertices:
    for(i in 1:nrow(vert.df)){
      placemark.s <- "<Placemark>"
      write.table(placemark.s, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      name <- paste("<name>", "", "</name>", sep = "")
      write.table(name, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      description <- paste("<description> Location ID: ", vert.df[i,1], "</description>", sep = "")
      write.table(description, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      point.s <- "<Point>"
      write.table(point.s, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      coordinates <- paste("<coordinates>", vert.df[i,3], ",", vert.df[i,4], ",", 0, "</coordinates>", sep = "")
      write.table(coordinates, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
      
      point.e <- "</Point>"
      write.table(point.e, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)

      timestamp <- paste("<TimeStamp><when>", vert.df[i,2], "T00:00:00Z</when></TimeStamp>", sep = "")  
      write.table(timestamp, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
            
      placemark.e <- "</Placemark>"
      write.table(placemark.e, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)

      
    }
    
    foot.1 <- "</Document>"
    write.table(foot.1, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
    
    foot.2 <- "</kml>"
    write.table(foot.2, file = vfile, append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = FALSE)
  }  

}

