inc.fixanames <- function(oareas.df, rareas.df, rmap.sf, rareaname, max.dist = 0.10, use = "both", gengine = "osm", gkey = NA){
  
  # Drop leading and trailing white spaces from each column in data frame oareas.df and rareas.df:
  oareas.df <- data.frame(apply(X = oareas.df, MARGIN = 2, FUN = function(x) trimws(x, which = "both")))
  rareas.df <- data.frame(apply(X = rareas.df, MARGIN = 2, FUN = function(x) trimws(x, which = "both")))

  fmatch.fun <- function(oareas.df, rareas.df, max.dist){

    # Make a copy of oareas.df and rareas.df:
    toareas.df <- oareas.df
    trareas.df <- rareas.df
    
    # How many columns in aname (i.e., number of location variables, nlocv):
    nlocv <- ncol(toareas.df)
    
    # Observed area name strings:
    toareas.df$str <- as.character(apply(X = toareas.df[,1:nlocv], MARGIN = 1, FUN = paste, collapse = " "))
    toareas.df$fstr <- NA
    
    # Reference area name strings:
    trareas.df$str <- as.character(apply(X = trareas.df[,1:nlocv], MARGIN = 1, FUN = paste, collapse = " "))

    # Corrected toreas.df names using agrep:       
    toareas.df$fstr <- sapply(
      X = toareas.df$str,
      FUN = function(pattern_str) {
        matches <- agrep(
          pattern = pattern_str,
          x = trareas.df$str,
          max.distance = max.dist,
          value = TRUE,
          ignore.case = TRUE,
          fixed = FALSE
        )
        
        if (length(matches) > 0) matches[1] else NA
      }
    )
    
    # Add a variable to data frame oareas.df to collect the corrected [reference] location details:
    vname <- paste(names(toareas.df)[1], ".f", sep = "")
    toareas.df[[vname]] <- NA
    names(toareas.df)   
    
    toareas.df[,nlocv + 3] <- trareas.df[,1][match(toareas.df$fstr, trareas.df$str)]
    toareas.df <- toareas.df[,c(1:nlocv,nlocv + 3)]
    head(toareas.df)
    
    # Results:
    fmatch <- toareas.df
    return(fmatch)
  
}

  geocode.fun <- function(oareas.df, gengine = "osm"){

    # Make a copy of oareas.df:
    toareas.df <- oareas.df
    
    # How many columns in aname (i.e., number of location variables, nlocv):
    nlocv <- ncol(toareas.df)
      
    # Area name strings:
    toareas.df$str <- as.character(apply(X = toareas.df[,1:nlocv], MARGIN = 1, FUN = paste, collapse = ", "))
    
    toareas.df$lon <- NA
    toareas.df$lat <- NA
    head(toareas.df)

    if(gengine == "google"){
      for(i in 1:nrow(toareas.df)){
        df <- googleway::google_geocode(address = toareas.df$str[i], key = gkey, simplify = TRUE)    
        
        if(df$status != "ZERO_RESULTS"){
          toareas.df$lon[i] <- df$results$geometry$location[1,2]
          toareas.df$lat[i] <- df$results$geometry$location[1,1]
        }

        message("Using Google Map to correct area name ", i, " of ", nrow(toareas.df))
      }
    }
    
    else
      
      if(gengine == "osm"){
        
        for(i in 1:nrow(toareas.df)){
          df <- data.frame(nominatimlite::geo_lite(address = toareas.df$str[i], lat = "lat", long = "lon"))
          
          toareas.df$lon[i] <- ifelse(nrow(df) > 0, df$lon, NA)
          toareas.df$lat[i] <- ifelse(nrow(df) > 0, df$lat, NA)
          
          message("Using OSM to correct area name ", i, " of ", nrow(toareas.df))
        }
        
      }
    
    # Take the lon-lats from toareas.df and plot over rmap.sf:
    id <- !is.na(toareas.df$lon)

    if(sum(id) == 0){
      toareas.df <- toareas.df[,c(1:nlocv)]
      toareas.df$geo <- NA
      
      vnames <- paste(names(oareas.df), ".r", sep = "") 
      names(toareas.df)[nlocv + 1] <- paste(names(oareas.df)[1], ".r", sep = "")  
      
    }
    
    if(sum(id) > 0){
      ttoareas.df <- toareas.df[id,]
      head(ttoareas.df)
      
      # sf spatial points object:
      ttoareas.sf <- sf::st_as_sf(ttoareas.df, coords = c("lon","lat"), remove = FALSE)
      sf::st_crs(ttoareas.sf) <- 4326
      
      # Make sure the geometry of rmap.sf is OK:
      sf::sf_use_s2(FALSE)
      rmap.sf <- sf::st_make_valid(rmap.sf) 
      sf::sf_use_s2(TRUE)
      sf::st_crs(rmap.sf) <- 4326
      
      # Take the lon-lat from the map query, plot over rmap.sf then look up rmap.sf area name:
      ttoareas.sf <- sf::st_join(x = ttoareas.sf, y = rmap.sf, join = st_within)
      ttoareas.df <- sf::st_drop_geometry(ttoareas.sf)
      names(ttoareas.df)
      
      # Simplify ttoareas.df to list the original area name (column 1) and the corrected area name (column 2):
      vnames <- names(ttoareas.df)[c(1,4 + nlocv)]
      ttoareas.df <- ttoareas.df[,vnames]
      names(ttoareas.df) <- c("original","corrected")
      
      # Update data frame toreas.df with the corrected area name from data frame ttoareas.df:
      toareas.df$corrected <- ttoareas.df$corrected[match(toareas.df[,1], ttoareas.df$original)]
      head(toareas.df)

      toareas.df <- toareas.df[,c(1:nlocv,nlocv + 4)]
      names(toareas.df)[nlocv + 1] <- paste(names(oareas.df)[1], ".g", sep = "")  
    }  
      
    # Results:
    geocode <- toareas.df
    return(geocode)
  }

  # ----------------------------------------------------------------------------
  if(use == "fuzzy"){
    message("Using fuzzy matching to check (and correct if necessary) ", nrow(oareas.df), " area names ...")
    fmatch.df <- fmatch.fun(oareas.df = oareas.df, rareas.df = rareas.df, max.dist = max.dist)
    rval.df <- fmatch.df
  }

  # ----------------------------------------------------------------------------
  if(use == "geocode"){
      geocode.df <- geocode.fun(oareas.df = oareas.df, gengine = gengine)
      rval.df <- geocode.df
      
  }
  
  
  # ----------------------------------------------------------------------------
  if(use == "both"){
    
    # Run fmatch.fun first:
    message("Using fuzzy matching to check (and correct if necessary) ", nrow(oareas.df), " area names ...")
    fmatch.df <- fmatch.fun(oareas.df = oareas.df, rareas.df = rareas.df, max.dist = max.dist)
    message("Fuzzy matching completed.")
    Sys.sleep(time = 3)
    
    # How many columns in oareas.df?
    nlocv <- ncol(oareas.df)
    
    # Select rows of fmatch.df where is.na(des.r): areas where we couldn't get a match:
    id <- is.na(fmatch.df[,c(nlocv + 1)]); table(id)
    toareas.df <- fmatch.df[id,1:nlocv]
    
    message("Now geocoding the ", nrow(toareas.df), " area names that couldn't be corrected using fuzzy matching ...")
    Sys.sleep(time = 3)
    
    geocode.df <- geocode.fun(oareas.df = toareas.df, gengine)
    message("Geocoding completed.")
    
    # Fuzzy matched:
    rval.df <- fmatch.df
    names(rval.df)[nlocv + 1] <- paste(names(rval.df)[1], ".f", sep = "")
    
    # Geocoded:
    rval.df$geo <- geocode.df[,nlocv + 1][match(rval.df[,1], geocode.df[,1])]
    names(rval.df)[nlocv + 2] <- paste(names(rval.df)[1], ".g", sep = "")
  }
  
  return(rval.df)
}
