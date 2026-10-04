inc.local <- function(ips, par, src.des, neighbours, relative.to = "signs", offset = -3){
   
   # requireNamespace(package = "survival", quietly = TRUE)
   
   # Inititialise matrix to collect results:
   rval <- matrix(data = c(0,0,0,0), nrow = 1, ncol = 4, dimnames = list(NULL, c("srcid", "desid", "status", "days")))

   # Take each positive farm in turn and call this the source farm:
   # j = 15 good choice for testing for 0 - 1000 m:
   
   for(j in 1:length(ips[,1])){
      source <- ips[j,1:7]

      # Define the temporal 'window' to select pos.recips as being potential events associated with the source farm.
      
      # if(relative.to == "signs"){inf.window <- c(source$sgndate + offset, par$slgdate[source$placeid == par$placeid])}
      if(relative.to == "signs"){inf.window <- c(source[,7] + offset, par[,5][source[,1] == par[,1]])}      
      
      # if(relative.to == "infection"){inf.window <- c(source$infdate + offset, par$slgdate[source$placeid == par$placeid])}
      if(relative.to == "infection"){inf.window <- c(source[,6] + offset, par[,5][source[,1] == par[,1]])}

      # Who are the potential recipients of infection (i.e. disease-positive and disease-negative) from the source farm?
      
      # id <- source$placeid == par$placeid
      id <- source[,1] == par[,1]
      neigh <- unlist(neighbours[id])

      if(length(neigh) > 1){
         recips <- par[neigh,1:6]
         recips$infdate <- as.Date("1/1/1900", format = "%d/%m/%Y")
         recips$status <- rep(0, times = nrow(recips))
         
         # Fix up infection dates:
         for (k in 1:nrow(recips)){
            # recips$infdate[k] <- ips$infdate[ips$placeid == recips$placeid[k]]
            recips$infdate[k] <- ips$infdate[ips[,1] == recips[,1][k]]
           }

         # Select the POSITIVE recips (locally infected by the source farm within the temporal window within the specified distance band):
         # id <- src.des$srcplaceid == source$placeid
         id <- src.des[,1] == source[,1]

         recips.pos <- src.des[id,1:7]
         
         # id <- recips$placeid %in% recips.pos$desplaceid
         id <- recips[,1] %in% recips.pos[,3]  
         
         recips.pos <- recips[id,1:8] 
         id <- recips.pos$infdate >= inf.window[1] & recips.pos$infdate <= inf.window[2]
         recips.pos <- recips.pos[id,1]       
         
         # Assign status:
         # id <- recips$placeid %in% recips.pos
         id <- recips[,1] %in% recips.pos
         recips$status[id] <- 1

         # Remove those premises with an infdate or slgdate *before* the start of the temporal window: 
         id <- recips$infdate >= inf.window[1]
         recips <- recips[id,]
         
         # id <- recips$slgdate >= inf.window[1]
         id <- recips[,5] >= inf.window[1]
         recips <- recips[id,]
      
         # Set days for these farms relative to date infectivity starts on the source farm: 
         infinf <- rep(0, times = nrow(recips))
         infinf <- as.vector(recips$infdate - (source$sgndate + offset))
         infslg <- rep(0, times = nrow(recips))
         infslg <- as.vector(recips$slgdate - (source$sgndate + offset))
         recips$days <- apply(cbind(infinf, infslg), MARGIN = 1, FUN = max)
         # recips$days[recips$status == 1] <- as.vector(recips$infdate[recips$status == 1] - (source$sgndate + offset))
         recips$days[recips$status == 1] <- as.vector(recips$infdate[recips$status == 1] - (source[,7] + offset))
      
         # If 'days' is greater than the length of the infection window, let days = infection + 1: 
         recips$days[recips$days > as.numeric(inf.window[2] - inf.window[1])] <- (inf.window[2] - inf.window[1]) + 1
      
         # Add source id:
         # recips$srcid <- rep(source$placeid, times = nrow(recips))
         recips$srcid <- rep(source[,1], times = nrow(recips))
      
         # Append data frame 'tmp' to the rval matrix:
         rval <- rbind(rval, as.matrix(recips[,c(10,1,8,9)]))
         }
      }

# Remove the initialisation case and turn rval into a data frame:
rval.df <- as.data.frame(rval[-1,], row.names = 1:nrow(rval))

# Kaplan-Meier survival function:
rval.km <- survival::survfit(survival::Surv(days, status) ~ 1, conf.type = "none", type = "kaplan-meier", data = rval.df)
rval.df <- data.frame(time = rval.km$time, n.risk = rval.km$n.risk, n.event = rval.km$n.event)
rval.df
}