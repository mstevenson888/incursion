inc.genint <- function(tra, use = "all"){
   
   if(use == "all"){
      src <- tra[,1]
      des <- tra[,3]
      gen <- as.vector(tra[,5] - tra[,2])
      rval <- data.frame(src, des, gen)
      }
   
   if(use == "first"){
      src <- c(); des <- c(); gen <- c()
      uniq <- unique(tra[,1])
      
      for(i in 1:length(uniq)){
        tra.tmp <- subset(tra, tra[,1] == uniq[i])
        tra.tmp$gen <-  as.vector(tra.tmp[,5] - tra.tmp[,2])

        id <- tra.tmp[,8] == min(tra.tmp[,8])
        tra.tmp <- tra.tmp[id,]
        
        src.tmp <- tra.tmp[,1]
        des.tmp <- tra.tmp[,3]
        gen.tmp <- tra.tmp[,8]
        
        src <- c(src, src.tmp)
        des <- c(des, des.tmp)
        gen <- c(gen, gen.tmp)
       }
      rval <- data.frame(src, des, gen)
   }
   
# Remove negative generation intervals:
rval <- subset(rval, rval$gen > 0)
rval 
}

# load("C:\\TEMP\\incursion\\data\\inc.nzfmd.RData")
# tra <- inc.nzfmd$tra
# gen01 <- inc.genint(tra, use = "first")
# hist(gen01$gen)
# summary(gen01$gen)