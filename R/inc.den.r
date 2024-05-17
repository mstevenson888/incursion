inc.den <- function(loc, par, dim, shape = "square"){
   
   requireNamespace(package = "splancs", quietly = TRUE)
   
   if(shape == "square"){
      # Define the vertices of the square:
      xmin <- loc[1] - (dim / 2); xmax <- loc[1] + (dim / 2)
      ymin <- loc[2] - (dim / 2); ymax <- loc[2] + (dim / 2)
      poly.p <- data.frame(x = c(xmin, xmin, xmax, xmax), y = c(ymin, ymax, ymax, ymin))
      poly.a <- dim^2
   }
   
   if(shape == "circle"){
      t <- seq(0, 2 * pi, length = 1000)
      poly.p <- t(rbind(loc[1] + sin(t) * dim, loc[2] + cos(t) * dim))
      poly.a <- pi * dim^2
   }   
   
   # Extract farms within the owin:
   par.p <- splancs::as.points(x = par[,1], y = par[,2])
   id <- splancs::inout(pts = par.p, poly = poly.p, bound = NULL, quiet = TRUE)
   tpar <- par[id,]
   
   # How many point locations?
   pre.n <- nrow(tpar)
   # How many animals?
   ani.n <- sum(tpar[,3])
   
   # Point locations per unit area:
   pre.d <- pre.n / poly.a
   # Animals per unit area:
   ani.d <- ani.n / poly.a
   
   rval <- list(
     premises = data.frame(n = pre.n, area = poly.a, den = pre.d),
     individuals = data.frame(n = ani.n, area = poly.a, den = ani.d))
   rval
}

# loc <- c(1789917, 5924157)
# load("C:\\TEMP\\incursion\\data\\inc.nzfmd.RData")
# par <- inc.nzfmd$par[,3:4]
# par$lsu <- round(runif(n = nrow(par), min = 1, max = 1500), digits = 0)
# dim <- 5000
# shape <- "square"
