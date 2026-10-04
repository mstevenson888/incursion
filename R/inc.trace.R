# Requires the following packages to be installed: calendR, ggplot2, lubridate

# 'dow' and 'woy' are data columns referenced inside ggplot2::aes(), not R objects:
utils::globalVariables(c("dow", "woy"))

inc.trace <- function(inc, edate, mlage, det.disin, buffer, plot = TRUE){
  
  # Helper to build a calendar without the ggplot2 deprecation warnings that originate inside calendR:
  make.cal <- function(...){
    suppressWarnings(
      calendR::calendR(from = first.d, to = last.d,
                       weeknames = c("M","T","W","T","F","S","S"),
                       title = ctitle, title.size = 14, ...)
    )
  }
  
  # Minimum incubation period:
  inc.min <- inc[1]
  
  # Mode incubation period:
  inc.mode <- inc[2]
  
  # Max incubation period:
  inc.max <- inc[3]
  
  # Incubation period values used below:
  inc.low <- inc.min
  inc.med <- inc.mode
  inc.upp <- inc.max
  
  # Incubation period:
  inc.l <- "Incubation period (min, max) days:"
  inc.c <- paste(inc.med, " (", inc.min, ", ", inc.max, ")", sep = "")
  
  # Examination 
  exam.l <- "Examination date:"
  exam.c <- as.character(format(edate, format = "%d-%b-%Y"))
  
  # Back trace start:
  bs.d <- as.Date(edate - mlage - inc.upp - buffer, format = "%d-%b-%Y")
  bs.c <- as.character(bs.d)
  bs.l <- "Back trace start:"
  
  # Back trace end:
  be.d <- as.Date(edate - mlage - inc.low, format = "%d-%b-%Y")
  be.c <- as.character(be.d)
  be.l <- "Back trace end:"
  
  # Forward trace start (same as back trace start, by design):
  fs.d <- as.Date(edate - mlage - inc.upp - buffer, format = "%d-%b-%Y")
  fs.c <- as.character(fs.d)
  fs.l <- "Forward trace start:"
  
  # Forward trace end:
  fe.d <- as.Date(edate + det.disin, format = "%d-%b-%Y")
  fe.c <- as.character(fe.d)
  fe.l <- "Forward trace end:"
  
  
  # ----------------------------------------------------------------------------
  # First date:
  
  first.d <- min(c(edate, bs.d, be.d, fs.d, fe.d))
  first.mo <- lubridate::month(first.d)
  first.yr <- lubridate::year(first.d)
  first.d <- as.Date(paste("01-", first.mo, "-", first.yr, sep = ""), format = "%d-%m-%Y")
  
  # Last date:
  last.d <- max(c(edate, bs.d, be.d, fs.d, fe.d))
  last.mo <- lubridate::month(last.d)
  last.yr <- lubridate::year(last.d)
  
  # Function to calculate first day of month:
  fom <- function(x) as.Date(cut(x, "month"))
  last.d <- as.Date(paste(last.yr, last.mo, 1, sep = "-"))
  last.d <- fom(last.d + 32) - 1
  
  # Special dates:
  special.df <- data.frame(edate = seq(from = first.d, to = last.d, by = 1))
  special.df$nday <- 1:nrow(special.df)
  special.df$dow <- factor(weekdays(special.df$edate))
  special.df$dow <- factor(special.df$dow, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  special.df$dow <- as.integer(special.df$dow) - 1
  special.df$woy <- as.numeric(strftime(special.df$edate, format = "%V"))
  
  # Back trace:
  special.df$back[special.df$edate >= bs.d & special.df$edate <= be.d] <- "Back trace"
  
  # Forward trace:
  special.df$frwd[special.df$edate >= fs.d & special.df$edate <= fe.d] <- "Forward trace"
  
  # Examination:
  special.df$exam[special.df$edate == edate] <- "Examination"
  
  back.df <- subset(special.df, subset = !is.na(special.df$back))
  back.df <- back.df[,1:5]
  
  frwd.df <- subset(special.df, subset = !is.na(special.df$frwd))
  frwd.df <- frwd.df[,c(1:4,6)]
  
  exam.df <- subset(special.df, subset = !is.na(special.df$exam))
  exam.df <- exam.df[,c(1:4,7)]
  
  # Create a calendar with a legend:
  ctitle <- paste(format(first.d, format = "%d %b %Y"), " to ", format(last.d, format = "%d %b %Y"), sep = "")
  
  back.cld <- make.cal()
  id <- back.cld$data$date %in% back.df$edate
  back.cld$data <- back.cld$data[id,]
  
  frwd.cld <- make.cal()
  id <- frwd.cld$data$date %in% frwd.df$edate
  frwd.cld$data <- frwd.cld$data[id,]
  
  # The calendar is always built and returned; 'plot' only controls whether it is drawn immediately:
  cal.gg2 <- make.cal(special.days = exam.df$nday, special.col = "red") +
    ggplot2::geom_point(data = back.cld$data, ggplot2::aes(x = dow, y = woy + 0.70), pch = 16, size = 3, colour = "#0072B2") +
    ggplot2::geom_point(data = frwd.cld$data, ggplot2::aes(x = dow, y = woy + 1.30), pch = 16, size = 3, colour = "#882255")
  
  if(plot == TRUE){
    # Explicit print() is needed: ggplot objects created inside a function are not displayed automatically:
    suppressWarnings(print(cal.gg2))
  }
  
  # Return values are a list:
  inc.df <- data.frame(median = inc.med, min = inc.low, max = inc.upp)
  btrace.df <- data.frame(start = bs.d, end = be.d)
  ftrace.df <- data.frame(fstart = fs.d, fend = fe.d)
  
  rval.ls <- list(incubation = inc.df, back.trace = btrace.df, forward.trace = ftrace.df, calendar = cal.gg2)
  return(rval.ls)
}