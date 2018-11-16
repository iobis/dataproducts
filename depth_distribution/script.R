rm(list = ls())
dev.off()

require(shape)

# config

regions <- c(0, 200, 1000, 4000, 6000, 11000)
regionlabels <- head(regions, -1)
categories <- c(seq(0, 200, by = 50), seq(300, 1000, by = 100), seq(1200, 6000, by = 200), seq(7000, 11000, by = 1000))
categorylabels <- head(categories, -1)
finecategories <- c(seq(0, 200, by = 10), seq(300, 1000, by = 100), seq(1200, 6000, by = 200),  seq(7000, 11000, by = 1000))
finecategorylabels <- head(finecategories, -1)

# OBIS data

#source("data.R")
load("data/records_.dat")
load("data/records_fine_.dat")
#load("data/taxa.dat")
#load("data/taxa_fine.dat")

recs.sum50 <- records
recs.sum10 <- records_fine
title <- "number of records"

#recs.sum50 <- taxa
#recs.sum10 <- taxa_fine
#title <- "number of taxa"

# proportional areas of ocean of different depths from data described in Smith & Sandwell 1997, Science 277: 1956-1962, and available to download as an ASCII file from http://ibis.grdl.noaa.gov/cgi-bin/bathy/bathD.pl

glob.depths <- read.table("data/global_map.xyz.sav", sep = "\t")
names(glob.depths) <- c("x", "y", "depth")
glob.depths <- glob.depths[glob.depths$depth <= 0,]
glob.depths$d <- -1 * glob.depths$depth
glob.depths$d.cat <- cut(glob.depths$d, breaks = regions, include.lowest = T, labels = F)

# percentages of ocean at different depths:

p.area <- round(100 * table(glob.depths$d.cat) / nrow(glob.depths), 1)
p.area.cum <- as.vector(c(0, cumsum(p.area)))

# scaled by the relative area of the ocean at different depths

n.per.cat50 <- c(4, 8, 15, 10, 5) #there are 42 cols / rows in recs.sum, which are split between the depth categories as follows
bd.cats50 <- numeric()
for (i in 1:5) {
  bd.cats50 <- c(bd.cats50, seq(p.area.cum[i], p.area.cum[i+1], length.out = n.per.cat50[i] + 1)[1:n.per.cat50[i]])
}
bd.cats50 <- c(bd.cats50, 100)
sd.cats50 <-  sort(-1 * categories)
diffs.sd <- matrix(diff(sd.cats50)) #To standardise the number of records in each 'cell' of the recs.sum50 matrix by the volume of water represented by that cell
diffs.bd <- matrix(diff(bd.cats50), ncol = 42)
area.mat <- diffs.sd %*% diffs.bd #matrix of cell areas
rec.by.area.mat <- recs.sum50 / area.mat #records divided by cell areas, because the 'surface area' axis is already an area, 'area' in this matrix is effectively volume

n.per.cat10 <- c(20, 8, 15, 10, 5)
bd.cats10 <- numeric()
for(i in 1:5){
  bd.cats10 <- c(bd.cats10, seq(p.area.cum[i], p.area.cum[i+1], length.out = n.per.cat10[i] + 1)[1:n.per.cat10[i]])
}
bd.cats10 <- c(bd.cats10, 100)
sd.cats10 <-  sort(-1 * finecategories)
diffs.sd.cs <- matrix(diff(sd.cats10[39:59]))
diffs.bd.cs <- matrix(diff(bd.cats10[1:21]), ncol = 20)
area.mat.cs <- diffs.sd.cs %*% diffs.bd.cs
rec.by.area.cs <- recs.sum10[39:58, 1:20] / area.mat.cs

# set up figure

setup.plot50 <- function(newplot = F, addlines = F, deep.pelagic = F, blue.col = "cornflowerblue", bottom.lwd = 0.5, do.bottom = T, x = bd.cats50, y = sd.cats50, add.axis = T) {
  if (newplot == T) {
    dev.new()
    par(mar = c(1, 7, 2, 2))
  }
  recsum.dummy <- recs.sum50
  recsum.dummy[!is.na(recsum.dummy)] <- 1
  for (i in 9:1) {
    recsum.dummy[,43-i] <- c(rep(NA, i-1), rep(1, 43-i))
  }
  image(x, y, t(recsum.dummy), col = blue.col, yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n")
  if (add.axis == T) {
    axis(side = 3, at = p.area.cum, xpd = T, lwd = bottom.lwd, lwd.ticks = 0, labels = F)
    axis(side = 3, at = p.area.cum[1:5], cex.axis = 1, xpd = T, lwd.ticks = bottom.lwd, lwd = 0, labels = F, tck = -.02)
    d.raw <- seq(-11000, 0, by = 1000) #depths for axis, on simple scale
    axis(side = 2, at = d.raw, labels = -1*d.raw, las = 1, lwd = bottom.lwd) #add depth axis to the left hand side of the plot
    text(-12, -5500, "depth (m)", xpd = T, cex = 1.5, srt = 90)
    
    #text.pos <- p.area.cum[1:5] + diff(p.area.cum)/2 #finally, add labels to indicate the different depth zones
    #text.pos[5] <- 101
    text(103, c(-350, -850, -2750, -5250, -8750), c("A","B","C","D","E"), pos = 3, xpd = T)
    
  }
}

figure2 <- function(rec.dat, cs.rec.dat, add.cont.shelf = T, min.rec = NULL, max.rec = NULL, v.scale = F, newplot = T, title = "number of records") {
    cs.rec.dat <- cs.rec.dat[39:58, 1:20]
    vol.scale <- function() {	
    #add an approximate volume scale onto the plot, based a global ocean surface area of 361M km^3
    #this gives the volume of one of the 'cells' in the deep sea, which is 200m in depth and 4.86% of the global
    #ocean area. So the area is 0.0486*(361 * 10^6)km^2, and to get volume multiply by 0.2km, giving 3508920km^3
    x.dim <- diff(bd.cats50)[cumsum(n.per.cat50)[4]]
    polygon(
      x = c(22, 22+x.dim, 22+x.dim, 22, 22),
      y = c(-3600, -3600, -3800, -3800, -3600),
      col = "cornflowerblue"
    )
    text(22 + 0.5*x.dim, -3600, expression(paste("c.", 3.5, " x ", 10^6, "k", m^3)), pos = 3, cex = 0.8)
  }
  
  cont.slope.fig <- function(rec.dat.cs = rec.by.area.cs, pal = NULL, min.rec = NULL, max.rec = NULL){
    #Add the inset figure of the continental shelf
    par(fig = c(0.2,0.5,0.05,0.35), new = T)
    #Fill with blue
    recsum.dummy <- rec.dat.cs
    recsum.dummy[!is.na(recsum.dummy)] <- 1
    #fill in
    image(bd.cats10[1:21], sd.cats10[39:59], t(recsum.dummy), col = "cornflowerblue", yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n", cex.main = 1, bg = "white")
    #add the records

    if(is.null(max.rec)){
      min.rec = min(log10(rec.dat.cs), na.rm = T)
      max.rec = max(log10(rec.dat.cs), na.rm = T)
    }
    
    message(paste0("slope figure min.rec: ", min.rec))
    message(paste0("slope figure max.rec: ", max.rec))
    message(paste0("slope figure sum: ", sum(rec.dat.cs, na.rm = TRUE)))
    
    image(bd.cats10[1:21], sd.cats10[39:59], t(log10(rec.dat.cs)), col = pal, ylab = "", xlab = "", bty = "n", add = T, yaxt = "n", xaxt = "n", zlim = c(min.rec, max.rec))
    #depths for axis, on simple scale
    d.raw <- seq(-1000,0, by = 100)
    #add axis
    axis(side = 4, at = d.raw, labels = F, las = 1, lwd = 0.5)
    axis(side = 4, at = d.raw[seq(1, 11, by = 2)], labels = -1*d.raw[seq(1, 11, by = 2)], tck = F, las = 1, lwd = 0.5)
    #this adds the bottom profile to the figure
    x <- rep(bd.cats10[1:21], each = 2)[-1]
    x <- x[-length(x)]
    y <- rep(sort(sd.cats10[39:59], decreasing = T)[-1], each = 2)
    polygon(c(x, 11.9,-1,-1), c(y, -1000, -1000, -10), lwd = 0.5, col = "white")
  }
  #create the colour palette, and set up the background
  pal <- intpalette(c("cornflowerblue", "lightblue1", "yellow", "orangered", "red"), numcol = 46)
  #create background plot
  setup.plot50(newplot = newplot, blue.col = "cornflowerblue", bottom.lwd = 0.75, do.bottom = F)
  
  #fill in with log(record numbers); the maximum number of records is derived from the data, unless specified in the 'max.rec' argument.
  #The purpose of specifying is to maintain the same colour scale on the main plot and the continental shelf inset, if this is to be produced.
  #NB - if max.rec IS specified, you MUST also specify min.rec.
  if (is.null(max.rec)) {
    image(bd.cats50, sd.cats50, t(log10(rec.dat)), col = pal, ylab = "", xlab = "", bty = "n", add = T)
  } else {
    image(bd.cats50, sd.cats50, t(log10(rec.dat)), col = pal, ylab = "", xlab = "", bty = "n", add = T, zlim = c(min.rec, max.rec))
  }
  
  #add the sea bottom profile
  x <- rep(bd.cats50, each = 2)[-1]
  x <- x[-length(x)]
  y <- rep(sort(sd.cats50, decreasing = T)[-1], each = 2)
  polygon(c(x, 100,0,0), c(y, -11000, -11000, -50), lwd = 0.75)
  
  abline(v = p.area.cum[1:5], lwd = 0.5, xpd = F)
  segments(100.05, 100, 100.05, -11000, lwd = 0.5, xpd = T)
  abline(h = c(-200, -1000, -4000, -6000), lwd = 0.5, xpd = F)
  
  if (v.scale == T){ vol.scale() }

  text(77, -6900, title, cex = 1.2)
  
  if (add.cont.shelf == T) {
    polygon(c(5,45,45,5,5), c(-6500,-6500,-10800,-10800,-6500), col = "white", border = NA)
    text(25.05, -6900, "continental shelf", cex = 1.2)
  }
  
  par(fig = c(0.76,0.83,0.05,0.35), new = T)      
  par(mar = c(0.5,0.5,1,0.5))

  #and draw the scale as a new image plot - set the scale using the specified max.rec, or a sensible value from the data
  if (is.null(max.rec)) {
    max.rec <- round(max(log10(rec.dat), na.rm = T), 2) + 0.01
  }
  
  ColorLevels <- seq(from = -1 * max.rec, to = 0, length = length(pal))
  ColorRamp <- pal
  image(1, ColorLevels, matrix(data = ColorLevels, ncol = length(ColorLevels), nrow = 1), col = ColorRamp, xlab = "", ylab = "", xaxt = "n", las = 1, yaxt = "n")
  
  #add an axis with log10 scale
  max.rec.int <- round(max.rec) # get an integer value for maximum
  #Then add the axis.
  #First, create pretty axis labels (a sequence of 10s with superscripts from 0 to max.rec.int)
  ax.lab <- rep(expression(10^0), (max.rec.int + 1))
  for (i in 2:(max.rec.int + 1)){ ax.lab[[i]][3] <- i - 1 }
  #now add the axis
  axis(side = 2, at = -1*max.rec.int:0, labels = ax.lab, las = 1)
  
  if (add.cont.shelf == T) {
    #min.rec <- min(log10(rec.dat), na.rm = T)
    #max.rec <- max(log10(cs.rec.dat), na.rm = T)
    cont.slope.fig(rec.dat.cs = cs.rec.dat, pal = pal, min.rec = min.rec, max.rec = max.rec)
  }
  
}

#figure2(rec.dat = rec.by.area.mat, cs.rec.dat = rec.by.area.cs, v.scale = T)
#This is the figure without standardising counts at all
max.rec = 6.5
recs.sum50[recs.sum50 > 10^max.rec] <- 10^max.rec
recs.sum10[recs.sum10 > 10^max.rec] <- 10^max.rec

figure2(recs.sum50, recs.sum10, v.scale = T, title = title, min.rec = 0, max.rec = 6.5)
quartz.save("output.png", width = 10, height = 7, dpi = 300, bg = "white")

#run the above code with use.old.data = T and use.old.data = F
#Run using min and max records from the data:
#for 2013 data, unscaled, this gives max = 5442286, min = 1; scaled gives min = 1.028807e-03, max = 2.551828e+05
#for 2009 data, unscaled max = 1516689, min = 1; scaled gives min = 1.028807e-03, max = 9.425609e+04
#for each year, files ending nonstand1 use the year's own data, nonstand2 use the range across both years (effectively 2013 range), likewise for stand1 and stand2

#figure2(rec.dat = recs.sum50, cs.rec.dat = recs.sum10[31:58, 1:28], v.scale = T, newplot = T,
#        max.rec = log10(max(c(recs.sum50, recs.sum10[39:58, 1:20]), na.rm = T)),
#        min.rec = log10(min(c(recs.sum50, recs.sum10[39:58, 1:20]), na.rm = T)))
#and using min/max from both years of data
#figure2(rec.dat = recs.sum50, cs.rec.dat = recs.sum10[39:58, 1:20], v.scale = T, newplot = T,
#        max.rec = log10(5442286), min.rec = log10(1))

#and standardised to volume from the data
#figure2(rec.dat = rec.by.area.mat, cs.rec.dat = rec.by.area.cs, v.scale = T, newplot = T,
#        max.rec = log10(max(c(rec.by.area.mat, rec.by.area.cs), na.rm = T)),
#        min.rec = log10(min(c(rec.by.area.mat, rec.by.area.cs), na.rm = T)))
#and across both years of data
#figure2(rec.dat = rec.by.area.mat, cs.rec.dat = rec.by.area.cs, v.scale = T, newplot = T,
#        max.rec = log10(2.551828e+05),
#        min.rec = log10(1.028807e-03))

#####################################################################
#END
#####################################################################